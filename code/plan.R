############################# Config values ###################################
###############################################################################

# Define the directory where the raw_data is.
raw_data_dir <- here("raw_data")

# Define the configuration of variables from pisa2015
# which contains the name of the weights, the name
# of the replication, weights, the name of the plausible
# values, etc... I don't define this for other rounds
# because they come integrated with the `intsvy` package.
pisa2015_conf <- list(
  variables = list(pvlabelpref = "PV",
                   pvlabelsuff = "READ",
                   weightFinal = "W_FSTUWT",
                   weightBRR = "W_FSTURWT"),
  parameters = list(cutoffs = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30),
                    percentiles = c(5, 10, 25, 75, 90, 95),
                    PVreps = 10,
                    BRRreps = 80,
                    weights = "BRR",
                    replication_scheme = 'pisa')
)

# Final countries used in the analysis (30)
# which have at least 50% of all years available (6 years)
final_countries <- c("Finland",
                     "Austria",
                     "Australia",
                     "Sweden",
                     "Slovakia",
                     "Czech Republic",
                     "Canada",
                     "Hungary",
                     "Iceland",
                     "Netherlands",
                     "Ireland",
                     "Spain",
                     "Belgium",
                     "Italy",
                     "Chile",
                     "Norway",
                     "United Kingdom",
                     "Latvia",
                     "Luxembourg",
                     "Switzerland",
                     "Greece",
                     "Denmark",
                     "Portugal",
                     "Bulgaria",
                     "Slovenia",
                     "Japan",
                     "Israel",
                     "Poland",
                     "United States",
                     "Germany",
                     "Turkey"
                     )


# Selected countries for plotting
countries <- c("Finland",
               # "France",
               # "New Zealand",
               "Austria",
               "Australia",
               "Sweden",
               # "Czech Republic",
               "Canada",
               "Hungary",
               # "Iceland",
               "Netherlands",
               "Spain",
               # "Belgium",
               "Italy",
               # "Norway",
               "United Kingdom",
               # "Greece",
               "Denmark",
               # "Israel",
               "Poland",
               "United States",
               "Germany"
               # "Turkey",
               # "Russia"
               )

# Reliability of test scores per year
reliability_pisa <-
  c("2000" = 0.81,
    "2003" = 0.85,
    "2006" = 0.78,
    "2009" = 0.74,
    "2012" = 0.82,
    "2015" = 0.74) # 2015 imputed


# Create recoding vector for car::recode in the below map call.
# This is used for normalizing the country names in all waves.

recode_cntrys <-
  # pisa_countrynames come from personal cimentadaj package
  enframe(cimentadaj::pisa_countrynames) %>% 
  mutate(recoder = paste0("'", name, "'", " = ", "'", value, "'")) %>%
  pull(recoder) %>%
  paste0(collapse = ";")


############################# Helper functions ################################
###############################################################################


# Function calculates the bottom 30th quantile for the bottom educated and the 70th quantile
# for the top educated. If the quantiles cannot be estimated, it returns two NA's instead
quantile_missing <- function(df, weights, probs, ch_var = "escs_trend") {

  quan <- try(Hmisc::wtd.quantile(
    df[[ch_var]],
    weights = df[[weights]],
    probs = probs
  ))

  if (any("try-error" %in% class(quan))) {
    return(rep(NA, times = length(probs)))
  } else {
    return(quan)
  }
}

# Producing the plot to get the difference between the top 30% of the high educated
# vs the bottom 30% of the low educated. This function loops through each dataset/country
# and survey reliability and estimates the difference while also extracting the s.e. of each
# difference.

# .x <- adapted_year_data[[1]]
# .y <- reliability_pisa[1]
# test <- "MATH"
# probs <- c(0.1, 0.9)

# It returns a dataframe for each survey with all countries and respective coefficients and
# standard errors.
calculate_gap <- function(data_modelling, test) {
  separate_waves <- split(data_modelling, data_modelling$wave)

  separate_gaps <- map(separate_waves, function(.x) {
    weights_var <- "stu_weight"

    mod_formula <-
      as.formula(
        paste0(
          test, " ~ escs_dummy + (1 + escs_dummy | country)"
        )
      )

    mod2 <-
      lmer(mod_formula,
           data = .x,
           weights = .x[[weights_var]],
           control = lmerControl(optimizer = "Nelder_Mead"))

    # Take the country coefficients (absolute coefficients)
    country_coef <-
      coef(mod2)$country %>%
               rownames_to_column() %>%
               gather(escs_dummy, Mean, -rowname) %>%
               mutate(escs_dummy = dplyr::recode(escs_dummy,
                                                 `(Intercept)` = "0",
                                                 `escs_dummy` = "1"))

    # Take the absolute country standard errors
    se <-
      se.coef(mod2)$country %>%
                  as.data.frame() %>%
                  rownames_to_column() %>%
                  gather(escs_dummy, s.e., -rowname) %>%
                  mutate(escs_dummy = dplyr::recode(escs_dummy,
                                                    `(Intercept)` = "0",
                                                    `escs_dummy` = "1"))

    results <-
      inner_join(country_coef, se, by = c("rowname", "escs_dummy")) %>%
      rename(country = rowname) %>%
      arrange(country, escs_dummy)

    message(paste0(unique(.x$wave), " modeling done"))
    list(results, mod2)
  })

  separate_gaps
}


# Adapted from: https://github.com/jtleek/slipper/blob/master/R/slipper.R
# Returns a tibble with the actual expr + the bootstrapped expr.
bootstrapper <- function(df, expr, B = 100, n = nrow(df), replacement = TRUE) {
  bootstrapper_(df, lazyeval::lazy(expr), B, n, replacement)
}

bootstrapper_ <- function(df, expr, B = 500, n = nrow(df), replacement = TRUE) {
  obs_val = lazyeval::lazy_eval(expr, data = df)
  boot_val = replicate(B, {
    newdata = sample_n(df, n, replace = replacement)
    lazyeval::lazy_eval(expr, data = newdata)
  })
  out = tibble(type = c("observed", "bootstrap"),
               value = c(obs_val, mean(boot_val, na.rm = T)))
  return(out)
}

# For example
# bootstrapper(mtcars, mean(mpg), B = 200)

f_ind <- function(.x) .x[[1]]

enframer <- function(df, col_name = "name") {
  df %>%
    enframe(name = col_name) %>%
    unnest()
}

print_table <- function(x, ...) {
  print.xtable(
    x = x,
    table.placement = "H",
    size = "small",
    hline.after = c(-1, -1, 0, nrow(x), nrow(x)),
    ...
  )
}


formula_sequence <- function(dv, ivs, random_effect) {
  formula_seq <-
    lapply(1:length(ivs), function(x) {
      paste0(dv ," ~ ", paste0(ivs[1:x], collapse = " + ") %>% paste0(" + ", random_effect))
    }) %>%
    lapply(as.formula)

  formula_seq
}

# Accepts a list of formulas, the data for the formulas
# and a set of priors following the prior structure of the brms
# package. It will return a list with the priors applied
# sequentially to all formulas, ignoring coefs
# which are not specified with the default brms prior
# but changing the prior of the variables that are specified.

# this includes speciftying only 'b'
list_priors <- function(all_formulas, data_formula, selected_priors) {
  all_priors <- map(all_formulas, get_prior, data_formula)

  selected_priors <- data.frame(selected_priors)

  # If there are priors only for betas, and not coefficients
  # then replace only those because this means that they are the default
  # prior for all coefficients without priors
  prior_betas <- which(selected_priors$class == "b" & selected_priors$coef == "")

  all_priors <-
    map(all_priors, ~ {
      # If there are priors only for 'b' in the supplied prior
      if (!is_empty(prior_betas)) {
        # Then locate that 'b' in the prior from the formula from 'all_priors'
        sub_prior_betas <- which(.x$class == "b" & .x$coef == "")
        if (!is_empty(sub_prior_betas)) {
          # And replace the 'b' from all_priors with the b from the supplied prior
          .x[sub_prior_betas, ] <- selected_priors[prior_betas, ]
        }
      }
      .x
    })

  prior_coefs <- which(selected_priors$class == "b" & selected_priors$coef != "")

  # For all priors of the coefficients, replace the ones with the default
  # prior with the supplied prior, if there is at least one
  final_priors <-
    map(all_priors, ~ {
      if (!is_empty(prior_coefs)) {

        # Where is the coefficient match in the priors from the formula?
        pos_replace_one <- which(.x$coef %in% selected_priors$coef[prior_coefs])

        # Where is the coefficient match in the supplied prior?
        pos_replace_two <- which(selected_priors$coef[prior_coefs] %in% .x$coef)

        # Replace the default prior with the supplied prior
        .x[pos_replace_one, ] <- selected_priors[prior_coefs, ][pos_replace_two, ]
      }
      .x
    })

  final_priors
}

stan_model_builder <- function(dv, iv, random, data, prior = NULL, max_treed = 10) {
  all_formulas <- formula_sequence(dv, iv, random)

  if (!is.null(prior)) {
    final_prior <- list_priors(all_formulas, data, prior)

    print("Memory used:")
    print(pryr::mem_used())

    mod_tracking <-
      map2(all_formulas, final_prior, ~ {
        brms::brm(
          .x,
          family = gaussian(),
          data = data,
          warmup = 1000, iter = 2000, chains = 5,
          prior = .y,
          control = list(max_treedepth = max_treed)
        )
      })
  } else {
    mod_tracking <-
      map(all_formulas, ~ {
        brms::brm(
          .x,
          family = gaussian(),
          data = data,
          warmup = 1000, iter = 2000, chains = 5,
          control = list(max_treedepth = max_treed)
        )
      })
  }
}

stan_extractor <- function(models) {
  coef_list <-
    map(models, ~ {
      broom::tidy(.x) %>%
        .[grepl("^b_", .$term), ] %>%
        map_if(is_double, round, 2) %>%
        as_tibble %>%
        transmute(term,
                  estimate = paste0(estimate, " (", lower, ", ", upper, ")"))
    })

  coef_list
}

stan_table_builder <- function(models_extracted) {
  intermediate_table <-
    reduce(models_extracted, full_join, by = "term") %>%
    setNames(c(" ", paste("Model", seq_len(models_extracted %>% length))))

  row_order <-
    setdiff(seq_len(nrow(intermediate_table)),
            grep("Intercept", intermediate_table$` `)) %>%
    `c`(grep("Intercept", intermediate_table$` `))

  intermediate_table[row_order, ]
}

stan_table <- function(models) {

  table_ready <-
    models %>%
    stan_extractor() %>%
    stan_table_builder()

  table_ready
}

btw_group_var <- function(brms_model) {
  unname(attr(unclass(sjstats::icc(brms_model)), "tau.00"))
}

table_details <- function(models, multilevel = TRUE) {

  random_effect <- nrow(models[[length(models)]]$ranef) > 0
  where_to_place <- models[[length(models)]] %>% fixef() %>% nrow()
  
  n_obs <-
    paste0(paste0(" Sample size: & ",
                  paste0(map(models, nobs), collapse = " & ")), " \\\\")

  if (multilevel) {
    r_square <-
      paste0("\\hline ",
             paste0("Between-group variance: & ",
                    paste0(map(models, ~ btw_group_var(.x)[1] %>% round(2)),
                           collapse = " & ")), " \\\\")
    n_groups <-
      paste0(paste0(" Number of groups: & ",
                    paste0(map(models, ~ unlist(brms::ngrps(.x))), collapse = " & ")), " \\\\")

    command_to_row <- paste0(r_square, n_obs, n_groups)
    addtorow_two <- list(pos = list(pos = where_to_place - 1), command = command_to_row)

    addtorow_two

    return(addtorow_two)
  }

  r_square <-
    paste0("\\hline ",
           paste0("R-squared: & ",
                  paste0(map(models, ~ bayes_R2(.x)[1] %>% round(2) * 100) %>% paste0("\\%"),
                         collapse = " & ")), " \\\\")

  command_to_row <- paste0(r_square, n_obs)
  addtorow_two <- list(pos = list(pos = where_to_place), command = command_to_row)

  addtorow_two
}

# 2000
# SC03Q01 School public/private
# 1 Public
# 2 Private
# 7 N/A
# 8 M/R
# 9 Mis

# 2003
# SC03Q01 (8) Public or private
# 1 Public
# 2 Private
# 7 N/A
# 8 Invalid
# 9 Miss

# 2006
# SC02Q01 (8) Public or private
#  1 Public
#  2 Private
#  7 N/A
#  8 Invalid
#  9 Missing

# 2009
# SC02Q01
# 1 public
# 2 private

# 2012
# SC01Q01 Public or private Num
# 1 Public
# 2 Private
# 7 N/A
# 8 Invalid
# 9 Missing

# 2015
# SC013Q01TA
# 1 Public
# 2 Private
# 7 N/A
# 8 Invalid
# 9 Missing

# central_examination <-
#   tribble(
#   ~ country, ~central_examination,
#   "Australia", 1,
#   "Austria", 0,
#   "Belgium", 0,
#   "Bulgaria", 1,
#   "Canada", rbinom(1, 1, prob = 0.51),
#   "Czech Republic", 1,
#   "Denmark", 1,
#   "Finland", 1,
#   "France", 1,
#   "Germany", 0,
#   "United Kingdom", 1,
#   "Greece", 0,
#   "Hong Kong", 1,
#   "Hungary", 1,
#   "Iceland", 1,
#   "Ireland", 1,
#   "Israel", 1,
#   "Italy", 1,
#   "Japan", 1,
#   "Korea", 1,
#   "Latvia", 1,
#   "Liechtenstein", 1,
#   "Luxembourg", 1,
#   "Netherlands", 1,
#   "New Zealand", 1,
#   "Norway", 1,
#   "Poland", 1,
#   "Portugal", 0,
#   "Russia", 1,
#   "Slovakia", 1,
#   "Slovenia", 1,
#   "Spain", 0,
#   "Sweden", 0,
#   "Switzerland", 0,
#   "Turkey", 1,
#   "United States", 1
#   )


############################# Read raw data ###################################
###############################################################################
## PISA data is too big to fit within drake (due to drake saving it as cache).
## For that reason, I run the function below only once to save the data as a fst
## data frame and then read it and select only the relevant columns. This makes
## it much faster to easily pick new variables instead of rereading all rounds
## and pick new variables.

read_pisa <- function(raw_data) {

  # Read PISA 2012 files
  pisa2012_path <- file.path(raw_data, "pisa2012", "INT_STU12_DEC03.txt")
  dic_path <- file.path(raw_data, "pisa2012", "PISA2012_SAS_student.sas")
  dic_student <- parse.SAScii(sas_ri = dic_path)
  student2012 <- read_fwf(pisa2012_path,
                          col_positions = fwf_widths(dic_student$width),
                          progress = TRUE)

  colnames(student2012) <- dic_student$varname

  print("pisa2012 read")

  # Read PISA 2015 data
  pisa2015_path <- file.path(raw_data, "pisa2015", "CY6_MS_CMB_STU_QQQ.sav")
  pisa_countrynames <- c(pisa_countrynames, "United States" = "USA")
  student2015 <- read_spss(pisa2015_path)

  print("pisa2015 read")

  # Provisional code for separate PISA2012
  # Vector with database names except student2015
  databases <- c("math2000", paste0("student", seq(2003, 2009, 3)))

  # Create a list with all data sets
  pisa_list <-
    list(
      student2000 = math2000,
      student2003 = student2003,
      student2006 = student2006,
      student2009 = student2009,
      student2012 = student2012,
      student2015 = student2015
    )

  # Give each dataset their own name
  names(pisa_list) <- c("student2000",
                        databases[-1],
                        "student2012",
                        "student2015")

  # Fix the country variable name which is different for PISA 2000
  pisa_list$math2000$CNT <- pisa_list$math2000$COUNTRY

  print("pisa_list ready")

  # Remove everything that uses memory
  rm(dic_student,
     student2012,
     student2015)

  unloadNamespace("PISA2000lite")
  unloadNamespace("PISA2003lite")
  unloadNamespace("PISA2006lite")
  unloadNamespace("PISA2009lite")
  print("PISA packages unloaded")

  print("Memory used:")
  print(mem_used())
  
  pisa_list
}

delete_raw_data <- function() {
  rm(raw_student, envir = globalenv())
  TRUE
}

print_memory <- function() {
  print(paste0("Currently consumed memory: ", capture.output(pryr::mem_used())))
}

center <- function(x) x - mean(x, na.rm = TRUE)

formula_gen <- function(model_formula) {
  dv <- find_response(model_formula)
  covariates <- str_subset(find_terms(model_formula)$conditional,
                           "\\(", negate = TRUE)
  random_effects <- str_subset(find_terms(model_formula)$conditional,
                               "\\(")

  dv <- paste(dv, "~ 1")
  combinations <- lapply(1:length(covariates), function(i) seq(1:i))
  formulas <- map(combinations, ~ {
    x <-
      as.formula(
        paste(c(dv, covariates[.x], random_effects), collapse = " + "),
        )
    x
  })

  first_formula <- paste0(dv, " + ", paste0(random_effects, collapse = "+"))
  formulas <- c(as.formula(first_formula), formulas)
  formulas
}

# Calculate the difference in significance between two coefficients
# and their associated standard error. Estimated from
# https://www.tandfonline.com/doi/abs/10.1198/000313006X152649#.VEn854_sM7w
sig_vs_sig <- function(coef1, se1, coef2, se2) {
  coef_diff <- coef1 - coef2
  se_diff <- sqrt(se1 ^ 2 + se2 ^ 2)
  round(c(coef_diff, se_diff), 2)
}

gaps <- c("90th/10th SES gap", "80th/20th SES gap", "70th/30th SES gap")

############################# Drake plan ######################################
###############################################################################

# These are all autonomy measures used in the analysis
autonomy_measures <- c("academic_content_aut", "personnel_aut", "budget_aut")

plan <-
  drake_plan(
    ############################# Loading data #################################
    ############################################################################
    # The really big list with all raw pisa dataset is in /code/read_raw_data.R
    # which is read in _drake.R because it's too heavy to have within the plan.
    # See the notes in /code/read_raw_data.R. It's called raw_data.

    # All read_* functions return a list with the data for each wave
    loaded_school = target(
      load_school(raw_data_dir),
      format = "fst"
     ),
    # This is the socio-economic index harmonized to be used across all waves
    loaded_escs = target(
      load_escs(raw_data_dir, recode_cntrys),
      format = "fst"      
     ),

    ############################# Harmonize data ###############################
    ############################################################################

    harmonized_student = target(
      harmonize_student(
        raw_student,
        recode_cntrys
      ),
      format = "fst"      
    ),
    rm_raw = delete_raw_data(), # Delete the student raw data after harmonized
    # Check how we're doing with memory
    test = target(print_memory(), trigger = trigger(change = sample(1000))),
    harmonized_school = target(
      harmonize_school(loaded_school),
      format = "fst"
    ),

    ############################# Merge data ###################################
    ############################################################################

    # Merge the student data with the harmonized loaded_escs (NOT the school data)
    merged_student_escs = target(
      merge_student_escs(harmonized_student, loaded_escs),
      format = "fst"      
    ),

    # This is the step when the tibble with a list column becomes an entire
    # data frame with over 1M students for all waves with the student and
    # school data.
    merged_student_school = target(
      merge_student_school(merged_student_escs,
                           harmonized_school),
      format = "fst"
    ),

    ############################# Wrangling for modelling ######################
    ############################################################################

    ## escs_dummy_data now contains the dummy column for the 90th/10th
    ## and the quantile_cuts with 10% quantile groups for the escs variable
    ## This function also adjusts and standardizes the test scores    
    ## It also only keep countries with more than 50% of data for all years
    created_escs_dummy = target(
      create_escs_dummy(merged_student_school,
                        c(0.1, 0.9),
                        reliability_pisa),
      format = "fst"
    ),

    ## Keeps countries with more than 50% of data for all years
    data_modelling = target(
      filter(created_escs_dummy,
             country %in% final_countries),
      format = "fst"
    ),

    # Create another version of the data with imputed missing values
    imputed_data_modelling = target(
      impute_missing(data_modelling),
      format = "fst"
    ),

    #### Create all data to visualize the achievement gap ####
    res_math = target(
      calculate_gap(data_modelling, "math"),
      # Because it's a list
      format = "rds"
    ),

    res_read = target(
      calculate_gap(data_modelling, "read"),
      # Because it's a list
      format = "rds"
    ),
    results_math = map(res_math, f_ind),
    results_read = map(res_read, f_ind),
    complete_data_topbottom = pisa_preparer(results_math,
                                            results_read,
                                            type_txt = "90th/10th SES gap"),

    ############################# Descriptives #################################
    ############################################################################

    plotted_academic_aut = plot_autonomy_public(harmonized_school,
                                                countries,
                                                academic_content_aut),
    plotted_personnel_aut = plot_autonomy_public(harmonized_school,
                                                 countries,
                                                 personnel_aut),
    plotted_budget_aut = plot_autonomy_public(harmonized_school,
                                              countries,
                                              budget_aut),

    calculated_corr_aut = calculate_corr_aut(harmonized_school),
    # Calculate the correlation between autonomy measures in their change
    # from 2000 to 2015
    calculated_corr_aut_change = calculate_corr_aut_change(harmonized_school),

    p1_evolution_gaps = plot_evolution_gaps(complete_data_topbottom),
    top_bottom_perc = perc_increase_fun(complete_data_topbottom),
    p2_perc_change = perc_graph(
      top_bottom_perc,
      "math",
      "90/10 achievement gap",
      "Percentage change from 2000 to 2015",
      countries
    ),

    ############################# Modelling ####################################
    ############################################################################

    ## # Run all combinations of tests/90th-10th/autonomy measure models
    ## # with the unimputed dataset. These will all be named like
    ## # aut_math_0_academic_content_aut, aut_read_0_academic_content_aut,
    ## # aut_math_1_academic_content_aut, ...
    aut = target(
      generate_models(data_modelling,
                      dv = math_read,
                      group = group_vals,
                      aut_var = aut_val
                      ),
      transform = cross(math_read = c("math", "read"),
                        group_vals = c(0, 1),
                        aut_val = !!autonomy_measures)
    ),
    # Run all combinations of tests/90th-10th/autonomy measure models
    # with the imputed dataset. These will all be named lik
    # impute_aut_math_0_academic_content_aut, impute_aut_read_0_academic_content_aut,
    # impute_aut_math_1_academic_content_aut, ...
    impute_aut = target(
      generate_models(imputed_data_modelling,
                      dv = math_read,
                      group = group_vals,
                      aut_var = aut_val
                      ),
      transform = cross(math_read = c("math", "read"),
                        group_vals = c(0, 1),
                        aut_val = !!autonomy_measures)
    ),

    ############################# Robustness ###################################
    ############################################################################
    # Re run the above models but for all countries (not only the chosen)
    # 30
    aut_allcnt = target(
      generate_models(created_escs_dummy,
                      dv = math_read,
                      group = group_vals,
                      aut_var = aut_val
                      ),
      transform = cross(math_read = c("math", "read"),
                        group_vals = c(0, 1),
                        aut_val = !!autonomy_measures)
    ),

    # Rerun using the school 90/10 dummy
    aut_schools = target(
      generate_models_schools(data_modelling,
                              dv = math_read,
                              group = group_vals,
                              aut_var = aut_val
                              ),
      transform = cross(math_read = c("math", "read"),
                        group_vals = c(0, 1),
                        aut_val = !!autonomy_measures)
    ),

    # Run the same models but add an interaction between the full
    # quantiles (instead of only 90th/10th, the Low, Mid, High)
    # to see whether the slope changes from negative to positive
    aut_interact = target(
      generate_models_interaction(data_modelling,
                                  dv = math_read,
                                  aut_var = aut_val,
                                  interact = type_interact
                                  ),
      transform = cross(math_read = c("math", "read"),
                        aut_val = !!autonomy_measures,
                        type_interact = c("quantiles_escs", "quantiles_escs_chr"))
    ),

    # Run the same models but with the three autonomy
    # measures pooled.
    aut_allpooled = target(
      generate_models_allaut(data_modelling,
                             dv = math_read,
                             group = group_vals
                             ),
      transform = cross(math_read = c("math", "read"),
                        group_vals = c(0, 1))
    )

    ##   sample_tables_topbottom = sample_size_calc(
    ##     merged_data,
    ##     c(.1, .9),
    ##     selected = TRUE,
    ##     countries
    ##   ),
    ##   descriptives_samplesize = sample_size_descriptives(
    ##     results_math,
    ##     sample_tables_topbottom,
    ##     complete_data_topbottom
    ##   ),
    ##   descriptives_tracking = tracking_descriptives(tracking_data, countries),
    ##   p3_evolution_ses = evolution_ses_groups(complete_data_topbottom, countries),
    ##   avg_sd_increase_high = avg_increase_fun(complete_data_topbottom, 1),
    ##   avg_sd_increase_low = avg_increase_fun(complete_data_topbottom, 0),
    ##   p4_rate_change = rate_change_graph(avg_sd_increase_high,
    ##                                      avg_sd_increase_low),
    ##   results_math_80 = test_diff(merged_data, reliability_pisa, "MATH",
    ##                               c(0.8, 0.2)) %>% map(f_ind),
    ##   results_read_80 = test_diff(merged_data, reliability_pisa, "READ",
    ##                               c(0.8, 0.2)) %>% map(f_ind),
    ##   results_math_70 = test_diff(merged_data, reliability_pisa, "MATH",
    ##                               c(0.7, 0.3)) %>% map(f_ind),
    ##   results_read_70 = test_diff(merged_data, reliability_pisa, "READ",
    ##                               c(0.7, 0.3)) %>% map(f_ind),
    ##   complete_data_topbottom_80 = pisa_preparer(results_math_80,
    ##                                              results_read_80,
    ##                                              type_txt = "80th/20th SES gap"),
    ##   complete_data_topbottom_70 = pisa_preparer(results_math_70,
    ##                                              results_read_70,
    ##                                              type_txt = "70th/30th SES gap"),
    ##   complete_gaps = bind_rows(complete_data_topbottom,
    ##                             complete_data_topbottom_80,
    ##                             complete_data_topbottom_70),
    ##   base_ready_data =
    ##     dif_data(complete_gaps, tracking_data) %>%
    ##     mutate(age_selection = ifelse(selage >= 15, 1, 0) %>% as.factor()),
    ##   ready_data_age =
    ##     base_ready_data %>%
    ##     filter(!is.na(num_tracks),
    ##            !is.na(age_selection),
    ##            !is.na(length),
    ##            !is.na(zvoc)
    ##            ),
    ##   mod1_complete_tracking = complete_tracking_model(ready_data_age),
    ##   mod1_table = 
    ##     stan_table(mod1_complete_tracking) %>%
    ##     mutate(" " = c(
    ##       "Only 1 track",
    ##       "Age selection >= 15",
    ##       "% of curric tracked",
    ##       "Vocational Index",
    ##       paste0("Year ", seq(2003, 2015, 3)),
    ##       "Intercept"
    ##     )),
    ##   ready_data =
    ##     base_ready_data %>% 
    ##     filter(!is.na(ztrack), !is.na(zvoc)),
    ##   mod2_tracking = interaction_tracking_model(ready_data),
    ##   mod2_table =
    ##     stan_table(mod2_tracking) %>%
    ##     mutate(" " = c(
    ##       "Tracking Index",
    ##       "Vocational Index",
    ##       "Tracking * Vocational Index",
    ##       paste0("Year ", seq(2003, 2015, 3)),
    ##       "Intercept"
    ##     )),
    ##   p5_interaction_plot = interaction_plot(mod2_tracking),
    ##   mod3_cumulative = mod3_cumulative_change(complete_gaps,
    ##                                            tracking_data,
    ##                                            gaps),
    ##   mod3_table = 
    ##     stan_table(mod3_cumulative) %>%
    ##     mutate(" " = c(
    ##       "Track Index",
    ##       "Vocational Index",
    ##       "Intercept"
    ##     )) %>%
    ##     setNames(c(" ", gsub(" SES gap", "", gaps)))
    )

## loadd(starts_with("aut_schools_"))
## all_mods <- ls(pattern = "aut_schools_")

## academic_content_mods <- all_mods[grepl("academic_", all_mods)]
## personnel_aut_mods <- all_mods[grepl("personnel_", all_mods)]
## budget_aut_mods <- all_mods[grepl("budget_", all_mods)]

## stargazer(
##   unlist(lapply(academic_content_mods, get)),
##   type = "text",
##   digits = 2
## )

## stargazer(
##   unlist(lapply(personnel_aut_mods, get)),
##   type = "text"
## )

## stargazer(
##   unlist(lapply(budget_aut_mods, get)),
##   type = "text"
## )


############################# Academic content ################################
###############################################################################

## stargazer(readd(aut_math_0_academic_content_aut), type = "text")
## stargazer(readd(aut_math_1_academic_content_aut), type = "text")

# Difference between 1-0 coefficients in math is significantly
# different from 0
## sig_vs_sig(0.02, 0.01, -0.03, 0.01)


## stargazer(readd(aut_read_0_academic_content_aut), type = "text")
## stargazer(readd(aut_read_1_academic_content_aut), type = "text")

# Difference between 1-0 coefficients in read is significantly
# different from 0
## sig_vs_sig(0.02, 0.01, -0.03, 0.01)

############################# Personnel Autonomy ##############################
###############################################################################

## stargazer(readd(aut_math_0_personnel_aut), type = "text")
## stargazer(readd(aut_math_1_personnel_aut), type = "text")

# Difference between 1-0 coefficients in math is significantly
# different from 0
## sig_vs_sig(0.02, 0.01, -0.03, 0.01)

## stargazer(readd(aut_read_0_personnel_aut), type = "text")
## stargazer(readd(aut_read_1_personnel_aut), type = "text")

# Difference between 1-0 coefficients in read is NOT significantly
# different from 0
## sig_vs_sig(0.01, 0.01, 0.01, 0.01)

############################# Budget Autonomy #################################
###############################################################################

## stargazer(readd(aut_math_0_budget_aut), type = "text")
## stargazer(readd(aut_math_1_budget_aut), type = "text")

# Difference between 1-0 coefficients in math is NOT significantly
# different from 0
## sig_vs_sig(0.03, 0.01, 0.01, 0.01)

## stargazer(readd(aut_read_0_budget_aut), type = "text")
## stargazer(readd(aut_read_1_budget_aut), type = "text")

# Difference between 1-0 coefficients in math is NOT significantly
# different from 0
## sig_vs_sig(0.03, 0.01, 0.01, 0.01)

############################# Playing around ##################################
###############################################################################

## filtered_data <- readd(filtered_data)
## dv <- "math"
## aut_var <- "academic_content_aut"

## filtered_data <- readd(filtered_data)
## filtered_data %>% select(escs_dummy, quantiles_escs)

## filtered_data %>%
##   select(escs_dummy, quantiles_escs) %>%
##   filter(!is.na(escs_dummy)) %>%
##   count(escs_dummy, quantiles_escs)

## all_mods <- readd(aut_math_1_academic_content_aut)
## all_mods <- readd(aut_allpooled_read_1)
## stargazer::stargazer(all_mods, type = "text")

## library(emmeans)

## all_slopes <-
##   emtrends(
##     all_mods[[2]],
##     "quantiles_escs_chr",
##     "academic_content_aut",
##   )

## plot(all_slopes)


## visreg::visreg(all_mods[[2]],
##                "academic_content_aut",
##                "quantiles_escs",
##                breaks = 5,
##                layout = c(10, 1),
##                gg = TRUE,
##                type = "contrast") +
##   ylim(c(0, 0.02))

## ggpredict(all_mods[[2]], c("academic_content_aut", "quantiles_escs")) %>% plot()
