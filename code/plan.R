############################# Config values ###################################
###############################################################################
Sys.setenv(R_MAX_NUM_DLLS = 512)

raw_data_dir <- here("raw_data")

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


countries <- c("Finland",
               "France",
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


reliability_pisa <-
  c("2000" = 0.81,
    "2003" = 0.85,
    "2006" = 0.78,
    "2009" = 0.74,
    "2012" = 0.82,
    "2015" = 0.74) # 2015 imputed


# Create recoding vector for car::recode in the below map call.
recode_cntrys <-
  enframe(pisa_countrynames) %>% # names come from personal cimentadaj package
  mutate(recoder = paste0("'", name, "'", " = ", "'", value, "'")) %>%
  pull(recoder) %>%
  paste0(collapse = ";")


############################# Helper functions ################################
###############################################################################


# Function calculates the bottom 30th quantile for the bottom educated and the 70th quantile
# for the top educated. If the quantiles cannot be estimated, it returns two NA's instead
quantile_missing <- function(df, weights, probs) {

  quan <- try(Hmisc::wtd.quantile(
    df$escs_trend,
    weights = df[[weights]],
    probs = probs
  ))

  if (any("try-error" %in% class(quan))) {
    return(c(NA, NA))
  } else {
    return(c(quan[1], quan[2]))
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
test_diff <- function(df, reliability, test, probs) {

  map2(df, reliability, function(.x, .y) {

    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    weights_var <- conf$variables$weightFinal

    country_split <- split(.x, .x$country)
    
    country_list <- map(country_split, function(country) {
      print(unique(country$country))

      quan <- quantile_missing(country, weights_var, probs)

      country$escs_dummy <-
        with(country, case_when(escs_trend >= quan[2] ~ 1,
                                escs_trend <= quan[1] ~ 0))
      country
    })
    
    rm(country_split)

    .x <-
      enframe(country_list) %>%
      unnest(value)
    
    rm(country_list)

    .x <-
      .x %>%
      dplyr::select(wave,
                    matches(paste0("^PV.*", test, "$")),
                    escs_dummy,
                    country,
                    one_of(weights_var),
                    AGE)

    message(paste(unique(.x$wave), "data ready"))


    test_vars <- paste0("PV", seq_len(conf$parameters$PVreps), test)
    .x[test_vars] <- map(.x[test_vars], ~ ifelse(.x == 9997, NA, .x))

    # Calculate median math score of all PV's
    .x$dv <- rowMedians(as.matrix(.x[test_vars]), na.rm = T)

    # Should I estimate the model separately by country?
    mod1 <- lm(dv ~ AGE,
               weights = .x[[weights_var]],
               data = .x,
               na.action = "na.exclude")

    # Take residuals of model and divide by rmse. Multiply that by
    # 1 / sqrt(reliability of each survey), which is .y in the loop.
    .x$adj_pvnum <- resid(mod1)/modelr::rmse(mod1, .x) * 1 / sqrt(.y)

    mod2 <-
      lmer(adj_pvnum ~ escs_dummy + (1 + escs_dummy | country),
           data = .x,
           weights = .x[[weights_var]])

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

gaps <- c("90th/10th SES gap", "80th/20th SES gap", "70th/30th SES gap")

############################# Drake plan ######################################
###############################################################################

plan <-
  drake_plan(
    pisa_data = target(
      read_harmonize_pisa(raw_data_dir, recode_cntrys),
      format = "fst"
    ),
    ## pisa_school_data = read_harmonize_pisa_school(raw_data_dir, recode_cntrys),
    escs_data = read_escs(raw_data_dir, recode_cntrys),
    tracking_data = read_tracking(raw_data_dir),
    merged_data = merge_data(pisa_data, escs_data),
    ## res_math = test_diff(merged_data, reliability_pisa, "MATH", c(0.1, 0.9)),
    ## res_read = test_diff(merged_data, reliability_pisa, "READ", c(0.1, 0.9)),
    ## results_math = map(res_math, f_ind),
    ## results_read = map(res_read, f_ind),
    ## complete_data_topbottom = pisa_preparer(results_math,
    ##                                         results_read,
    ##                                         type_txt = "90th/10th SES gap"),
    escs_data_trans = escs_dummy_creator(merged_data, c(0.1, 0.9)),
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
    ##   ordered_cnt = order_cnt(complete_data_topbottom, countries),
    ##   p1_evolution_gaps = plot_evolution_gaps(complete_data_topbottom,
    ##                                           ordered_cnt),
    ##   top_bottom_perc = perc_increase_fun(complete_data_topbottom),
    ##   p2_perc_change = perc_graph(
    ##     top_bottom_perc,
    ##     "math",
    ##     "90/10 achievement gap",
    ##     "Percentage change from 2000 to 2015",
    ##     countries
    ##   ),
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
