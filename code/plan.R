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

      # it is very important to create a variable that returns the number of observations of this dummy
      # For each country. Possibly to weight by the number of observations.
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

escs_dummy_creator <- function(df, probs) {

  map(df, function(.x) {

    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    weights_var <- conf$variables$weightFinal

    country_split <- split(.x, .x$country)

    country_list <- map(country_split, function(country) {
      print(unique(country$country))

      quan <- quantile_missing(country, weights_var, probs)

      # it is very important to create a variable that returns the number of observations of this dummy
      # For each country. Possibly to weight by the number of observations.
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

    message(paste(unique(.x$wave), "data ready"))

    .x
  })
}

############################# Drake plan ######################################
###############################################################################

plan <-
  drake_plan(
    pisa_data = target(
      read_harmonize_pisa(raw_data_dir, recode_cntrys),
      format = "fst"
    ),
    escs_data = read_escs(raw_data_dir, recode_cntrys),
    merged_data = merge_data(pisa_data, escs_data),
    res_math = test_diff(merged_data, reliability_pisa, "MATH", c(0.1, 0.9)),
    res_read = test_diff(merged_data, reliability_pisa, "READ", c(0.1, 0.9))
    ## results_math = map(res_math, f_ind),
    ## results_read = map(res_read, f_ind)
  )
