## ----packages_conf, echo = F---------------------------------------------
  library(knitr)
  library(arm)
  library(saves)
  library(haven)
  library(PISA2000lite)
  library(PISA2003lite)
  library(PISA2006lite)
  library(PISA2009lite)
  library(PISA2012lite)
  library(intsvy)
  library(cimentadaj) # For pisa_countrynames
  library(countrycode) # For region variable
  library(car)
  library(readr)
  library(SAScii)
  library(inequalityintsvy)
  library(lme4)
  library(modelr)
  library(tidyverse)
  library(ggrepel)
  
  # source("./transform_data.R")

  # Conf for PISA_2015
  pisa2015_conf <- list(variables = list(pvlabelpref = "PV",
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


## ----loading_data-recoding-----------------------------------------------
pisa_all <- read_rds("./data/pisa_listcol.Rdata")
pisa_all2 <- pisa_all

years <- seq(2000, 2015, 3)
countries <- c("Germany", "United States", "Denmark", "Sweden", "United Kingdom",
                 "Spain", "Italy", "Canada", "Australia")
  
db <- paste0("pisa", years)
pisa_all2$value <- map2(pisa_all2$value, db, ~ { .x$wave <- .y; .x})
pisa_all2$value[[1]]$CNT <- pisa_all2$value[[1]]$COUNTRY
  
pisa_all2$value <- map(pisa_all2$value, ~ {
  
# 2000 to 2015
# The coding is from 0 to 6, where 0 is no schooling and 6 is
# BA or above.

# When turning 0:6 to numeric, it becomes 1:7 that's why
# I recode 8:9 to NA. This, however, didn't work for last two surveys
  
  .x$father_edu <- car::recode(as.numeric(.x$FISCED), "8:9 = NA")
  .x$mother_edu <- car::recode(as.numeric(.x$MISCED), "8:9 = NA")
  .x$high_edu_broad <- pmax(.x$father_edu, .x$mother_edu)
  .x$country <- pisa_countrynames[as.character(.x$CNT)]
  
  if (any(unique(.x$wave) %in% c("pisa2012", "pisa2015"))) {
    # These two surveys were from 0:6 so I had to add + 1
    # so that it equals 1:7 as all other surveys.
    .x$father_edu <- .x$father_edu + 1
    .x$mother_edu <- .x$mother_edu + 1
    .x$high_edu_broad <- .x$high_edu_broad + 1
  }
  .x
})
  
reliability_pisa <-
  c("2000" = 0.81,
    "2003" = 0.85,
    "2006" = 0.78,
    "2009" = 0.74,
    "2012" = 0.82,
    "2015" = 0.74) # 2015 imputed


## ----escs_trend, cache = TRUE--------------------------------------------
  # Rescaled trend ESCS data to merge.
  # This only has data for seq(2000, 2012, 3) because
  # PISA 2015 has the ESCS trend variable.
  dir <- tempdir()
  file_name <- "escs_trend.zip"
  download.file("http://vs-web-fs-1.oecd.org/pisa/trend_escs_SPSS.zip",
                destfile = file.path(dir, file_name))
  unzip(file.path(dir, file_name), exdir = dir)
  escs_trend <- map(file.path(dir, list.files(dir, pattern = ".sav")), haven::read_spss)
  file.remove(file.path(dir, list.files(dir)))
  
  escs_trend <-
    map(escs_trend, ~ {
    mutate(.x, cnt = pisa_countrynames[cnt]) %>%
    rename(country = cnt)
  })


## ----merge_escs_pisa, cache = TRUE---------------------------------------
   # Next we'll merge the ESCS data with the PISA data. As explained above, the 6th data (PISA
  # 2015) doesn't need to be merged so I exclude it with this vector
  exclude <- -6
  
  # Loop in parallel to the PISA data, the ESCS data and the year vector (which is seq(2012, 2015, 3))
  pisa_all2$value[exclude] <-
    pmap(list(pisa_all2$value[exclude], escs_trend, years[exclude]), function(.x, .y, .z) {
    
    # The escs data needs to have the key variables the same class as the
    # same data.
    escs <-
      .y %>% mutate(schoolid = as.numeric(schoolid),
                    stidstd = as.numeric(stidstd))
    
    # .z is the corresponding year that will be created as a column
    # And perform the same transformation of the key variables as in the ESCS data
    data_trend <-
      .x %>%
        mutate(
          year = .z,
          schoolid = as.numeric(as.character(SCHOOLID)),
          stidstd = as.numeric(as.character(STIDSTD))
          ) %>%
   left_join(escs,
              by = c("country", "schoolid", "stidstd"))
    
    message(paste(unique(.x$wave), "done"))
    
    data_trend
  })
  
  pisa_all2$value[[6]] <-
    pisa_all2$value[[6]] %>%
    rename(escs_trend = ESCS)

## ----functions_for_modelling---------------------------------------------

# Function calculates the bottom 30th quantile for the bottom educated and the 70th quantile
# for the top educated. If the quantiles can't be estimated, it returns two NA's instead
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
        
        # It's very important to create a variable that returns the number of observations of this dummy
        # For each country. Possibly to weight by the number of observations.
        country$escs_dummy <-
          with(country, case_when(escs_trend >= quan[2] ~ 1,
                                  escs_trend <= quan[1] ~ 0))
        country
      })
      
      .x <-
        enframe(country_list) %>%
        unnest(value)

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
      .x$dv <- apply(.x[test_vars], 1, median, na.rm = T)
      
      # Should I estimate the model separately by country?
      mod1 <- lm(dv ~ AGE,
                 weights = .x[[weights_var]],
                 data = .x,
                 na.action = "na.exclude")
      
      # Take residuals of model and divide by rmse. Multiply that by
      # 1 / sqrt(reliability of each survey), which is .y in the loop.
      .x$adj_pvnum <- resid(mod1)/rmse(mod1, .x) * 1 / sqrt(.y)
      
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
      results
    })
}

## ----modeling------------------------------------------------------------
adapted_year_data <-
    map(pisa_all2$value, ~ {
      if (unique(.x$wave) == "pisa2000") {
        # pisa2000 has a different coding so here I recode 6 to 7 so that in all waves the top edu
        # is 7 and the bottom is 1
        .x <-
          mutate(.x, new_hisced = as.character(dplyr::recode(as.numeric(high_edu_broad), `6` = 7)))
      } else {
        .x <-
          mutate(.x, new_hisced = as.character(high_edu_broad))
      }
      .x
})

# results_math <- test_diff(adapted_year_data, reliability_pisa, "MATH")
# results_read <- test_diff(adapted_year_data, reliability_pisa, "READ")
# results_math_topmid <- test_diff(adapted_year_data, reliability_pisa, "MATH", c(0.5, 0.9))
# results_read_topmid <- test_diff(adapted_year_data, reliability_pisa, "READ", c(0.5, 0.9))
# results_math_midbottom <- test_diff(adapted_year_data, reliability_pisa, "MATH", c(0.1, 0.5))
# results_read_midbottom <- test_diff(adapted_year_data, reliability_pisa, "READ", c(0.1, 0.5))

results_math <- read_rds("./data/delete.Rdata")
results_read <- read_rds("./data/delete_read.Rdata")
results_math_topmid <- read_rds("./data/delete_math_topmid.Rdata")
results_read_topmid <- read_rds("./data/delete_read_topmid.Rdata")
results_math_midbottom <- read_rds("./data/delete_math_midbottom.Rdata")
results_read_midbottom <- read_rds("./data/delete_read_midbottom.Rdata")
# US is missing for reading

# Get sample counts for each dummy
sample_size_calc <- function(df, probs, selected = F, cnts = NULL) {
  
  stopifnot(selected & !is.null(NULL))
  if (selected) df <- map(df, ~ filter(.x, country %in% cnts))
  
  cnt_to_bind <-
    map(df, function(df) {
      
      print(unique(df$wave))
      conf <- if (unique(df$wave) == "pisa2015") pisa2015_conf else pisa_conf
      weights_var <- conf$variables$weightFinal
      
      split_df <- split(df, df$country)
      
      split_df_two <-
        map(split_df, ~ {
          # In some countries the quan can't be estimated because of very few obs.
          # The function doesn't stop but returns two NA's.
          quan <- quantile_missing(.x, weights_var, probs)
          
          # It's very important to create a variable that returns the number of observations of this dummy
          # For each country. Possibly to weight by the number of observations.
          .x$escs_dummy <-
            with(.x, case_when(escs_trend >= quan[2] ~ 1,
                               escs_trend <= quan[1] ~ 0))
          .x
        })
      unsplit_df <- split_df_two %>% enframe() %>% unnest(value)
      
      unsplit_df %>%
        count(country, escs_dummy) %>%
        filter(!is.na(escs_dummy)) %>%
        left_join(summarize(group_by(unsplit_df, country), total_n = n()), by = "country") %>%
        mutate(perc = paste0(round(n / total_n * 100, 0), "%")) %>%
        select(-total_n)
    })
  setNames(cnt_to_bind, seq(2000, 2015, 3)) %>%
    enframe() %>%
    unnest()
}
sample_tables <- sample_size_calc(adapted_year_data, c(.1, .9), selected = TRUE, countries)

# Cache is not working properly for the code above, so I just load the saved cached file
# load("./paper/cache/modeling_9a0b38d1d53fa243b0242580f0672fa5.RData")


## ------------------------------------------------------------------------
countries_subset <- c("Australia",
          "Germany",
          "Denmark",
          "Spain",
          "France",
          "Italy",
          "Netherlands",
          "Sweden",
          "Finland",
          "United States",
          "United Kingdom")

## ----eval = F------------------------------------------------------------
# In this chunk you can join reading and math datasets

pisa_preparer <- function(df_math, df_read) {

descrip_math <- map(df_math, ~ rename(.x, mean_math = Mean, se_math = s.e.))
descrip_read <- map(df_read, ~ rename(.x, mean_read = Mean, se_read = s.e.))

## -- correlation with indicators --------
reduced_data_math <-
  map2(descrip_math, years, function(.x, .y) {
    .x %>%
      mutate(wave = .y) %>%
      filter(!is.na(escs_dummy))
  }) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(lower_math = mean_math - 1.96 * se_math,
         upper_math = mean_math + 1.96 * se_math)

reduced_data_read <-
  map2(descrip_read, years, function(.x, .y) {
    .x %>%
      mutate(wave = .y) %>%
      filter(!is.na(escs_dummy))
  }) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(lower_read = mean_read - 1.96 * se_read,
         upper_read = mean_read + 1.96 * se_read)

reduced_data <- left_join(reduced_data_math,
                          reduced_data_read, by = c("country", "escs_dummy", "wave"))

# reduced_data %>%
#   dplyr::select(country, wave, escs_dummy, mean_math) %>%
#   spread(escs_dummy, mean_math) %>%
#   transmute(country, wave = as.character(wave), avg_diff = `1` - `0`) %>%
#   left_join(inequalityintsvy::economic_inequality, by = c("wave" = "year", "country")) %>%
#   filter(indicators == "GINI") %>%
#   group_by(country) %>%
#   summarize(avg_diff = mean(avg_diff, na.rm = T),
#             avg_value = mean(value, na.rm = T)) %>%
#   ggplot(aes(avg_value, avg_diff)) +
#   geom_point() +
#   geom_smooth(method = "lm")
# 
# 
# # But if I eclude some outliers, the relationship is non linear
# reduced_data %>%
#   select(country, wave, escs_dummy, mean_math) %>%
#   spread(escs_dummy, mean_math) %>%
#   transmute(country, wave = as.character(wave), avg_diff = `1` - `0`) %>%
#   left_join(inequalityintsvy::economic_inequality, by = c("wave" = "year", "country")) %>%
#   filter(indicators == "GINI") %>%
#   group_by(country) %>%
#   summarize(avg_diff = mean(avg_diff, na.rm = T),
#             avg_value = mean(value, na.rm = T),
#             high_value = avg_value > 0.3) %>%
#   filter(avg_diff <= 2.7) %>%
#   ggplot(aes(avg_value, avg_diff, colour = high_value)) +
#   geom_point() +
#   geom_smooth(method = "lm")

## ----ci_for_difference---------------------------------------------------

test_data <-
  reduced_data %>%
  select(country, wave, escs_dummy, contains("mean")) %>%
  gather(test, score, contains("mean"))

math_data <-
  reduced_data %>%
  select(country, wave, escs_dummy, contains("math")) %>%
  gather(test_bound, bound, contains("lower"), contains("upper")) %>%
  select(-contains("math")) %>%
  right_join(filter(test_data, test == "mean_math"))

read_data <-
  reduced_data %>%
  select(country, wave, escs_dummy, contains("read")) %>%
  gather(test_bound, bound, contains("lower"), contains("upper")) %>%
  select(-contains("read")) %>%
  right_join(filter(test_data, test == "mean_read"))

all_data <- bind_rows(math_data, read_data)

## ------------------------------------------------------------------------
# Calculate the joint standard error of the different
math_se_data <-
  reduced_data %>%
  select(country, escs_dummy, wave, se_math) %>%
  spread(escs_dummy, se_math) %>%
    transmute(country, wave,
              se_diff_math = sqrt(abs(`1`^2 - `0`^2)))

read_se_data <-
  reduced_data %>%
  select(country, escs_dummy, wave, se_read) %>%
  spread(escs_dummy, se_read) %>%
  transmute(country, wave,
            se_diff_read = sqrt(abs(`1`^2 - `0`^2)))

se_data <- left_join(math_se_data, read_se_data)


## ----include = T, out.height = '5in', out.width = '5.5in', fig.align = 'center'----

# Calculate the different between the gap and together with it's joint s.e graph
# the absolut difference.

math_diff <-
  reduced_data %>%
  select(wave, country, escs_dummy, mean_math) %>%
  spread(escs_dummy, mean_math) %>%
  transmute(wave, country, diff_math = `1` - `0`)

read_diff <-
  reduced_data %>%
  select(wave, country, escs_dummy, mean_read) %>%
  spread(escs_dummy, mean_read) %>%
  transmute(wave, country, diff_read = `1` - `0`)


# data_summaries <-
#   math_diff %>%
#   left_join(math_se_data) %>%
#   transmute(wave, country, diff_math,
#             lower_math = diff_math - 1.96 * se_diff_math,
#             upper_math = diff_math + 1.96 * se_diff_math)

data_summaries <-
  math_diff %>%
  left_join(read_diff) %>%
  left_join(se_data) %>%
  transmute(wave, country, diff_math, diff_read,
           lower_math = diff_math - 1.96 * se_diff_math,
           lower_read = diff_read - 1.96 * se_diff_read,
           upper_math = diff_math + 1.96 * se_diff_math,
           upper_read = diff_read + 1.96 * se_diff_read)

differences <-
  data_summaries %>%
  select(wave, country, diff_math, diff_read) %>%
  gather(test, difference, starts_with("diff")) %>%
  mutate(type_test = ifelse(.$test == "diff_math", "math", "read"))

# differences <-
#   data_summaries %>%
#   select(wave, country, diff_math) %>%
#   gather(test, difference, starts_with("diff"))

bounds_lower <-
  data_summaries %>%
  select(wave, country, contains("lower")) %>%
  gather(lower_bound, lower, lower_math, lower_read) %>%
  mutate(type_test = ifelse(grepl("math", .$lower_bound), "math", "read"))

# bounds_lower <-
#   data_summaries %>%
#   select(wave, country, contains("lower")) %>%
#   gather(lower_bound, lower, lower_math)

bounds_upper <-
  data_summaries %>%
  select(wave, country, contains("upper")) %>%
  gather(upper_bound, upper, upper_math, upper_read) %>%
  mutate(type_test = ifelse(grepl("math", .$upper_bound), "math", "read"))

# bounds_upper <-
#   data_summaries %>%
#   select(wave, country, contains("upper")) %>%
#   gather(upper_bound, upper, upper_math)

# Getting the original data in
original_math <-
  reduced_data_math %>%
  select(wave, everything(), -se_math) %>%
  gather(metric, value, -(wave:escs_dummy)) %>%
  unite(combination, escs_dummy, metric, sep = "_") %>%
  spread(combination, value) %>%
  mutate(type_test = "math")

original_read <-
  reduced_data_read %>%
  select(wave, everything(), -se_read) %>%
  gather(metric, value, -(wave:escs_dummy)) %>%
  unite(combination, escs_dummy, metric, sep = "_") %>%
  spread(combination, value) %>%
  mutate(type_test = "read")

# final data
complete_data <-
  left_join(differences, bounds_lower) %>%
  left_join(bounds_upper) %>%
  left_join(original_math) %>%
  left_join(original_read)
}

complete_data_topbottom <- pisa_preparer(results_math, results_read)
complete_data_topmid <- pisa_preparer(results_math_topmid, results_read_topmid)
complete_data_midbottom <- pisa_preparer(results_math_midbottom, results_read_midbottom)

complete_data_topbottom <- mutate(complete_data_topbottom, type = "90th/10th SES gap")
complete_data_topmid <- mutate(complete_data_topmid, type = "90th/50th SES gap")
complete_data_midbottom <- mutate(complete_data_midbottom, type = "50th/10th SES gap")


# 90/10 gaps acros countries
complete_data_topbottom %>%
  filter(country %in% c("United States", "Netherlands", "France",
                        "Germany", "Poland", "Finland")) %>%
  ggplot(aes(as.factor(wave), difference, group = type_test, colour = type_test)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() +
  geom_point(size = 0.5) +
  coord_cartesian(ylim = c(-0.5, 3)) +
  facet_wrap(~ country)

# Comparing gaps across countries
complete_data_topbottom %>%
  bind_rows(complete_data_topmid) %>%
  bind_rows(complete_data_midbottom) %>%
  filter(country %in% c("United States", "Denmark", "France")) %>%
  mutate(type = factor(type,
                       levels = c("90th/10th SES gap", "90th/50th SES gap", "50th/10th SES gap"),
                       ordered = TRUE)) %>%
  ggplot(aes(as.factor(wave), difference, group = type_test, colour = type_test)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() +
  geom_point(size = 0.5) +
  coord_cartesian(ylim = c(-0.5, 3)) +
  facet_grid(country ~ type)

# Comparing gaps across countries
complete_data_topbottom %>%
  bind_rows(complete_data_topmid) %>%
  bind_rows(complete_data_midbottom) %>%
  filter(country %in% c("Germany", "Denmark", "France"), type_test == "math") %>%
  select(wave, country, type_test, type, contains("math")) %>%
  mutate(type = factor(type,
                       levels = c("90th/10th SES gap", "90th/50th SES gap", "50th/10th SES gap"),
                       ordered = TRUE)) %>%
  gather(score, value, -(wave:type)) %>%
  separate(score, c("ses", "score"), sep = 2) %>%
  spread(score, value) %>%
  ggplot(aes(as.factor(wave), mean_math, group = ses, colour = ses)) +
  geom_errorbar(aes(ymin = lower_math, ymax = upper_math), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line() +
  geom_point(size = 0.5) +
  coord_cartesian(ylim = c(-0.5, 3)) +
  facet_grid(country ~ type)

# Show table with % increase decrease over time

# Increase:  
# Sweden - steady increase in both tests
# Austria - increase in math - slight increase in read
# Finland - very sharp increase in both
# France - very sharp increase in both
# Netherlands - sharp increase in both

# Decrease:
# US - decrease in both tests
# Chile - decrease in both tests
  
# No change:
# Canada - stable red - increase math
# UK - slight decrease red - stable math
# Belgium - no change
# Czech republic - no change
# Denmark no change
# Germany - no change
# ITaly - no change
# Japan -  no change
# Norway - no change
# Poland - no change
# Spain - no change

## ------------------------------------------------------------------------
# Rate of change

  
avg_increase_fun <- function(df, class) {

# Average standard deviation increase
  data_ready <-
    df %>%
    select(wave, country, type_test, contains("mean_math")) %>%
    gather(metric, value, -(wave:type_test)) %>%
    separate(metric, c("ses", "test"), sep = 2) %>%
    spread(test, value) %>%
    mutate(ses = gsub("_", "", ses)) %>%
    filter(type_test == "math", ses == class) %>%
    split(.$country) %>%
    map(~ mutate(.,
                 diff = c(diff(mean_math, lag = 1), NA),
                 perc = round(diff / mean_math, 2) * 100,
                 perc_pos = mean(perc > 0, na.rm = T))) %>%
    enframe() %>%
    unnest(value) %>%
    group_by(country) %>%
    summarize(avg_diff = mean(diff, na.rm = T))
  
  data_ready
}

avg_sd_increase_high <- avg_increase_fun(complete_data_topbottom, 1)
avg_sd_increase_low <- avg_increase_fun(complete_data_topbottom, 0)

full_data <-
  left_join(avg_sd_increase_high, avg_sd_increase_low, by = "country") %>%
  mutate(continent = ifelse(country %in% c(countries, "France"), "my_cnt", "other_cnt"))

colnames(full_data) <- c("country", "high_increase", "low_increase", "continent")

lims <- list(xlim = c(-0.15, 0.25), ylim = c(-0.25, 0.25))

rect_data <- tibble(xst = c(lims$xlim[1], 0),
                    xen = c(0.0, lims$xlim[2]),
                    yst = c(0.0, lims$ylim[1]),
                    yen = c(lims$ylim[2], 0),
                    colour = c("red", "green"))

full_data %>%
  ggplot(aes(low_increase, high_increase), alpha = 0.2) +
  geom_rect(data = rect_data, aes(xmin = xst,
                                  xmax = xen,
                                  ymin = yst,
                                  ymax = yen),
            fill = rect_data$colour,
            alpha = 0.2,
            inherit.aes = FALSE) +
  geom_line(stat="smooth", method = "lm", se = FALSE, alpha = 0.5, colour = "grey", size = 1) +
  geom_point(alpha = 0.2) +
  geom_point(data = filter(full_data, continent == "my_cnt"), colour = "red", alpha = 0.7) +
  geom_text_repel(data = filter(full_data, continent == "my_cnt"), aes(label = country), box.padding = unit(2.7, "lines")) +
  geom_vline(xintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  xlim(lims$xlim) +
  ylim(lims$ylim) +
  coord_cartesian(expand = FALSE) +
  annotate(geom = "text", x = 0.15, y = -0.2, label = "Low SES are catching up \n faster than High SES", fontface = 2, size = 3) +
  annotate(geom = "text", x = -0.1, y = 0.20, label = "High SES are increasing  \n faster than Low SES", fontface = 2, size = 3) +
  labs(x = "Average increase of low SES in SD", y = "Average increase of high SES in SD") +
  theme_minimal()

## ------------------------------------------------------------------------

# Show the rates at which is increasing/decreasing
perc_increase_fun <- function(df) {
  
  # Average standard deviation increase
  data_ready <-
    df %>%
    select(wave, country, type_test, difference) %>%
    group_by(type_test) %>%
    split(.$country) %>%
    map(~ {
      .x <-
        spread(.x, wave, difference) %>%
        ungroup()
  
      year_vars <- sum(map_dbl(.x, is.numeric)) - 1
      years_subtract <- names(.x)[c(ncol(.x) - year_vars, ncol(.x))]
      years_subtract <- lapply(years_subtract, as.name)

      last_year <- rlang::new_quosure(years_subtract[[2]], env = .GlobalEnv)
      first_year <- rlang::new_quosure(years_subtract[[1]], env = .GlobalEnv)
      
      years_available <-
        .x %>%
        gather(year, val, -(country:type_test)) %>%
        group_by(type_test) %>%
        summarise(yr_avaible = sum(!is.na(val))) %>%
        pull(yr_avaible)
      
      .x %>%
        ungroup() %>%
        transmute(type_test,
                  country,
                  diff = round(((!!last_year) - (!!first_year)) / (!!first_year) * 100, 1),
                  years_available = years_available)
    })
  data_ready
}

top_bottom_perc <- perc_increase_fun(complete_data_topbottom)
top_mid_perc <- perc_increase_fun(complete_data_topmid)


# Gap is closing at an average of the variable diff per year.

## ------------------------------------------------------------------------
# Next steps:

# Continue by doing the multilevel models to see what explains what. Include
# all indicators from the reardon/russian girl paper.
  
# Graph the increase in each country vs the increase/decrease of the economic inequality indicators
# Specially the 90/10

# Calculate how big is the gap between reading and math
  
# Continue with the PIRLS to see if there are specific patterns in 4th and 8th graders gap.
  
# Get each country trendline adjusted for the inequality indicators and place in the same country graph.

# Should I add the parent's education in the lm model to see how trends change adjusted for that?
 
# The gap between the top 90th and the 50th, did it grow?
# The gap between the top 50th and the 10th, did it grow?

# A weak welfare system, together with income inequality, what's their pattern?
# What if we put the school differentiation/tracking aspect in? Are there country groups based on these
# patterns.

# In countries where there is high differentiation/tracking, is there a jump in the evolution of the gap between PIRLS/TIMSS and PISA?


