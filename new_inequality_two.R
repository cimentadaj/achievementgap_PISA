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
  library(tidyverse)
  library(car)
  library(readr)
  library(SAScii)
  library(inequalityintsvy)
  library(lme4)
  library(modelr)
  
  source("./transform_data.R")
  
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
  
  pisa_all <- read_rds("./data/pisa_listcol.Rdata")
  pisa_all2 <- pisa_all
  
  years <- seq(2000, 2015, 3)
  countries <- c("Germany", "United States", "Denmark", "Sweden", "United Kingdom",
                 "Spain", "Italy", "Canada", "Australia")
  
  db <- paste0("pisa", years)
  pisa_all2$value <- map2(pisa_all2$value, db, ~ { .x$wave <- .y; .x})
  pisa_all2$value[[1]]$CNT <- pisa_all2$value[[1]]$COUNTRY
  
  pisa_all2$value <- map(pisa_all2$value, ~ {
    .x$father_edu <- car::recode(as.numeric(.x$FISCED), "8:9 = NA")
    .x$mother_edu <- car::recode(as.numeric(.x$MISCED), "8:9 = NA")
    .x$high_edu_broad <- pmax(.x$father_edu, .x$mother_edu)
    .x$country <- pisa_countrynames[as.character(.x$CNT)]
  
    if (any(unique(.x$wave) %in% c("pisa2012", "pisa2015"))) {
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
  
  # # Producing gap plots for the gap between high educated and low educated.
  # # That is, using the education variable as the grouping variable.
  # results <-
  #   map2(pisa_all2$value, reliability_pisa, function(.x, .y) {
  #     
  #     # min and max education values
  #     vals <- range(.x$high_edu_broad, na.rm = T)
  #     
  #     # recode the lower education to 0 and highest to 1, so university vs no schooling
  #     .x$edu_dummy <- car::recode(.x$high_edu_broad,
  #                                 paste0(vals[2], " = 1; ", vals[1], " = 0", "; else = NA"))
  # 
  #     conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
  #     
  #     # Calculate median math score
  #     .x$dv <- apply(.x[paste0("PV", seq_len(conf$parameters$PVreps), "MATH")], 1, median, na.rm = T)
  #     
  #     mod1 <- lm(dv ~ AGE,
  #                weights = .x[[conf$variables$weightFinal]],
  #                data = .x,
  #                na.action = "na.exclude")
  #     
  #     # Take residuals of model and divide by rmse. Multiply that by
  #     # 1 / sqrt(reliability of each survey), which is .y in the loop.
  #     .x$adj_pvnum <- resid(mod1)/rmse(mod1, .x) * 1 / sqrt(.y) 
  #     
  #     results <- intsvy.mean(variable = "adj_pvnum",
  #                            by = c("country", "edu_dummy"),
  #                            data = .x,
  #                            config = conf)
  #     
  #     message(paste0(unique(.x$wave), " done"))
  #     results
  #   })
  # 
  # reduced_data <-
  #   map2(results, years, function(.x, .y) {
  #     .x %>%
  #     mutate(wave = .y) %>%
  #     filter(!is.na(edu_dummy))
  #     # split(.$country) %>%
  #     # Filter(function(.x) !length(.x$Freq) < 2 & all(.x$Freq >= 30), .) %>%
  #     # Reduce(rbind, .)
  #   }) %>% # Take countries that have the values of edu_dummy and where the frequency
  #   bind_rows() %>%
  #   mutate(lower = Mean - 2 * s.e.,
  #          upper = Mean + 2 * s.e.)
  # 
  # reduced_data %>%
  #   filter(country %in% countries) %>%
  #   ggplot(aes(as.factor(wave), Mean, group = edu_dummy, colour = edu_dummy)) +
  #   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
  #   geom_line() +
  #   facet_wrap(~ country)
  # 
  # reduced_data %>%
  #   select(country, edu_dummy, Mean, wave) %>%
  #   spread(key = edu_dummy, value = Mean) %>%
  #   mutate(diff = `1` - `0`) %>%
  #   gather(edu_dummy, Mean, -country, -wave, -`1`, -`0`) %>%
  #   mutate(continent = countrycode(country, "country.name", "continent"),
  #          region = countrycode(country, "country.name", "region")) %>%
  #   ggplot(aes(as.factor(wave), Mean, group = country, colour = continent)) +
  #   # geom_errorbar(aes(ymin = lower, ymax = upper), width = 1.2) +
  #   geom_line(show.legend = F) +
  #   geom_point(size = 0.5, show.legend = F) +
  #   geom_smooth(aes(group = continent), colour = "blue", method = "lm") +
  #   facet_wrap(~ continent)
  # 
  # reduced_data %>%
  #   select(country, edu_dummy, Mean, wave) %>%
  #   spread(key = edu_dummy, value = Mean) %>%
  #   mutate(diff = `1` - `0`) %>%
  #   gather(edu_dummy, Mean, -country, -wave, -`1`, -`0`) %>%
  #   mutate(continent = countrycode(country, "country.name", "continent"),
  #          region = countrycode(country, "country.name", "region")) %>%
  #   ggplot(aes(as.factor(wave), Mean, group = country, colour = region)) +
  #   # geom_errorbar(aes(ymin = lower, ymax = upper), width = 1.2) +
  #   geom_line(show.legend = F) +
  #   geom_point(size = 0.5, show.legend = F) +
  #   geom_smooth(aes(group = region), colour = "blue", method = "lm") +
  #   facet_wrap(~ region)
  
  
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
  
  # results <-
  #   map2(pisa_all2$value[exclude_two], reliability_pisa[exclude_two], function(.x, .y) {
  # 
  #     conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
  # 
  #     quan <- Hmisc::wtd.quantile(.x$escs_trend,
  #                                 weights = .x[[conf$variables$weightFinal]],
  #                                 probs = c(0.30, 0.90))
  #         .x$escs_dummy <-
  #           with(.x, case_when(escs_trend >= quan[2] ~ 1,
  #                              escs_trend <= quan[1] ~ 0))
  # 
  #         .x %>%
  #           select(wave, matches("^PV.*MATH$"), escs_dummy, country)
  #     message(paste(unique(.x$wave), "data ready"))
  # 
  #     # Calculate median math score
  #     .x$dv <- apply(.x[paste0("PV", seq_len(conf$parameters$PVreps), "MATH")], 1, median, na.rm = T)
  # 
  #     mod1 <- lm(dv ~ AGE,
  #                weights = .x[[conf$variables$weightFinal]],
  #                data = .x,
  #                na.action = "na.exclude")
  # 
  #     # Take residuals of model and divide by rmse. Multiply that by
  #     # 1 / sqrt(reliability of each survey), which is .y in the loop.
  #     .x$adj_pvnum <- resid(mod1)/rmse(mod1, .x) * 1 / sqrt(.y)
  # 
  #     results <- intsvy.mean(variable = "adj_pvnum",
  #                            by = c("country", "escs_dummy"),
  #                            data = .x,
  #                            config = conf)
  # 
  #     message(paste0(unique(.x$wave), " modeling done"))
  #     results
  #   })
  
  # df_lower <-
  #   pisa_all2$value[[1]] %>%
  #   mutate(new_hisced = as.character(dplyr::recode(as.numeric(HISCED),
  #                                                  `0` = 1))) %>%
  #   filter(country == "United States", new_hisced == 1)
  # 
  # df_upper <- 
  #   pisa_all2$value[[6]] %>%
  #   mutate(new_hisced = as.character(dplyr::recode(as.numeric(HISCED),
  #                                                  `0` = 1))) %>%
  #   filter(country == "United States", new_hisced == 6)
  # 
  # qp_lower <- Hmisc::wtd.quantile(df_lower$hisei,
  #                     weights = df_lower[["spftw0"]],
  #                     probs = c(0.10))
  # 
  # qp_upper <- Hmisc::wtd.quantile(df_upper$hisei,
  #                                 weights = df_upper[["spftw0"]],
  #                                 probs = c(0.95))
  # 
  # df %>%
  #   ggplot(aes(hisei, colour = as.character(new_hisced))) +
  #   geom_freqpoly() +
  #   geom_vline(xintercept = c(qp_lower, qp_upper)) +
  #   scale_x_continuous(breaks = seq(min(df$hisei, na.rm = T),
  #                                   max(df$hisei, na.rm = T), 2))
  # 
  # df %>%
  #   mutate(really_high_really_low = case_when(.$hisei >= qp_upper[1] ~ "1",
  #                                             .$hisei <= qp_lower[1] ~ "0")) %>%
  #   count(country, really_high_really_low) %>%
  #   filter(!is.na(really_high_really_low)) %>%
  #   arrange(n)
  # 
  # qp <- Hmisc::wtd.quantile(df$escs_trend,
  #                           weights = df[["spftw0"]],
  #                           probs = c(0.05, 0.95))
  # 
  # df %>%
  #   ggplot(aes(escs_trend, colour = new_hisced)) +
  #   geom_freqpoly() +
  #   geom_vline(xintercept = qp) +
  #   scale_x_continuous(breaks = seq(min(pisa_all2$value[[6]]$escs_trend, na.rm = T),
  #                                   max(pisa_all2$value[[6]]$escs_trend, na.rm = T), 2))
  # 
  # df %>%
  #   mutate(really_high_really_low = case_when(.$escs_trend >= qp[2] ~ "1",
  #                                             .$escs_trend <= qp[1] ~ "0")) %>%
  #   count(country, really_high_really_low) %>%
  #   filter(!is.na(really_high_really_low)) %>%
  #   arrange(n)
  
  # Function calculates the bottom 5th quantile for the bottom educated and the 95th quantile
  # for the top educated. If the quantiles can't be estimated, it returns two NA's instead
  quantile_missing <- function(df, weights) {
    
    df_lower <- filter(df, new_hisced == 1)
    df_upper <- filter(df, new_hisced == 7)
    
    
    quan_lower <- try(Hmisc::wtd.quantile(df_lower$escs_trend,
                                          weights = df_lower[[weights]],
                                          probs = c(0.30)))
    
    quan_upper <- try(Hmisc::wtd.quantile(df_upper$escs_trend,
                                    weights = df_upper[[weights]],
                                    probs = c(0.70)))
    
    if (any("try-error" %in% c(class(quan_lower), class(quan_upper)))) {
      return(c(NA, NA))
      } else {
     return(c(quan_lower[1], quan_upper[1]))
    }
  }
  
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
  
  # Producing the plot to get the difference between the top 5% of the high educated
  # vs the bottom 5% of the low educated.
  test_diff <- function(df, reliability, test) {
  
    map2(df, reliability, function(.x, .y) {
      
      conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
      weights_var <- conf$variables$weightFinal
      
      country_split <- split(.x, .x$country)
      
      country_list <- map(country_split, function(country) {
        print(unique(country$country))
        
        # In some countries the quan can't be estimated because of very few obs.
        # The function doesn't stop but returns two NA's.
        quan <- quantile_missing(country, weights_var)
        
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
        select(wave, matches(paste0("^PV.*", test, "$")), escs_dummy,
               country, one_of(weights_var),
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

  results_math <- test_diff(adapted_year_data, reliability_pisa, "MATH")
  # results_read <- test_diff(adapted_year_data, reliability_pisa, "READ")
  # US is missing for reading
  
  reduced_data <-
    map2(results_math, years, function(.x, .y) {
      .x %>%
        mutate(wave = .y) %>%
        filter(!is.na(escs_dummy))
    }) %>%
    bind_rows() %>%
    mutate(lower = Mean - 1.96 * s.e.,
           upper = Mean + 1.96 * s.e.)
  
  # Graph the gap by using both the top and bottom instead of the absolute diff
  reduced_data %>%
    filter(country %in% c("United States")) %>%
    ggplot(aes(as.character(wave), Mean, group = escs_dummy, colour = escs_dummy)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_line() +
    facet_wrap(~ country)
  
  # Calculate the joint standard error of the different
  se_data <-
    reduced_data %>%
    select(country, escs_dummy, wave, s.e.) %>%
    spread(escs_dummy, s.e.) %>%
    transmute(country, wave,
              se_diff = sqrt(abs(`1`^2 - `0`^2)))
  
  # Calculate the different between the gap and together with it's joint s.e graph
  # the absolut difference.
  
  countries <- c("Chile", "Austria", "Belgium", "Canada", "Czech Republic", "Denmark",
                 "Finland", "France", "Germany", "Italy", "Japan", "Netherlands", "Norway",
                 "Poland", "Spain", "Sweden", "United Kingdom", "United States")

  reduced_data %>%
    select(country, escs_dummy, Mean, wave) %>%
    spread(key = escs_dummy, value = Mean) %>%
    mutate(diff = `1` - `0`) %>%
    gather(escs_dummy, Mean, -country, -wave, -`1`, -`0`) %>%
    left_join(se_data) %>%
    mutate(continent = countrycode(country, "country.name", "continent"),
           region = countrycode(country, "country.name", "region"),
           lower = Mean - 1.96 * se_diff,
           upper = Mean + 1.96 * se_diff) %>%
    filter(!is.na(continent), !is.na(region), country %in% countries) %>%
    ggplot(aes(as.factor(wave), Mean, group = country, colour = country)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    geom_line() +
    geom_point(size = 0.5) +
    scale_colour_discrete(guide = F) +
    # geom_smooth(aes(group = region), colour = "blue", method = "lm") +
    coord_cartesian(ylim = c(0, 4)) +
    facet_wrap(~ country) +
    ggtitle("math")

# Next steps:

# Making country groups of the graphs showing that some countries are increasing, others are steady
# While others are decreasing. Do this with a few countries, but end by showing all countries.

# Continue by studying what's happening in those countries where it's increasing/decreasing
# In the gap graph
  
# Continue by doing the multilevel models to see what explains what. Include
# all indicators from the reardon/russian girl paper.
  
# Graph the increase in each country vs the increase/decrease of the economic inequality indicators
# Specially the 90/10

# Calculate how big is the gap between reading and math
  
# Continue with the PIRLS to see if there are specific patterns in 4th and 8th graders gap.
  
# Get each country trendline adjusted for the inequality indicators and place in the same country graph.

# Should I add the parent's education in the lm model to see how trends change adjusted for that?
  
  walk(countries, ~ {
    reduced %>%
    filter(!is.na(continent), !is.na(region), country == .x) %>%
    ggplot(aes(as.factor(wave), Mean, group = country, colour = country)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    geom_line() +
    geom_point(size = 0.5) +
    scale_colour_discrete(guide = F) +
    # geom_smooth(aes(group = continent), colour = "blue", method = "lm") +
    coord_cartesian(ylim = c(0, 2.5)) +
    facet_wrap(~ country) +
    ggtitle(paste0(.x, "_read"))
    
    ggsave(paste0(.x, "_read.png"), path = "./plots/")
  })
  

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
  
# Exploratory section
library(inequalityintsvy)

# Join with inequality data and see how much these indicators explain the variance
# between countries.
m <-
  reduced_data %>%
  select(country, escs_dummy, Mean, wave) %>%
  spread(key = escs_dummy, value = Mean) %>%
  mutate(diff = `1` - `0`,
         wave = as.character(wave)) %>%
  select(country, wave, diff) %>%
  as_tibble() %>%
  left_join(inequality_data, by = c("wave" = "year", "country"))


# Excluding and including value decreases the between country variation by over 50%

m %>%
  filter(indicators == "P90P10") %>%
  lmer(diff ~ 1 + (1 + value | country), data = .) %>%
  display()

# Compare between the education variable and the 95th and 5th difference.

# In addition compute
# For inside high/low edu, the gap between the 90/50 and 90/10
# For the absolute difference the gap between 95/5, 90/10, 90/50



pdf_link <- "https://www.russellsage.org/sites/default/files/duncan_murnane_online_appendix.pdf"




