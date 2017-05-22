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

# Download PISA 2015 data
# temp_dir <- tempdir()
# url <- "http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip"
# dir_pisa <- file.path(temp_dir, "pisa.zip")
# download.file(url, destfile = dir_pisa)
# 
# unzip(dir_pisa, exdir = temp_dir)
# 
# pisa_2015 <- read_spss(file.path(temp_dir, "CY6_MS_CMB_STU_QQQ.sav"))
# 
# # Download PISA 2012 files
# url <- "http://www.oecd.org/pisa/pisaproducts/INT_STU12_DEC03.zip"
# pisa2012_syntax <- "http://www.oecd.org/pisa/pisaproducts/PISA2012_SAS_student.sas"
# 
# # Download the SAS syntax to determine column width
# download.file(pisa2012_syntax, file.path(temp_dir, "PISA2012_SAS_student.sas"))
# dic_student <- parse.SAScii(sas_ri = file.path(temp_dir, 'PISA2012_SAS_student.sas'))
# 
# # Download PISA 2012 actual data
# dir_pisa <- file.path(temp_dir, "pisa2012.zip")
# download.file(url, destfile = dir_pisa)
# unzip(dir_pisa, exdir = temp_dir)
# 
# # Read the data using the fixed width from the SAS syntax
# student2012 <- read_fwf(file = file.path(temp_dir, 'INT_STU12_DEC03.txt'),
#                         col_positions = fwf_widths(dic_student$width), progress = T)
# colnames(student2012) <- dic_student$varname
# 
# # Remove temporary files
# file.remove(file.path(temp_dir, list.files(temp_dir)))
# 
# student2015 <- pisa_2015 # toy dataset
# 
# # # If PISA2012 starts working, then use this code:
# # # Vector with database names except student2015
# # databases <- c("math2000", paste0("student", seq(2003, 2012, 3)))
# #
# # # Create a list with all data sets
# # pisa_list <- map(c(databases, "student2015"), get)
# #
# # # Give each dataset their own name
# # names(pisa_list) <- c("student2000", databases[-1], "student2015")
# 
# # Provisional code for separate PISA2012
# # Vector with database names except student2015
# databases <- c("math2000", paste0("student", seq(2003, 2009, 3)))
# 
# # Create a list with all data sets
# pisa_list <- map(c(databases, paste0("student", c(2012, 2015))), get)
# 
# # Give each dataset their own name
# names(pisa_list) <- c("student2000", databases[-1], "student2012","student2015")
# 
# # Fix the country variable name which is different for PISA 2000
# pisa_list$math2000$CNT <- pisa_list$math2000$COUNTRY
# 
# # Create a data frame with a list column containing all lists.
# pisa_all <- enframe(pisa_list)
# 
# # See:
# pisa_all
# 
# # Go through each PISA and grab the highest education from Male
# # and Female partner. Finally, recode it to three categories
# pisa_all$value <- map(pisa_all$value, function(each_pisa) {
#   each_pisa$high_edu <- pmax(as.numeric(each_pisa$MISCED), # find highest education
#                              as.numeric(each_pisa$FISCED)) # in the household
# 
#   # Recode new highest education into three categories
#   each_pisa$high_edu <- recode(each_pisa$high_edu, "1:3 = 1; 4:5 = 2; 6:7 = 3; else = NA")
#   each_pisa
# })
# 
# # Gender variables:
# # 2000: ST03Q01
# # 2003: ST03Q01
# # 2006: ST04Q01
# # 2009: ST04Q01
# # 2012: ST04Q01
# # 2015: ST004D01T
# 
# # Vector with all gender vars in order of PISA surveys
# gender <- c("ST03Q01", "ST03Q01", "ST04Q01", "ST04Q01", "ST04Q01", "ST004D01T")
# 
# # Loop through each data frame and assign the gender variable name
# # and turn it into character
# pisa_all$value <- map(seq_along(pisa_all$value), function(pisa_data) {
# 
#   pisa_all$value[[pisa_data]] %>%
#     rename_("gender" = gender[pisa_data]) %>%
#     mutate(gender = as.character(gender),
#            country = pisa_countrynames[CNT],
#            region = countrycode(country, "country.name", "region")) %>%
#     as_tibble()
# })
# write_rds(pisa_all, path = "./data/pisa_listcol.Rdata")

# For PISA_2015
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
quantiles <- c(10, 50, 90)

# Function computes arbitrary descriptives with fun for a specific class(topic).
# Using the specified configuration for pisa survey.

pisa_fun_switch <- function(fun, topic, groups, data, quant, config = pisa_conf) {
  if (fun == "quantile" && missing(quant)) stop("Quantiles not supplied")
  
  topic_mean <- paste0("PV", seq_len(config$parameters$PVreps), topic) # Create variable name for mean
  
  # Depending on the type of function, switch to the appropriate function
  switch(fun,
  "mean" = intsvy.mean.pv(pvnames = topic_mean, by = groups, data = data, config = config),
  "quantile" = intsvy.per.pv(pvlabel = topic, by = groups, per = quant, data = data, config = config)
  )
}

# Function loops over df, applies the fun for the chosen topic and the groups specified.
# quant is an optional argument for specifiying quantiles.

pisa_yearly_mean <- function(fun, topic, groups, df, quant) {
  stopifnot(class(df[[2]]) == "list")
  
  map(seq_len(nrow(df)), function(data) { # Loop through the df's rows
    
  print(data) # Where are we?
  config <- switch(df[[data, 1]], "student2015" = pisa2015_conf, pisa_conf) # Which survey config?
  
# Estimate either mean or perc pv and change the config to either pisa 2015 or all others.
# Using the second column of df (which should be the list-column of the data)
  pisa_fun_switch(fun, topic, groups, df[[data, 2]], quant,
                  config = config)
    })
}

# pisa_all2$pvmean <- pisa_yearly_mean("mean", "MATH", c("country"), df = pisa_all2)
# 
# pisa_all2$quantile_mean <- pisa_yearly_mean("quantile", "MATH", c("country", "high_edu"), df = pisa_all2,
#                                             quant = c(10, 50, 90))
# 
# pisa_all2$quantile_mean <- pisa_yearly_mean("quantile", "MATH", c("country", "high_edu"), df = pisa_all2,
#                                             quant = c(10, 90))
#
# pisa_merged <-
#   pisa_all2 %>%
#   separate(name, c("type", "year"), 7)

# write_rds(pisa_merged, path = "./data/pisa_list2.Rdata")
pisa_merged <- read_rds("./data/pisa_list2.Rdata")

unnest_rename <- function(df, col_unnest, var_rename) {
  unnested <-
    df %>%
    unnest_(col_unnest)
  
  if(sum(names(unnested) %in% var_rename) > 1) {
    warning(paste(as.name(var_rename), "not a unique name in unnested df. Renaming the first instance"))
  }
  
  unnested %>%
    rename_(se_mean = var_rename)
}

pisa_ci <- function(df, col_unnest, var_rename, mean_var) {
  dots <- setNames(list(
    lazyeval::interp(~ x - y, x = as.name(mean_var), y = quote(se_twice)),
    lazyeval::interp(~ x + y, x = as.name(mean_var), y = quote(se_twice))
  ), c("low_ci", "upper_ci"))
  
  unnest_rename(pisa_merged, col_unnest, var_rename) %>%
    mutate(
      se_twice = 2 * se_mean,
      region = countrycode(country, "country.name", "region"),
      continent = countrycode(country, "country.name", "continent")
    ) %>%
    mutate_(.dots = dots) %>%
    filter(!is.na(continent))
}

pisa_quant_ci <-
  pisa_ci(pisa_merged, "quantile_mean", "`Std. err.`", "Score")

pisa_gaps <-
  pisa_quant_ci %>%
  unite(edu_perc, high_edu, Percentiles) %>%
  select(year:Score) %>%
  spread(edu_perc, Score) %>%
  mutate(`srich/spoor` = `3_90` - `1_90`,
         `srich/dpoor` = `3_90` - `1_10`,
         `drich/spoor` = `3_10` - `1_90`,
         `avgrich/avgpoor` = `3_50` - `1_50`,
         `srich/avgpoor` = `3_90` - `1_50`,
         `avgpoor/dpoor` = `1_50`- `1_10`) %>%
  select(year, country, contains("rich"), contains("poor")) %>%
  gather(diff, value, -year, -country)

pisa_gaps %>%
  mutate(region = countrycode(country, "country.name", "region")) %>%
  group_by(year, country, diff) %>%
  summarize(avg = mean(value, na.rm = T)) %>%
  filter(country %in% c("Sweden", "Denmark")) %>%
  ggplot(aes(x = year, avg, group = diff, colour = diff)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ country)


pisa_gaps %>%
  mutate(region = countrycode(country, "country.name", "region")) %>%
  group_by(year, region, diff) %>%
  summarize(avg = mean(value, na.rm = T)) %>%
  ggplot(aes(x = year, avg, group = diff, colour = diff)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ region)


pisa_pv_ci <-
  pisa_ci(pisa_merged, "pv_mean", "s.e.", "Mean")

pisa_pv_ci %>%
  group_by(year, continent) %>%
  summarise(
    n = n(),
    mean_score = mean(Mean, na.rm = T),
    mean_low_ci = mean(low_ci, na.rm = T),
    mean_upper_ci = mean(upper_ci, na.rm = T)
  ) %>%
  ggplot(aes(year, mean_score)) +
  geom_point() +
  geom_pointrange(aes(ymin = mean_low_ci,
                      ymax = mean_upper_ci,
                      colour = continent)) +
  geom_line(aes(group = continent,
                colour = continent))


# Convergence of European countries
pisa_col_diff <- function(df, stacked_var, vals_filter, value_var) {
  
  filter_dots <- lazyeval::interp(~ x %in% y, x = as.name(stacked_var), y = vals_filter)
  select_dots <- map(c("year", "country", "region", "continent", stacked_var, value_var), as.name)
  mutate_dots <- paste(c(as.name(vals_filter[1]), as.name(vals_filter[2])), collapse = " - ")
  
  df %>%
  filter_(.dots = filter_dots) %>%
  select_(.dots = select_dots) %>%
  spread_(stacked_var, value_var) %>%
  mutate_(diff = paste0("abs(", mutate_dots, ")")) %>%
  gather_(stacked_var, value_var, c(vals_filter, "diff"))
}

pisa_pv_differenced <-
  pisa_col_diff(pisa_pv_ci, "high_edu", c(1, 3), "Mean") %>%
  filter(high_edu == "diff")

pisa_quant_differenced <- 
  pisa_col_diff(pisa_quant_ci, "Percentiles", c(90, 10), "Score") %>%
  filter(Percentiles == "diff")

pisa_quant_differenced_eighty <- 
  pisa_col_diff(pisa_quant_ci, "Percentiles", c(80, 20), "Score") %>%
  filter(Percentiles == "diff")

dataset <- c("pisa_pv_differenced", "pisa_quant_differenced", "pisa_quant_differenced_eighty")
filter_vars <- c("high_edu", rep("Percentiles", 2))
vals <- c("Mean", rep("Score", 2))
dir <- "/Users/cimentadaj/Google Drive/PhD project/PISA/plots/"

map2(dataset, vals, function(data, value) {
    get(data) %>%
    filter(continent == "Europe") %>%
    ggplot(aes_string("year", value)) +
    geom_smooth(aes(group = country, colour = country), method = "lm",
                se = F) +
    ggtitle(data) +
    ggsave(paste("plot0_", data), path = dir, device = "png")
  

})

# Function to do non-standard filtering. In case needed
# dplyr_filter <- function(df, name, what_filter) {
#   df %>%
#     filter_(.dots = lazyeval::interp(~ y == x, y = as.name(name), x = what_filter))
# }

dplyr_mean <- function(df, new_name, x) {
  df %>%
    summarise_(.dots = setNames(list(lazyeval::interp(~ mean(x, na.rm = T), x  = x)), new_name))
}

# Lower variance for each year
# Lower difference in general
# Estimate by excluding the outliers
# in the model.

map(seq_along(dataset), function(index) {
  summarise_dots <- setNames(list(lazyeval::interp(~ sd(x, na.rm = T), x  = as.name(vals[index])),
                                  lazyeval::interp(~ mean(x, na.rm = T), x  = as.name(vals[index]))),
                             c("var", "mean_diff"))
  get(dataset[index]) %>%
    group_by(year) %>%
    summarise_(.dots = summarise_dots)
}) %>%
  setNames(dataset)

# Convergence of continents

map2(dataset, vals, function(data, values) {
  get(data) %>%
    group_by(year, continent) %>%
    dplyr_mean("diff", as.name(values)) %>%
    ggplot(aes(year, diff)) +
    geom_point(aes(colour = continent)) +
    geom_line(aes(group = continent, colour = continent)) +
    ggtitle(data) +
    ggsave(paste("plot1_", data), path = dir, device = "png")
})

map2(dataset, vals, function(data, values) {
  get(data) %>%
    filter(country %in% c("Denmark", "United States")) %>%
    ggplot(aes_string("year", values)) +
    geom_point(aes(colour = continent)) +
    geom_line(aes(group = continent, colour = continent)) +
    ggtitle(data) +
    ggsave(paste("plot2_", data), path = dir, device = "png")
})

tidy_datasets <-
  map2(dataset, vals, function(data, values) {
  get(data) %>%
    group_by(region, year) %>%
    dplyr_mean("values", as.name(values)) %>%
    spread(year, values)
}) %>%
  setNames(c("tidy_pv_data", "tidy_quant_data"))


list_pisa <-
  map(tidy_datasets, function(data) {
  map(seq_len(nrow(data)), function(data_rows) {
    as.numeric(data[data_rows, -1])
    })
  }) %>%
  map(function(list_data) {
  names(list_data) <- tidy_datasets[[1]]$region
  list_data
  })

list_pisa %>%
  map(function(data) {
    map(data, function(time) mean(diff(time) > 0, na.rm = T)) %>%
    Filter(function(inequality) !is.nan(inequality), .) %>%
    enframe() %>%
    unnest(value) %>%
    ggplot(aes(reorder(name, value), value)) +
      geom_point() +
      scale_y_continuous(limits = c(0, 1)) +
      coord_flip()
  })

map2(dataset, vals, function(data, values) {
  get(data) %>%
    ggplot(aes_string("year", values, colour = "country")) +
    geom_smooth(aes(group = region), method = "glm") +
    facet_wrap(~ region) +
    scale_colour_discrete(guide = F) +
    ggtitle(data) +
    ggsave(paste("plot3_", data), path = dir, device = "png")
  
})

map2(dataset, vals, function(data, values) {
  get(data) %>%
  ggplot(aes_string("year", values, colour = "country")) +
  geom_point(aes(colour = country), size = 0.6) +
  geom_line(aes(group = country)) +
  facet_wrap(~ region) +
  scale_colour_discrete(guide = F) +
  ggtitle(data) +
  ggsave(paste("plot4_", data), path = dir, device = "png")
  
})

map2(dataset, vals, function(data, values) {
  get(data) %>%
    filter(!is.na(continent)) %>%
    ggplot(aes_string("year", values)) +
    geom_smooth(aes(group = country), method = "glm") +
    facet_wrap(~ country) +
    ggtitle(data) +
    ggsave(paste("plot5_", data), path = dir, device = "png")
  
})

#TODO: You could cluster the patterns of countries
