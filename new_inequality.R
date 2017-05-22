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

db <- paste0("pisa", seq(2000, 2015, 3))
pisa_all2$value <- map2(pisa_all2$value, db, ~ { .x$wave <- .y; .x})
pisa_all2$value[[1]]$CNT <- pisa_all2$value[[1]]$COUNTRY

pisa_all2$value <- map(pisa_all2$value, ~ {
  .x$father_edu <- car::recode(as.numeric(.x$FISCED), "8:9 = NA")
  .x$mother_edu <- car::recode(as.numeric(.x$MISCED), "8:9 = NA")
  .x$high_edu_broad <- pmax(.x$father_edu, .x$mother_edu)
  .x$country <- pisa_countrynames[.x$CNT]

  if (any(unique(.x$wave) %in% c("pisa2012", "pisa2015"))) {
    .x$father_edu <- .x$father_edu + 1
    .x$mother_edu <- .x$mother_edu + 1
    .x$high_edu_broad <- .x$high_edu_broad + 1
  }
  .x
})

pisa_all2$value[[1]] <- left_join(pisa_all2$value[[1]], escs2000,
                                  by = c("SCHOOLID", "STIDSTD", "SUBNATIO"))

pisa_all2$value[[1]]$CNT <- pisa_all2$value[[1]]$CNT.x
pisa_all2$value[[1]]$CNT.y <- NULL
pisa_all2$value[[1]]$CNT.x <- NULL

results <-
  map(pisa_all2$value, ~ {
  vals <- range(.x$high_edu_broad, na.rm = T)
  .x$edu_dummy <- car::recode(.x$high_edu_broad, paste0(vals[2], " = 1; ", vals[1], " = 0", "; else = NA"))

  conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
  
  results <- intsvy.mean.pv(pvnames = paste0("PV", seq_len(conf$parameters$PVreps), "MATH"),
                            by = c("country", "edu_dummy"),
                            data = .x,
                            config = conf)
  message(paste0(unique(.x$wave), " done"))
  results
})

years <- seq(2000, 2015, 3)

results_summary <-
  map(seq_along(results), ~ {
  results[[.x]] %>%
    select(country, edu_dummy, Mean) %>%
    spread(key = edu_dummy, value = Mean) %>%
    mutate(diff = `1` - `0`,
           diff_na = `1` - `<NA>`,
           wave = years[.x]) %>%
    gather(category, value, diff, diff_na) %>%
    select(country, category, value, wave)
}) %>%
  reduce(rbind)

dat <- 
  results_summary %>%
  mutate(continent = countrycode(country, origin = "country.name", destination = "continent"),
         region = countrycode(country, origin = "country.name", destination = "region")) %>%
  filter(!is.na(continent)) %>%
  group_by(wave, country, category) %>%
  summarise(value = mean(value, na.rm = T))

dat %>%
  ggplot(aes(x = wave, y = value, colour = category, group = category)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  facet_wrap(~ country)

as_tibble(dat) %>%
  filter(category == "diff") %>%
  split(.$country) %>%
  Filter(function(data) nrow(data) >= 3, .) %>%
  map(~ broom::tidy(lm(value ~ wave, data = .,
                       weights = .[[pisa_conf$variables$weightFinal]]))) %>%
  enframe() %>%
  unnest(value) %>%
  filter(term == "wave") %>%
  mutate(lower = estimate - std.error * 1.96,
         upper = estimate + std.error * 1.96) %>%
  ggplot(aes(reorder(name, estimate), estimate)) +
  geom_col(aes(colour = p.value < 0.1, alpha = 0.2)) +
  geom_errorbar(aes(ymin = lower, ymax = upper)) +
  coord_flip()

results_gender <-
  map(pisa_all2$value, ~ {
    .x$country <- pisa_countrynames[.x$CNT]
    vals <- range(.x$high_edu_broad, na.rm = T)
    
    .x$edu_dummy <-
      car::recode(.x$high_edu_broad,
                  paste0(vals[2], " = 1; ", "c(", vals[1], ",", vals[1] + 1, ")", " = 0", "; else = NA"))
    
    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    
    results <- intsvy.mean.pv(pvnames = paste0("PV", seq_len(conf$parameters$PVreps), "MATH"),
                              by = c("country", "edu_dummy", "gender"),
                              data = .x,
                              config = conf)
    message(paste0(unique(.x$wave), " done"))
    results
  })

for (i in 5:6) {
  results_gender[[i]]$gender <- car::recode(results_gender[[i]]$gender, "1 = 'Male'; 2 = 'Female'")
}

datasets_for_viz <-
  map(results_gender, ~ {
  data_for_ci <-
    as_tibble(.x) %>%
    filter(!is.na(edu_dummy) & !is.na(gender)) %>%
    unite(edu_gender, edu_dummy, gender)
  
  se_data <-
    data_for_ci %>%
    select(country, edu_gender, s.e.) %>%
    spread(edu_gender, s.e.) %>%
    mutate(se_female = sqrt(`1_Female`^2 + `0_Female`^2),
           se_male = sqrt(`1_Male`^2 + `0_Male`^2)) %>%
    select(country, starts_with("se"))
  
  data_for_ci %>%
    select(country, edu_gender, Mean) %>%
    spread(edu_gender, Mean) %>%
    mutate(diff_female = `1_Female` - `0_Female`,
           diff_male = `1_Male` - `0_Male`) %>%
    select(country, starts_with("diff")) %>%
    left_join(se_data) %>%
    mutate(upper_female = diff_female + 2 * se_female,
           lower_female = diff_female - 2 * se_female,
           upper_male = diff_male + 2 * se_male,
           lower_male = diff_male - 2 * se_male)
})

setNames(datasets_for_viz, years) %>%
  enframe() %>%
  unnest(value) %>%
  gather(category, value, -name, -country) %>%
  separate(category, c("category", "gender")) %>%
  spread(category, value) %>%
  filter(country == "Italy") %>%
  ggplot(aes(name, diff, colour = gender)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = gender), alpha = 0.3) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)
  geom_smooth(aes(group = gender), method = "lm", se = F) +
  theme_minimal() +
  ylim(c(0, 200)) +
  facet_wrap(~ country)
  

results_gender <-
  map(pisa_all2$value, ~ {
    .x$country <- pisa_countrynames[.x$CNT]
    vals <- range(.x$high_edu_broad, na.rm = T)
    .x$edu_dummy <- car::recode(.x$high_edu_broad, paste0(vals[2], " = 1; ", "c(", vals[1], ",", vals[1] + 1, ")", " = 0", "; else = NA"))
    
    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    
    results <- intsvy.mean.pv(pvnames = paste0("PV", seq_len(conf$parameters$PVreps), "MATH"),
                              by = c("country", "gender"),
                              data = .x,
                              config = conf)
    message(paste0(unique(.x$wave), " done"))
    results
  })

for (i in 5:6) {
  results_gender[[i]]$gender <- car::recode(results_gender[[i]]$gender, "1 = 'Male'; 2 = 'Female'")
}

datasets_for_viz <-
  map(results_gender, ~ {
    data_for_ci <-
      as_tibble(.x) %>%
      filter(!is.na(gender))
  })

setNames(datasets_for_viz, years) %>%
  enframe() %>%
  unnest(value) %>%
  filter(country == "United States") %>%
  ggplot(aes(name, Mean, colour = gender)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = gender), alpha = 0.3) +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25)
  geom_smooth(aes(group = gender), method = "lm", se = F) +
  theme_minimal() +
  facet_wrap(~ country)

map(2:6, ~ {
  pisa_all2$value[[.x]] %>%
  group_by(CNT) %>%
  summarise(cor = cor(HOMEPOS, PV1MATH, use = "pairwise.complete.obs"))
  }) %>%
  setNames(years[-1]) %>%
  enframe() %>%
  unnest(value) %>%
  mutate(CNT = pisa_countrynames[CNT],
         continent = countrycode(CNT, "country.name", "continent")) %>%
  filter(!is.na(continent) & continent == "Europe") %>%
  ggplot(aes(reorder(CNT, cor), name, fill = cor)) +
  geom_tile() + coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"))

pisa_all2$value[[4]] %>%
  mutate(edu_recode = car::recode(high_edu_broad, "c(1, 2) = 0; 7 = 1; else = NA")) %>%
  filter(!is.na(edu_recode)) %>%
  ggplot(aes(PV1MATH, colour = as.factor(edu_recode))) +
  geom_freqpoly(alpha = 0.7) +
  geom_freqpoly(data = pisa_all2$value[[4]], aes(x = PV1MATH), colour = "blue")
  
pisa_all2$value[[4]] %>%
  mutate(edu_recode = car::recode(high_edu_broad, "c(1, 2) = 0; 7 = 1; else = NA")) %>%
  filter(!is.na(edu_recode)) %>%
  ggplot(aes(ESCS, colour = as.factor(edu_recode))) +
  geom_freqpoly(alpha = 0.7) +
  geom_freqpoly(data = pisa_all2$value[[4]], aes(x = ESCS), colour = "blue")


mod1 <- lm(as.formula(paste("ESCS ~", paste0("PV", 1:5, "MATH", collapse = " + "))), 
              data = pisa_all2$value[[2]],
              weights = pisa_all2$value[[2]][[pisa_conf$variables$weightFinal]],
           na.action = na.exclude)
pisa_all2$value[[2]]$ESCS_two <- predict(mod1)

### Try all different datasets
data_pisa <- pisa_all2$value[[5]]
quan <-
  Hmisc::wtd.quantile(data_pisa$ESCS,
                      weights = data_pisa[[pisa_conf$variables$weightFinal]],
                      probs = c(0.10, 0.90))

data_pisa %>%
  mutate(edu_recode = car::recode(high_edu_broad, "c(1, 2) = 0; 7 = 1; else = NA")) %>%
  filter(!is.na(edu_recode) & ESCS < 750) %>%
  ggplot(aes(ESCS, colour = as.factor(edu_recode))) +
  geom_freqpoly(alpha = 0.7) +
  geom_freqpoly(data = filter(data_pisa, ESCS < 750), aes(x = ESCS), colour = "blue") +
  geom_vline(xintercept = quan[1], colour = "red") +
  geom_vline(xintercept = quan[2], colour = "red")

###

results_ses <-
  map(pisa_all2$value, ~ {
    quan <- Hmisc::wtd.quantile(.x$ESCS,
                          weights = .x[[pisa_conf$variables$weightFinal]],
                          probs = c(0.10, 0.90))
    .x$ESCS_dummy <-
      ifelse(.x$ESCS >= quan[2] & .x$ESCS <= 750, 1,
      ifelse(.x$ESCS <= quan[1], 0, NA))
    
    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    
    results <- intsvy.mean.pv(pvnames = paste0("PV", seq_len(conf$parameters$PVreps), "MATH"),
                              by = c("country", "ESCS_dummy"),
                              data = .x,
                              config = conf)
    message(paste0(unique(.x$wave), " done"))
    results
  })

datasets_for_viz <-
  map(results_ses, ~ {
    data_for_ci <-
      as_tibble(.x) %>%
      filter(!is.na(ESCS_dummy))

    se_data <-
      data_for_ci %>%
      select(country, ESCS_dummy, s.e.) %>%
      spread(ESCS_dummy, s.e.) %>%
      mutate(se_diff = sqrt(`1`^2 + `0`^2)) %>%
      select(country, starts_with("se"))
    
    data_for_ci %>%
      select(country, ESCS_dummy, Mean) %>%
      spread(ESCS_dummy, Mean) %>%
      mutate(diff_escs = `1` - `0`) %>%
      select(country, starts_with("diff")) %>%
      left_join(se_data) %>%
      mutate(upper_bound = diff_escs + 2 * se_diff,
             lower_bound = diff_escs - 2 * se_diff)
  })

dat <-
  setNames(datasets_for_viz, years) %>%
  enframe() %>%
  unnest(value) %>%
  mutate(continent = countrycode(country, "country.name", "continent"),
         region = countrycode(country, "country.name", "region"))

countries <- c("Germany", "United States", "Denmark", "Sweden", "United Kingdom",
               "Spain", "Italy", "Canada", "Australia")
dat %>%
  filter(country %in% countries) %>%
  ggplot(aes(name, diff_escs)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = country), alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, alpha = 0.5) +
  geom_smooth(aes(group = country), method = "lm") +
  theme_minimal() +
  facet_wrap(~ country)

m_two <-
  dat %>%
  select(country, name, diff_escs) %>%
  spread(name, diff_escs) %>%
  by_row(function(row) mean(is.na(row)), .collate = "cols") %>%
  filter(.out < 0.5)

# Get the row means
m <-
  m_two %>%
  select(-country, -.out) %>%
  rowMeans(na.rm = T)

index <- which(is.na(m_two))
means_rep <- rep(m, ncol(m_two))

df_matrix <- as.matrix(m_two)
df_matrix[index] <- means_rep[index]


cluster_matrix <-
  df_matrix %>%
  as_tibble() %>%
  mutate_at(vars(`2000`:`2015`), function(value) round(as.numeric(value), 2)) %>%
  select(starts_with("2")) %>%
  filter(complete.cases(.)) %>%
  as.matrix()

clusters <-
  cluster_matrix %>%
  kmeans(centers = 4, nstart = 30)

m_three <-
  m_two %>%
  select(-.out) %>%
  mutate(clust = clusters$cluster) %>%
  gather(name, value, -country, -clust)

m_three %>%
  ggplot(aes(reorder(name, clust), value, group = country, colour = as.factor(clust))) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = country), alpha = 0.3) +
  # geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, alpha = 0.5) +
  geom_smooth(aes(group = country), method = "lm", se = F, alpha = 0.3) +
  ylim(0, 300) +
  theme_minimal() +
  facet_wrap(~ clust)

m_three %>%
  ggplot(aes(x = reorder(country, -clust), y = clust, fill = as.factor(clust))) +
  geom_tile() +
  coord_flip()

hc.complete = hclust ( dist(cluster_matrix) , method ="complete")
plot(hc.complete, main =" Complete Linkage ", xlab ="" , sub ="" ,
       cex =.9, labels = m_two$country)

m_three$clust_hier <- cutree(hc.complete, 4)

# The KNN has a more coherent clustering
m_three %>%
  ggplot(aes(reorder(name, clust_hier), value, group = country, colour = as.factor(clust_hier))) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = country), alpha = 0.3) +
  # geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, alpha = 0.5) +
  geom_smooth(aes(group = country), method = "lm", se = F, alpha = 0.3) +
  ylim(0, 300) +
  theme_minimal() +
  facet_wrap(~ clust_hier)

# Average coef is negative
dat %>%
  filter(category == "diff") %>%
  {mod2 <<- lm(value ~ wave, data = ., weights = .[[pisa_conf$variables$weightFinal]])} %>%
  sim(n.sims = 1000) %>%
  coef %>%
  colMeans

dat %>%
  filter(category == "diff") %>%
  {mod2 <<- lm(value ~ wave, data = ., weights = .[[pisa_conf$variables$weightFinal]])} %>%
  sim(n.sims = 1000) %>%
  sigma.hat %>%
  mean # very similar to the model sigma, not so uncertain

# All negative, so the uncertainty is small
dat %>%
  filter(category == "diff") %>%
  {mod2 <<- lm(value ~ wave, data = ., weights = .[[pisa_conf$variables$weightFinal]])} %>%
  sim(n.sims = 1000) %>%
  coef %>%
  apply(2, function(x) quantile(x, probs = c(0.1, 0.5, 0.9)))

# Allowing the wave coef to vary by country is just too hard on the model
dat %>%
  filter(category == "diff") %>%
  {mod2 <<- lmer(value ~ (1 | country), data = ., weights = .[[pisa_conf$variables$weightFinal]])} %>%
  sim(n.sims = 1000) %>%
  fixef() %>%
  colMeans()

pooled_countries <-
  map(pisa_all2$value, ~ {
  quan <- Hmisc::wtd.quantile(.x$ESCS,
                              weights = .x[[pisa_conf$variables$weightFinal]],
                              probs = c(0.10, 0.90))
  .x$ESCS_dummy <-
    with(.x, case_when(ESCS >= quan[2] & ESCS <= 750 ~ 1,
                       ESCS <= quan[1] ~ 0))
  .x %>%
    select(wave, matches("^PV[1-5]MATH$"), ESCS_dummy, country)
  }) %>%
  bind_rows()

list_formulas <- formulas(~ PV1MATH,
  ~ (1 | country),
  ~ ESCS_dummy + (1  | country),
  ~ ESCS_dummy + (1 + ESCS_dummy | country),
  ~ ESCS_dummy + wave + (1 + ESCS_dummy | country),
  ~ ESCS_dummy + wave + (1 + ESCS_dummy + wave | country)
  ) %>%
  setNames(paste0("mod", 1:length(list_formulas)))

pooled_models <-
  pooled_countries %>%
  separate(wave, c("survey", "wave"), sep = 4) %>%
  mutate(wave = as.numeric(wave)) %>%
  fit_with(lmer, list_formulas)

# Tracking indi

temp_dir <- tempdir()
file.remove(file.path(temp_dir, list.files(temp_dir))) %>%
  invisible()

file_name <- file.path(temp_dir, "tracking.zip")
download.file("http://thijsbol.com/wp-content/uploads/2015/09/educsysdata-v4.zip",
              destfile = file_name)

unzip(file_name, exdir = temp_dir)

tracking_name <- list.files(temp_dir, pattern = "bw-.*\\-full-.*\\.dta")

tracking <-
  read_dta(file.path(temp_dir, tracking_name)) %>%
  select(-bwid, -cntry)

inequality_data

m_three %>%
  rename(score = value) %>%
  left_join(inequality_data, by = c("country", "name" = "year")) %>%
  filter(indicators == "P90P50") %>%
  lmer(score ~ 1 + name + value + (1 | country), data = .) %>%
  display()

results_ses_exp <-
  map(pisa_all2$value, ~ {
    quan <- Hmisc::wtd.quantile(.x$ESCS,
                                weights = .x[[pisa_conf$variables$weightFinal]],
                                probs = c(0.10, 0.90))
    .x$ESCS_dummy <-
      ifelse(.x$ESCS >= quan[2] & .x$ESCS <= 750, 1,
             ifelse(.x$ESCS <= quan[1], 0, NA))
    
    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    
    results <- intsvy.mean.pv(pvnames = paste0("PV", seq_len(conf$parameters$PVreps), "MATH"),
                              by = c("country", "ESCS_dummy"),
                              data = .x,
                              config = conf)
    message(paste0(unique(.x$wave), " done"))
    results
  })

datasets_for_viz <-
  map(results_ses_exp, ~ {
    data_for_ci <-
      as_tibble(.x) %>%
      filter(!is.na(ESCS_dummy))
    
    se_data <-
      data_for_ci %>%
      select(country, ESCS_dummy, s.e.) %>%
      spread(ESCS_dummy, s.e.) %>%
      mutate(se_diff = sqrt(`1`^2 + `0`^2)) %>%
      select(country, starts_with("se"))
    
    data_for_ci %>%
      select(country, ESCS_dummy, Mean) %>%
      spread(ESCS_dummy, Mean) %>%
      mutate(diff_escs = `1` - `0`) %>%
      select(country, starts_with("diff")) %>%
      left_join(se_data) %>%
      mutate(upper_bound = diff_escs + 2 * se_diff,
             lower_bound = diff_escs - 2 * se_diff)
  })

dat <-
  setNames(datasets_for_viz, years) %>%
  enframe() %>%
  unnest(value) %>%
  mutate(continent = countrycode(country, "country.name", "continent"),
         region = countrycode(country, "country.name", "region"))

countries <- c("Germany", "United States", "Denmark", "Sweden", "United Kingdom",
               "Spain", "Italy", "Canada", "Australia")
dat %>%
  filter(country %in% countries) %>%
  ggplot(aes(name, diff_escs)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(group = country), alpha = 0.3) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), width = 0.1, alpha = 0.5) +
  geom_smooth(aes(group = country), method = "lm") +
  theme_minimal() +
  facet_wrap(~ country)

dat %>%
  filter(name == "2009", country %in% countries) %>%
  ggplot(aes(reorder(country, -diff_escs), diff_escs)) +
  geom_col() +
  coord_flip()
  

m_two <-
  dat %>%
  select(country, name, diff_escs) %>%
  spread(name, diff_escs) %>%
  by_row(function(row) mean(is.na(row)), .collate = "cols") %>%
  filter(.out < 0.5)