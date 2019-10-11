read_harmonize_pisa <- function(raw_data, recode_cntrys) {

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

  # Create a data frame with a list column containing all lists.
  pisa_all <- enframe(pisa_list)

  print("pisa_all ready")

  # Remove everything that uses memory
  rm(pisa_list,
     dic_student,
     student2012,
     student2015)

  unloadNamespace("PISA2000lite")
  unloadNamespace("PISA2003lite")
  unloadNamespace("PISA2006lite")
  unloadNamespace("PISA2009lite")
  print("PISA packages unloaded")

  # Go through each PISA and grab the highest education from Male
  # and Female partner. Finally, recode it to three categories
  pisa_all$value <- map(pisa_all$value, function(each_pisa) {
    each_pisa$high_edu <- pmax(as.numeric(each_pisa$MISCED), # find highest education
                               as.numeric(each_pisa$FISCED)) # in the household
    
    # Recode new highest education into three categories
    each_pisa$high_edu <- recode(each_pisa$high_edu, "1:3 = 1; 4:5 = 2; 6:7 = 3; else = NA")
    each_pisa
  })

  # Gender variables:
  # 2000: ST03Q01
  # 2003: ST03Q01
  # 2006: ST04Q01
  # 2009: ST04Q01
  # 2012: ST04Q01
  # 2015: ST004D01T

  # Vector with all gender vars in order of PISA surveys
  gender <- c("ST03Q01",
              "ST03Q01",
              "ST04Q01",
              "ST04Q01",
              "ST04Q01",
              "ST004D01T")

  # Loop through each data frame and assign the gender variable name
  # and turn it into character
  pisa_all$value <- map(seq_along(pisa_all$value), function(pisa_data) {
    
    pisa_all$value[[pisa_data]] %>%
      rename_("gender" = gender[pisa_data]) %>%
      mutate(gender = as.character(gender),
             country = pisa_countrynames[as.character(CNT)],
             region = countrycode(country, "country.name", "region")) %>%
      as_tibble()
  })

  years <- seq(2000, 2015, 3)

  db <- paste0("pisa", years)
  pisa_all$value <- map2(pisa_all$value, db, ~ { .x$wave <- .y; .x})
  pisa_all$value[[1]]$CNT <- pisa_all$value[[1]]$COUNTRY

  print("Before going into valueeee")
  pisa_all$value <-
    map(pisa_all$value, ~ {
      # 2000 to 2015
      # The coding is from 0 to 6, where 0 is no schooling and 6 is
      # BA or above.

      # When turning 0:6 to numeric, it becomes 1:7 that's why
      # I recode 8:9 to NA. This, however, did not work for last two surveys

      .x$father_edu <- car::recode(as.numeric(.x$FISCED), "8:9 = NA")
      .x$mother_edu <- car::recode(as.numeric(.x$MISCED), "8:9 = NA")
      .x$high_edu_broad <- pmax(.x$father_edu, .x$mother_edu)
      .x$country <- car::recode(.x$CNT, recode_cntrys)


      if (any(unique(.x$wave) %in% c("pisa2012", "pisa2015"))) {
        # These two surveys were from 0:6 so I had to add + 1
        # so that it equals 1:7 as all other surveys.
        .x$father_edu <- .x$father_edu + 1
        .x$mother_edu <- .x$mother_edu + 1
        .x$high_edu_broad <- .x$high_edu_broad + 1
      }

      if (unique(.x$wave) == "pisa2015") {

        # PISA 2015 doesn't have the schoolid column at the moment.
        select(.x,
               wave,
               country,
               AGE,
               matches("PV[0-9]{1,2}[MATH|READ]"),
               W_FSTUWT,
               ESCS,
               high_edu_broad,
               gender,
               PROGN,
               ISCEDO,
               matches("ST16Q03|ST37Q01|ST019CQ01T|ST013Q01TA"))

      } else if (unique(.x$wave) == "pisa2000") {

        select(.x,
               wave,
               country,
               SCHOOLID,
               STIDSTD,
               AGE,
               matches("PV[0-9]{1,2}[MATH|READ]"),
               W_FSTUWT,
               high_edu_broad,
               gender,
               # Excludes PROGN because it wasn't asked in PISA 2000
               matches("ST16Q03|ST37Q01|ST019CQ01T|ST013Q01TA"))
      } else {
        
        select(.x,
               wave,
               country,
               SCHOOLID,
               STIDSTD,
               AGE,
               matches("PV[0-9]{1,2}[MATH|READ]"),
               W_FSTUWT,
               high_edu_broad,
               gender,
               PROGN,
               ISCEDO,
               matches("ST16Q03|ST37Q01|ST019CQ01T|ST013Q01TA"))

      }

    })


  pisa_all

}

read_harmonize_pisa_school <- function(raw_data, recode_cntrys) {

  # To be used when > PISA2006 waves divide autonomy tick
  # into several columns rather than pasted into one.
  # This takes the N (4, 5) columns and collapse them into
  # one string summarizing the non-autonomy columns into one
  pmax_narm <- function(...) {
    pmax(... = ..., na.rm = TRUE)
  }

  collapse_aut_cols <- function(df_pisa, list_aut, max_cols = 4) {
    aut_cols <- seq(1, max_cols)
    # Always last two columns
    non_aut_cols <- aut_cols[(length(aut_cols) - 1):length(aut_cols)]

    for (i in seq_along(list_aut)) {
      tst <- df_pisa[, list_aut[[i]]]
      tst <- mutate_all(tst, ~ recode(as.numeric(.x), `7` = NA_real_, `8` = NA_real_))
      tst$higher_aut <- do.call(pmax_narm, tst[non_aut_cols])
      original_var <- paste0(names(list_aut)[i], "_original")
      new_var <- names(list_aut)[i]
      new_old_cols <- c(max_cols + 1, setdiff(aut_cols, non_aut_cols))
      df_pisa[[original_var]] <- apply(tst[, new_old_cols], 1, paste0, collapse = "")
      df_pisa[[new_var]] <- df_pisa[[original_var]]
    }

    df_pisa
  }


  identify_autonomy <- function(x) {
    autonomy_test <- ifelse("1" == str_sub(x, end = 1), 0, 1)
    missing_codes <- c("99999",
                       "99997",
                       "88888",
                       "00000",
                       "000",
                       "NANANA",
                       "77777",
                       "N    ",
                       "M    ",
                       "I    ")

    autonomy_test[x %in% missing_codes] <- NA

    autonomy_test
  }

  # Commong variables to pick for all waves
  var_picker <- function(df_pisa) {
    df_pisa %>%
      select(COUNTRY, SCHOOLID, ends_with("_aut"), "SCHLTYPE")
  }
  
  # PISA 20000
  school2000 <-
    school2000 %>%
    as_tibble() %>%
    rename(course_aut = SC22Q12,
           content_aut = SC22Q11,
           textbook_aut = SC22Q10,
           hiring_aut = SC22Q01,
           salary_aut = SC22Q03,
           budget_aut = SC22Q06,
           admittance_aut = SC22Q09) %>%
    mutate_at(vars(ends_with("aut")), identify_autonomy) %>%
    mutate(COUNTRY = tools::toTitleCase(tolower(COUNTRY))) %>%
    var_picker()

  # PISA 2003
  school2003 <-
    school2003 %>%
    as_tibble() %>% 
    rename(course_aut = SC26Q12,
           content_aut = SC26Q11,
           textbook_aut = SC26Q10,
           hiring_aut = SC26Q01,
           salary_aut = SC26Q03,
           budget_aut = SC26Q06,
           admittance_aut = SC26Q09) %>%
    mutate_at(vars(ends_with("aut")), str_replace_all, "2", "0") %>%
    mutate_at(vars(ends_with("aut")), identify_autonomy) %>%
    mutate(COUNTRY = trimws(COUNTRY)) %>%
    var_picker()

  # PISA 2006
  list_aut <- list("course_aut" = paste0("SC11QL", 1:4),
                   "content_aut" = paste0("SC11QK", 1:4),
                   "textbook_aut" = paste0("SC11QJ", 1:4),
                   "hiring_aut" = paste0("SC11QA", 1:4),
                   "salary_aut" = paste0("SC11QC", 1:4),
                   "budget_aut" = paste0("SC11QF", 1:4),
                   "admittance_aut" = paste0("SC11QI", 1:4))

  school2006 <-
    as_tibble(school2006) %>%
    mutate_at(unlist(list_aut, use.names = FALSE), ~ recode(.x, `2` = 0))

  school2006 <- collapse_aut_cols(school2006, list_aut)
  school2006 <-
    mutate_at(school2006, vars(ends_with("aut")), identify_autonomy) %>%
    mutate(COUNTRY = as.character(COUNTRY)) %>%
    var_picker()

  # PISA 2009
  list_aut <- list("course_aut" = paste0("SC24QL", 1:5),
                   "content_aut" = paste0("SC24QK", 1:5),
                   "textbook_aut" = paste0("SC24QJ", 1:5),
                   "hiring_aut" = paste0("SC24QA", 1:5),
                   "salary_aut" = paste0("SC24QC", 1:5),
                   "budget_aut" = paste0("SC24QF", 1:5),
                   "admittance_aut" = paste0("SC24QI", 1:5))

  school2009 <-
    as_tibble(school2009) %>%
    mutate_at(unlist(list_aut, use.names = FALSE), ~ recode(as.numeric(.x), `2` = 0))

  school2009 <- collapse_aut_cols(school2009, list_aut, max_cols = 5)
  school2009 <-
    mutate_at(school2009, vars(ends_with("aut")), identify_autonomy) %>%
    mutate(CNT = as.character(CNT)) %>%
    select(-COUNTRY) %>% 
    rename(COUNTRY = CNT,
           SCHLTYPE = SCHTYPE) %>%
    var_picker()

  # PISA 2012
  pisa2012_path <- file.path(raw_data, "pisa2012", "INT_SCQ12_DEC03.txt")
  dic_path <- file.path(raw_data, "pisa2012", "PISA2012_SAS_school.sas")
  dic_school <- parse.SAScii(sas_ri = dic_path)
  school2012 <- read_fwf(pisa2012_path,
                         col_positions = fwf_widths(dic_school$width),
                         progress = TRUE)

  colnames(school2012) <- dic_school$varname

  up_letters <- toupper(letters[1:5])

  list_aut <- list("course_aut" = paste0("SC33Q12", up_letters),
                   "content_aut" = paste0("SC33Q11", up_letters),
                   "textbook_aut" = paste0("SC33Q10", up_letters),
                   "hiring_aut" = paste0("SC33Q01", up_letters),
                   "salary_aut" = paste0("SC33Q03", up_letters),
                   "budget_aut" = paste0("SC33Q06", up_letters),
                   "admittance_aut" = paste0("SC33Q09", up_letters))

  school2012 <-
    school2012 %>%
    mutate_at(unlist(list_aut, use.names = FALSE), ~ recode(as.numeric(.x), `2` = 0))

  school2012 <- collapse_aut_cols(school2012, list_aut, max_cols = 5)

  school2012 <-
    mutate_at(school2012, vars(ends_with("aut")), identify_autonomy) %>%
    mutate(CNT = cimentadaj::pisa_countrynames[CNT]) %>%
    rename(COUNTRY = CNT) %>%
    var_picker()

  # PISA 2015
  pisa2015_path <- file.path(raw_data, "pisa2015", "CY6_MS_CMB_SCH_QQQ.sav")
  pisa_countrynames <- c(cimentadaj::pisa_countrynames, "United States" = "USA")
  school2015 <- read_spss(pisa2015_path)

  up_letters <- toupper(letters[1:5])

  list_aut <- list("course_aut" = paste0("SC010Q12T", up_letters),
                   "content_aut" = paste0("SC010Q11T", up_letters),
                   "textbook_aut" = paste0("SC010Q10T", up_letters),
                   "hiring_aut" = paste0("SC010Q01T", up_letters),
                   "salary_aut" = paste0("SC010Q03T", up_letters),
                   "budget_aut" = paste0("SC010Q06T", up_letters),
                   "admittance_aut" = paste0("SC010Q09T", up_letters))

  school2015 <- collapse_aut_cols(school2015, list_aut, max_cols = 5)
  school2015 <- mutate_at(school2015, vars(ends_with("aut")), identify_autonomy)

  sum_sc2015 <-
    school2015 %>%
    mutate(CNT = cimentadaj::pisa_countrynames[CNT]) %>%
    rename(COUNTRY = CNT) %>% 
    group_by(COUNTRY) %>%
    summarize_at(vars(ends_with("_aut")), mean, na.rm = TRUE) %>% 
    mutate(wave = "pisa2015") %>% 
    pivot_longer(ends_with("aut"),
                 names_to = "aut",
                 values_to = "vals")

  # Merge all and plot
  sum_sc <- bind_rows(sum_sc2000,
                      sum_sc2003,
                      sum_sc2006,
                      sum_sc2009,
                      sum_sc2012,
                      sum_sc2015)

  sum_sc %>%
    mutate(wave = factor(wave, levels = paste0("pisa", seq(2000, 2015, by = 3)), ordered = TRUE)) %>%
    filter(COUNTRY %in% recode_cntrys) %>% 
    ggplot(aes(wave, vals, group = aut, linetype = aut, color = aut)) +
    geom_point() +
    geom_line() +
    ## scale_color_manual(values = c("black", "grey60")) +
    facet_wrap(~ COUNTRY)
}

read_escs <- function(raw_data, recode_cntrys) {
  escs_path <- file.path(raw_data, "escs_data")
  escs_files <- list.files(escs_path, pattern = ".sav", full.names = TRUE)
  escs_trend <- map(escs_files, haven::read_spss)

  escs_trend <-
    map(escs_trend, ~ {
    .x$country <- car::recode(.x$cnt, recode_cntrys)
    .x
  })

  escs_trend
}

read_tracking <- function(raw_data) {
  tracking_data <-
    read_xlsx(file.path(raw_data, "tracking/tracking.xlsx"), sheet = "all_data") %>%
    map_if(is_double, round, 2) %>%
    as_tibble()

  tracking_data
}


merge_data <- function(pisa_all, escs_trend) {
  pisa_all <- as_tibble(pisa_all)
  # Next we'll merge the ESCS data with the PISA data. As explained above, the 6th data (PISA
  # 2015) does not need to be merged so I exclude it with this vector
  exclude <- -6
  years <- seq(2000, 2015, 3)

  # Loop in parallel to the PISA data, the ESCS data and the year vector (which is seq(2012, 2015, 3))
  pisa_all$value[exclude] <-
    pmap(list(pisa_all$value[exclude], escs_trend, years[exclude]), function(.x, .y, .z) {

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

  pisa_all$value[[6]] <-
    pisa_all$value[[6]] %>%
    rename(escs_trend = ESCS)

  adapted_year_data <-
    map(pisa_all$value, ~ {
      if (unique(.x$wave) == "pisa2000") {
        # pisa2000 has a different coding so here I recode 6 to 7 so that in all
        # waves the top edu is 7 and the bottom is 1
        .x <-
          mutate(.x, new_hisced = as.character(dplyr::recode(as.numeric(high_edu_broad), `6` = 7)))
      } else {
        .x <-
          mutate(.x, new_hisced = as.character(high_edu_broad))
      }

      .x
    })

  adapted_year_data
}

escs_dummy_creator <- function(df, probs) {

  map(df, function(.x) {

    conf <- if (unique(.x$wave) == "pisa2015") pisa2015_conf else pisa_conf
    weights_var <- conf$variables$weightFinal

    # Harmonize education variable
    if (unique(.x$wave) == "pisa2000") {
      .x$high_edu_broad <- dplyr::recode(.x$high_edu_broad, `6` = 7)
    }

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


sample_size_calc <- function(df, probs, selected = FALSE, cnts = NULL) {

  stopifnot(selected & !is.null(cnts))

  if (selected) df <- map(df, ~ filter(.x, country %in% cnts))

  cnt_to_bind <-
    map(df, function(df) {

      print(unique(df$wave))
      conf <- if (unique(df$wave) == "pisa2015") pisa2015_conf else pisa_conf
      weights_var <- conf$variables$weightFinal

      split_df <- split(df, df$country)

      split_df_two <-
        map(split_df, ~ {
          # In some countries the quan cannot be estimated because of very few obs.
          # The function does not stop but returns two NA's.
          quan <- quantile_missing(.x, weights_var, probs)

          # it is very important to create a variable that returns the number of observations of this dummy
          # For each country. Possibly to weight by the number of observations.
          .x$escs_dummy <-
            with(.x, case_when(escs_trend >= quan[2] ~ 1,
                               escs_trend <= quan[1] ~ 0))
          .x
        })

      unsplit_df <- split_df_two %>% enframe() %>% unnest(value)

      unsplit_df %>%
        dplyr::count(country, escs_dummy) %>%
        filter(!is.na(escs_dummy)) %>%
        left_join(summarize(group_by(unsplit_df, country), total_n = n()), by = "country") %>%
        mutate(perc = paste0(round(n / total_n * 100, 0), "%")) %>%
        dplyr::select(-total_n)
      
    })

  setNames(cnt_to_bind, seq(2000, 2015, 3)) %>%
    map(~ {.x$country <- as.character(.x$country); .x}) %>% 
    enframe() %>%
    unnest()
}

# Function does a lot of things, but in short:

# Calculate the difference between the gap and together with it's joint s.e
# Also uncertainty intervals and returns a tibble with the difference between
# SES gaps with the adjusted SE difference + uncertainty intervals + the original
# data (the absolute numbers before the differences)

pisa_preparer <- function(df_math, df_read, type_txt) {
  years <- seq(2000, 2015, 3)

  descrip_math <- map(df_math, ~ rename(.x, mean_math = Mean, se_math = s.e.))
  descrip_read <- map(df_read, ~ rename(.x, mean_read = Mean, se_read = s.e.))


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

  # Merging math and reading data
  test_data <-
    reduced_data %>%
    dplyr::select(country, wave, escs_dummy, contains("mean")) %>%
    gather(test, score, contains("mean"))

  math_data <-
    reduced_data %>%
    dplyr::select(country, wave, escs_dummy, contains("math")) %>%
    gather(test_bound, bound, contains("lower"), contains("upper")) %>%
    dplyr::select(-contains("math")) %>%
    right_join(filter(test_data, test == "mean_math"))

  read_data <-
    reduced_data %>%
    dplyr::select(country, wave, escs_dummy, contains("read")) %>%
    gather(test_bound, bound, contains("lower"), contains("upper")) %>%
    dplyr::select(-contains("read")) %>%
    right_join(filter(test_data, test == "mean_read"))

  all_data <- bind_rows(math_data, read_data)

  # Calculate the joint standard error of the difference
  math_se_data <-
    reduced_data %>%
    dplyr::select(country, escs_dummy, wave, se_math) %>%
    spread(escs_dummy, se_math) %>%
    transmute(country, wave,
              se_diff_math = sqrt(abs(`1`^2 - `0`^2)))

  read_se_data <-
    reduced_data %>%
    dplyr::select(country, escs_dummy, wave, se_read) %>%
    spread(escs_dummy, se_read) %>%
    transmute(country, wave,
              se_diff_read = sqrt(abs(`1`^2 - `0`^2)))

  se_data <- left_join(math_se_data, read_se_data)

  # Calculate the different between the gap and together with it's joint s.e graph
  # the absolut difference.

  math_diff <-
    reduced_data %>%
    dplyr::select(wave, country, escs_dummy, mean_math) %>%
    spread(escs_dummy, mean_math) %>%
    transmute(wave, country, diff_math = `1` - `0`)

  read_diff <-
    reduced_data %>%
    dplyr::select(wave, country, escs_dummy, mean_read) %>%
    spread(escs_dummy, mean_read) %>%
    transmute(wave, country, diff_read = `1` - `0`)

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
    dplyr::select(wave, country, diff_math, diff_read) %>%
    gather(test, difference, starts_with("diff")) %>%
    mutate(type_test = ifelse(.$test == "diff_math", "math", "read"))

  bounds_lower <-
    data_summaries %>%
    dplyr::select(wave, country, contains("lower")) %>%
    gather(lower_bound, lower, lower_math, lower_read) %>%
    mutate(type_test = ifelse(grepl("math", .$lower_bound), "math", "read"))

  bounds_upper <-
    data_summaries %>%
    dplyr::select(wave, country, contains("upper")) %>%
    gather(upper_bound, upper, upper_math, upper_read) %>%
    mutate(type_test = ifelse(grepl("math", .$upper_bound), "math", "read"))

  # Getting the original data in
  original_math <-
    reduced_data_math %>%
    dplyr::select(wave, everything(), -se_math) %>%
    gather(metric, value, -(wave:escs_dummy)) %>%
    unite(combination, escs_dummy, metric, sep = "_") %>%
    spread(combination, value) %>%
    mutate(type_test = "math")

  original_read <-
    reduced_data_read %>%
    dplyr::select(wave, everything(), -se_read) %>%
    gather(metric, value, -(wave:escs_dummy)) %>%
    unite(combination, escs_dummy, metric, sep = "_") %>%
    spread(combination, value) %>%
    mutate(type_test = "read")

  # final data
  complete_data <-
    left_join(differences, bounds_lower) %>%
    left_join(bounds_upper) %>%
    left_join(original_math) %>%
    left_join(original_read) %>%
    mutate(type = type_txt)
}


sample_size_descriptives <- function(results_math,
                                     sample_tables_topbottom,
                                     complete_data_topbottom) {
  avg_performance <-
    list(results_math[[1]], results_math[[6]]) %>%
    set_names(c(2000, 2015)) %>%
    enframe() %>%
    unnest()

  summary_table <-
    sample_tables_topbottom %>%
    mutate(escs_dummy = as.character(escs_dummy)) %>%
    filter(name %in% c("2000", "2015")) %>%
    left_join(avg_performance) %>%
    arrange(country, name)

  diff_cnts <-
    complete_data_topbottom %>%
    filter(type_test == "math") %>%
    dplyr::select(wave, country, difference) %>% 
    mutate(wave = as.character(wave))

  table_coming <-
    summary_table %>%
    dplyr::select(-perc) %>%
    gather(cats, vals, -(name:escs_dummy)) %>%
    unite(all_vals, escs_dummy, cats, sep = "_") %>%
    spread(all_vals, vals) %>%
    dplyr::select(name, country, `0_n`, `0_Mean`, `0_s.e.`, `1_n`, `1_Mean`, `1_s.e.`) %>%
    mutate(`0_n` = as.character(`0_n`),
           `1_n` = as.character(`1_n`)) %>%
    left_join(diff_cnts, by = c('name' = 'wave', 'country'))

  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste("\\
                          & & & Low SES & & & High SES \\\\
                          \\cmidrule(l){3-5}
                          \\cmidrule(l){6-8}",
                          paste0(
                            c("Year",
                              "Countries",
                              "N",
                              "Avg score",
                              "S.E",
                              "N",
                              "Avg score",
                              "S.E",
                              "SES gap"),
                            collapse = " & "),
                          "\\\\")

  # Table coming needs to be passed to xtable
  # and addtorow to print.xtable. See lines 1068
  # in chapter2.Rnw
  list(table_coming, addtorow)
}

tracking_descriptives <- function(tracking_data, countries) {
  tracking_table <-
    tracking_data %>%
    dplyr::select(cntry_name, tracks15y, selage, length, zvoc, ztrack) %>%
    rename(Countries = cntry_name,
           `# of tracks` = tracks15y,
           `Age of selection` = selage,
           `% of curric tracked` = length,
           `Std. Voc` = zvoc,
           `Std. tracking` = ztrack) %>%
    mutate_if(is_numeric, as.character) %>%
    filter(Countries %in% countries)

  # Tracking needs to be passed to xtable
  # See lines 1111 in chapter2.Rnw.
  tracking_table
}


diff_increase_fun <- function(df) {

  # Average standard deviation increase
  data_ready <-
    df %>%
    dplyr::select(wave, country, type_test, difference) %>%
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

      year_sd <-
        .x %>%
        gather(year, val, -(country:type_test)) %>%
        split(.$type_test) %>%
        map_dbl(~ bootstrapper(.x, mad(val, na.rm = T), B = 100) %>% .[[2, 2]]) %>%
        round(2) * 100


      .x %>%
        map_if(is_double, round, 3) %>%
        as_tibble() %>%
        transmute(type_test,
                  country,
                  diff = round(((!!last_year) - (!!first_year)), 1),
                  sd_year = year_sd,
                  diff_lower = diff - 1 * year_sd,
                  diff_upper = diff + 1 * year_sd,
                  years_available = years_available)
    })
  data_ready
}

order_cnt <- function(complete_data_topbottom, countries) {
  lm_data <- function(df) {
    lm(log(difference) ~ wave, data = df) %>%
      broom::tidy() %>%
      mutate(estimate = exp(estimate))
  }


  ordered_cnt <-
    complete_data_topbottom %>%
    filter(type_test == "math") %>%
    dplyr::select(wave, country, difference) %>%
    filter(country %in% countries) %>%
    split(.$country) %>%
    map(lm_data) %>%
    enframer("country") %>%
    filter(term == "wave") %>%
    arrange(-estimate) %>%
    pull(country)

  ordered_cnt
}


plot_evolution_gaps <- function(complete_data_topbottom,
                                ordered_cnt) {

  diff_data <-
    diff_increase_fun(complete_data_topbottom) %>%
    enframer("country") %>%
    filter(country %in% ordered_cnt) %>%
    dplyr::select(country, type_test, diff, contains("math")) %>%
    split(.$type_test) %>%
    map(~ .x %>% dplyr::select(-type_test) %>% deframe())

  pooled_trendline <-
    complete_data_topbottom %>%
    mutate(country = factor(country, levels = ordered_cnt, ordered = TRUE)) %>%
    filter(!is.na(country))

  pooled_trendline %>%
    filter(type_test == "math") %>%
    ggplot(aes(as.character(wave), difference)) +
    geom_point() +
    geom_linerange(aes(ymin = 0, ymax = difference)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) +
    geom_line(data = pooled_trendline, stat = "smooth", method = "lm", aes(group = 1),
              formula = y ~ splines::ns(x, 2), size = 0.7,
              colour = "red") +
    facet_wrap(~ country, ncol = 5) +
    scale_y_continuous(name = "90/10 gap in SD", expand = c(0, 0), lim = c(0, 3)) +
    scale_x_discrete(name = NULL, breaks = c(2000, 2009, 2015)) +
    theme_few() +
    theme(panel.spacing = unit(1, "lines"),
          panel.grid.major.y = element_line(colour = "grey")) +
    ggtitle("Evolution of the 90/10 achievement gap")

}

# Show the rates at which is increasing/decreasing
perc_increase_fun <- function(df) {

  # Average standard deviation increase
  data_ready <-
    df %>%
    dplyr::select(wave, country, type_test, difference) %>%
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

      year_sd <-
        .x %>%
        gather(year, val, -(country:type_test)) %>%
        split(.$type_test) %>%
        map_dbl(~ bootstrapper(.x, mad(val, na.rm = T), B = 100) %>% .[[2, 2]]) %>%
        round(2) * 100


      .x %>%
        map_if(is_double, round, 3) %>%
        as_tibble() %>%
        transmute(type_test,
                  country,
                  perc_diff = round(((!!last_year) - (!!first_year)) * 100, 1),
                  sd_year = year_sd,
                  diff_lower = perc_diff - 1 * year_sd,
                  diff_upper = perc_diff + 1 * year_sd,
                  years_available = years_available)
    })
  data_ready
}

perc_graph <- function(df, test, title, subtitle = NULL, ordered_cnt) {
  df %>%
    enframe(name = "country") %>%
    mutate(value = map(value, ~ dplyr::select(.x, -country))) %>% 
    unnest(value) %>%
    filter(country %in% ordered_cnt) %>%
    dplyr::select(-years_available) %>%
    mutate(diff_95_lower = perc_diff - 2*sd_year,
           diff_95_upper = perc_diff + 2*sd_year) %>%
    setNames(c("Country", "Type of test", "Average % difference", "Average SD",
               "Lower 50% bound", "Upper 50% bound", "Lower 95% bound", "Upper 95% bound")) %>%
    filter(`Type of test` == test) %>%
    arrange(`Average % difference`) %>%
    mutate(Country = ordered(forcats::as_factor(Country)),
           Country_num = as.numeric(Country)) %>%
    ggplot(aes(Country, `Average % difference`, fill = `Average % difference` > 0)) +
    geom_hline(yintercept = 0, linetype = "longdash") +
    geom_point() +
    geom_ribbon(aes(x = Country_num, ymin = `Lower 95% bound`, ymax = `Upper 95% bound`), alpha = 0.3) +
    geom_ribbon(aes(x = Country_num, ymin = `Lower 50% bound`, ymax = `Upper 50% bound`), alpha = 0.2) +
    geom_linerange(aes(ymin = 0, ymax = `Average % difference`,
                       colour = `Average % difference` > 0), alpha = 0.4) +
    scale_y_continuous(name = "Avg % increase/decrease", breaks = seq(-160, 160, 40)) +
    scale_fill_discrete(guide = FALSE) +
    scale_colour_discrete(guide = FALSE) +
    coord_cartesian(ylim = c(-160, 160)) +
    ggtitle(title, subtitle) +
    theme_few() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

evolution_ses_groups <- function(complete_data_topbottom, ordered_cnt) {
  # You left off here. Plot the gap for 1 and 0 for all countries in the graph before.
  complete_data_topbottom %>%
    filter(country %in% ordered_cnt) %>%
    mutate(country = factor(country, levels = ordered_cnt, ordered = TRUE)) %>%
    dplyr::select(wave, country, matches("*._math$")) %>%
    gather(ses, gap_size, matches("^\\d_mean_math$")) %>%
    separate(ses, c("ses", "delete"), sep = 2) %>%
    mutate(ses = gsub("_", "", ses)) %>%
    filter(complete.cases(.)) %>%
    ggplot(aes(as.factor(wave), gap_size, group = ses, colour = ses, shape = ses)) +
    geom_point(size = 1) +
    geom_line() +
    geom_line(stat = "smooth", method = "lm", aes(group = 1),
              formula = y ~ splines::ns(x, 1), linetype = "longdash",
              colour = "black") +
    geom_errorbar(aes(ymin = `0_lower_math`, ymax = `0_upper_math`), width = 0.3, alpha = 0.4, color = "#F8766D") +
    geom_errorbar(aes(ymin = `1_lower_math`, ymax = `1_upper_math`), width = 0.3, alpha = 0.4, color = "#00BFC4") +
    scale_y_continuous(name = "Standardized test scores (mean 0)") +
    scale_x_discrete(name = NULL, breaks = c(2000, 2009, 2015)) +
    scale_colour_discrete(name = NULL, labels = c("Low SES", "High SES")) +
    scale_shape_discrete(name = NULL, labels = c("Low SES", "High SES")) +
    ggtitle("Evolution of the achievement gap by top/bottom groups") +
    facet_wrap(~ country, ncol = 5) +
    theme_few() +
    theme(panel.spacing = unit(1, "lines"),
          legend.position = "bottom")

}

avg_increase_fun <- function(df, class) {

# Average standard deviation increase
  data_ready <-
    df %>%
    dplyr::select(wave, country, type_test, contains("mean_math")) %>%
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
    split(.$country)

    map2(data_ready, names(data_ready), ~ {

      print(.y)

      mean_df <-
        bootstrapper(.x, mean(diff, na.rm = T), B = 500) %>%
        filter(type == "bootstrap") %>%
        rename(mean = value)

      sd_df <-
        bootstrapper(.x, sd(diff, na.rm = T), B = 500) %>%
        filter(type == "bootstrap") %>%
        rename(sd = value)

      suppressMessages(
        left_join(mean_df, sd_df) %>%
        mutate(lower_bound = mean - 1 * sd,
               upper_bound = mean + 1 * sd)
      )
    }) %>%
    enframe() %>%
    unnest(value)
}

rate_change_graph <- function(avg_sd_increase_high,
                              avg_sd_increase_low) {
  
  thirtytwo_cnt <-
    c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Chile", 
      "Czech Republic", "Denmark", "Finland", "France", "Germany", 
      "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", 
      "Latvia", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
      "Spain", "Sweden", "Switzerland", "United Kingdom", "United States", 
      "Japan", "Slovakia", "Turkey", "Slovenia")

  selected_countries <- c("France", "Austria", "Sweden", "Denmark", "United States", "Germany")

  full_data <-
    left_join(dplyr::select(avg_sd_increase_high, name, mean, sd),
              dplyr::select(avg_sd_increase_low, name, mean, sd), by = "name") %>%
    mutate(continent = ifelse(name %in% selected_countries, "my_cnt", "other_cnt")) %>% 
    filter(name %in% thirtytwo_cnt) %>% 
    mutate(all_sd = sd.x + sd.y)

  colnames(full_data) <- c("country", "high_increase", "sd_high_increase",
                           "low_increase", "sd_low_increase", "continent",
                           "full_sd")

  lims <- list(xlim = c(-0.15, 0.20), ylim = c(-0.25, 0.25))

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
    geom_line(stat="smooth", method = "lm", se = FALSE, alpha = 0.5, colour = "grey", size = 1,
              linetype = "longdash") +
    geom_point(alpha = 0.2) +
    geom_point(data = filter(full_data, continent == "my_cnt"), colour = "red", alpha = 0.7) +
    geom_text_repel(data = filter(full_data, continent == "my_cnt"),
                    aes(label = country), box.padding = unit(3.7, "lines")) +
    geom_vline(xintercept = 0, alpha = 0.5) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    xlim(lims$xlim) +
    ylim(lims$ylim) +
    coord_cartesian(expand = FALSE) +
    annotate(geom = "text", x = 0.15, y = -0.2,
             label = "Low SES improves \n while High SES worsens",
             fontface = 2, size = 3) +
    annotate(geom = "text", x = -0.075, y = 0.20,
             label = "High SES improves \n while Low SES worsens",
             fontface = 2, size = 3) +
    labs(x = "Average increase of Low SES in SD", y = "Average increase of High SES in SD") +
    theme_few()

}

dif_data <- function(my_data, tracking_data, which_gap = "90th/10th SES gap") {
  my_data %>%
    filter(type_test == "math", type == which_gap) %>%
    dplyr::select(wave, country, difference) %>%
    rename(year = wave) %>%
    mutate(year = as.character(year)) %>%
    left_join(tracking_data, by = c("country" = "cntry_name")) %>%
    mutate(num_tracks = ifelse(tracks15y == 1, 1, 0) %>% as.factor,
           age_selection = selage) %>%
    filter(!is.na(num_tracks), !is.na(age_selection), !is.na(difference))

}

complete_tracking_model <- function(ready_data_age) {
  priorb2 <- set_prior("student_t(3, 0, 10)", class = "b")
  print("Memory used:")
  print(pryr::mem_used())

  all_track_models <-
    stan_model_builder(
      dv = "difference",
      iv = c("1", "num_tracks", "age_selection", "I(length > 0)", "zvoc", "year"),
      random = "(1 | country)",
      data = ready_data_age,
      prior = priorb2
    )

  all_track_models
}

interaction_tracking_model <- function(ready_data) {
  bprior1 <- c(prior_string("normal(0.3, 0.1)", coef = "ztrack"),
               prior_string("normal(-0.2, 0.1)", coef = "zvoc"),
               prior_string("normal(-0.15, 0.1)", coef = "ztrack:zvoc"))

  stan_models <-
    stan_model_builder(
      dv = "difference",
      iv = c("1", "ztrack", "zvoc", "ztrack:zvoc", "year"),
      random = "(1 | country)",
      data = ready_data,
      prior = bprior1,
      max_treed = 15
    )
}


interaction_plot <- function(interaction_models) {
  plot_interaction <-
    marginal_effects(interaction_models[[length(interaction_models)]],
                     effects = "ztrack:zvoc",
                     int_conditions = list(zvoc = c(-0.2, 0.46, 0.95)),
                     probs = c(.15, .85))

  data_interaction <-
    plot(plot_interaction, plot = FALSE)[[1]] %>%
    ggplot_build() %>%
    pluck(1) %>%
    pluck(1)

  order_colour <-
    data_interaction %>%
    distinct(group, colour) %>%
    mutate(colour = colour) %>%
    pull(colour) %>%
    rev()

  legend_labels <- c(25, 50, 75) %>% paste0("th quantile")
  legend_labels[1] <- paste0(legend_labels[1])
  legend_labels[3] <- paste0(legend_labels[3])

  data_interaction %>%
    mutate(linetype = factor(group, levels = 3:1),
           colour = factor(colour, levels = order_colour)) %>%
    ggplot(aes(x, y)) +
    geom_line(aes(colour = colour, linetype = linetype), size = 1) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = colour), alpha = 0.4) +
    labs(x = "Tracking Index (SD)", y = "90/10 Achievement gap (SD)") +
    scale_colour_discrete(
      name = "Vocational index \n quantiles (legend relative \n to line ordering)",
      labels = legend_labels
    ) +
    scale_linetype_discrete(
      name = "Vocational index \n quantiles (legend relative \n to line ordering)",
      labels = legend_labels
    ) +
    scale_fill_discrete(guide = FALSE) +
    theme_few() +
    ggtitle("Interaction between Tracking and Vocational index") +
    labs(caption = "The vocational index is composed by two measures 
                  of vocational enrollment and one of vocational specificity")

}

mod3_cumulative_change <- function(complete_gaps,
                                   tracking_data,
                                   gaps) {

  # One idea I had was to make a cumulative sum of the trend,
  # so the cumulative inequality over time for each country
  # and see how each tracking feature explains this cumulative
  # pattern, and it does so very well, with each one
  # separately (because I have only 30 observations) explaining
  # about 22% each. Age selection, up to 30% alone.

  vars_unique <- c("length",
                   "num_tracks",
                   "age_selection",
                   "ztrack",
                   "zvoc")

  all_gaps_models_cum <-
    map(gaps, function(gap) {

      gap_data <- dif_data(complete_gaps, tracking_data, which_gap = gap)

      ready_data_age <-
        gap_data %>%
        mutate(age_selection = ifelse(selage >= 15, 1, 0) %>% as.factor) %>%
        group_by(country) %>%
        summarize_at(vars(vars_unique), unique) %>%
        mutate(cum_diff =
                 gap_data %>%
                 group_by(country) %>%
                 summarize(m = sum(difference)) %>%
                 pull(m))

      mod_tracking <-
        ready_data_age %>%
        filter(!is.na(age_selection)) %>%
        brm(
          cum_diff ~ ztrack + zvoc,
          family = gaussian(),
          data = .,
          warmup = 1000, iter = 2000, chains = 5
        )

      mod_tracking
    })

  all_gaps_models_cum
}

## escs_data_trans[[1]]$ISCEDO <- NA_character_


# Japan
# Germany
# Spain
# Netherlands
# Denmark and Sweden no diff
# United States and Cnada (the same distribution)
# Netherlands
# Belgium
# France

# 2000 - 
# 2003 - PROGN/ISCEDO
# 2006 - PROGN/ISCEDO
# 2009 - PROGN/ISCEDO
# 2012 - PROGN/ISCEDO
# 2015 - PROGN/ISCEDO

## merged_df <-
##   escs_data_trans %>%
##   map(~ filter(.x, country == "France", !is.na(escs_dummy))) %>%
##   map(~ mutate(.x, ISCEDO = as.character(ISCEDO))) %>% 
##   map(~ select(.x, country, wave, high_edu_broad, escs_dummy, contains("PROGN"),
##                contains("ISCEDO"))) %>%
##   bind_rows()

## merged_df %>%
##   count(wave, escs_dummy, PROGN) %>%
##   group_by(wave, escs_dummy) %>% 
##   mutate(perc = n / sum(n)) %>%
##   filter(wave == "pisa2009") %>%
##   arrange(escs_dummy, perc) %>%
##   top_n(3) %>%
##   ggplot(aes(PROGN, perc, fill = as.character(escs_dummy))) +
##   geom_col(position = "dodge") +
##   theme(axis.text.x = element_text(angle = 90))

## thirtytwo_cnt <-
##   c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Chile", 
##     "Czech Republic", "Denmark", "Finland", "France", "Germany", 
##     "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", 
##     "Latvia", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", 
##     "Spain", "Sweden", "Switzerland", "United Kingdom", "United States", 
##     "Japan", "Slovakia", "Turkey", "Slovenia")

## model_df <-
##   escs_data_trans %>%
##   map(~ filter(.x, !is.na(escs_dummy), country %in% thirtytwo_cnt)) %>%
##   map(~ mutate(.x,
##                ISCEDO = as.character(ISCEDO),
##                avg_math = rowMeans(dplyr::select(.x, ends_with("MATH")), na.rm = TRUE))) %>% 
##   map(~ dplyr::select(.x, country,
##                wave,
##                high_edu_broad,
##                escs_dummy,
##                contains("PROGN"),
##                avg_math)) %>%
##   bind_rows()

## tst <-
##   model_df %>%
##   nest(data = c(escs_dummy, high_edu_broad, avg_math, PROGN)) %>%
##   mutate(model = map(data, ~ lm(avg_math ~ escs_dummy, data = .x)),
##          within_group = map_dbl(model, ~ anova(.x)["Residuals", "Mean Sq"]),
##          between_group = map_dbl(model, ~ anova(.x)["escs_dummy", "Mean Sq"]),
##          perc_within = within_group / (within_group + between_group),
##          sd_within_low = map_dbl(data, ~ .x %>% filter(escs_dummy == 0) %>% pull(avg_math) %>% sd()),
##          sd_within_high = map_dbl(data, ~ .x %>% filter(escs_dummy == 1) %>% pull(avg_math) %>% sd()),
##          ratio_sd = sd_within_high / sd_within_low,
##          avg_bottom = map_dbl(data, ~ filter(.x, escs_dummy == 0) %>% pull(avg_math) %>% mean()),
##          avg_high = map_dbl(data, ~ filter(.x, escs_dummy == 1) %>% pull(avg_math) %>% mean()),
##          gap = (avg_high - avg_bottom))

## tst %>%
##   ggplot(aes(x = wave, y = gap, group = 1)) +
##   geom_line() +
##   facet_wrap(~ country)


## null_model <- brm(gap ~ (1 | country/wave), data = tst,
##                   control = list(adapt_delta = 0.99,
##                                  max_treedepth = 15))

## full_model <- brm(gap ~ ratio_sd + (1 | country/wave), data = tst,
##                   control = list(adapt_delta = 0.99, max_treedepth = 15))

## null_var <- as.data.frame(VarCorr(null_model))["sdcor"]
## full_var <- as.data.frame(VarCorr(full_model))["sdcor"]

## (null_var - full_var) / null_var

## mod_dt <- tst %>% slice(10) %>% pull(data) %>% .[[1]]

## anova(lm(avg_math ~ escs_dummy, mod_dt))

## mod_dt %>%
##   group_by(escs_dummy) %>%
##   summarize(avg = mean(avg_math),
##             sd = sd(avg_math))

## hist(mod_dt$avg_math)

## tst <- lmer(avg_math ~ (1 | country:wave:escs_dummy), data = model_df)
