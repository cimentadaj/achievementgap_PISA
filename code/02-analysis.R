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

      # PISA 2015 doesn't have the schoolid column at the moment.
      if (unique(.x$wave) == "pisa2015") {
        select(.x,
               wave,
               country,
               AGE,
               matches("PV[0-9]{1,2}[MATH|READ]"),
               W_FSTUWT,
               ESCS,
               high_edu_broad,
               gender,
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
               matches("ST16Q03|ST37Q01|ST019CQ01T|ST013Q01TA"))
      }

    })


  pisa_all

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
