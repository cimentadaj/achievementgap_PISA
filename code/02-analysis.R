
read_harmonize_pisa <- function(raw_data) {
  # Read PISA 2012 files
  pisa2012_path <- file.path(raw_data, "pisa2012", "INT_STU12_DEC03.txt")
  dic_path <- file.path(raw_data, "pisa2012", "PISA2012_SAS_student.sas")
  dic_student <- parse.SAScii(sas_ri = dic_path)
  student2012 <- read_fwf(pisa2012_path,
                          col_positions = fwf_widths(dic_student$width),
                          progress = TRUE)

  colnames(student2012) <- dic_student$varname

  # Read PISA 2015 data
  pisa2015_path <- file.path(raw_data, "pisa2015", "CY6_MS_CMB_STU_QQQ.sav")
  pisa_countrynames <- c(pisa_countrynames, "United States" = "USA")
  student2015 <- read_spss(pisa2015_path)

  # Provisional code for separate PISA2012
  # Vector with database names except student2015
  databases <- c("math2000", paste0("student", seq(2003, 2009, 3)))

  # Create a list with all data sets
  pisa_list <- map(c(databases, paste0("student", c(2012, 2015))), get)

  # Give each dataset their own name
  names(pisa_list) <- c("student2000",
                        databases[-1],
                        "student2012",
                        "student2015")

  # Fix the country variable name which is different for PISA 2000
  pisa_list$math2000$CNT <- pisa_list$math2000$COUNTRY

  # Create a data frame with a list column containing all lists.
  pisa_all <- enframe(pisa_list)

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

  pisa_all
}

read_escs <- function(raw_data) {
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
}
