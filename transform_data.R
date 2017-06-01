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

temp_dir <- tempdir()
url <- "http://vs-web-fs-1.oecd.org/pisa/PUF_SPSS_COMBINED_CMB_STU_QQQ.zip"
dir_pisa <- file.path(temp_dir, "pisa.zip")
download.file(url, destfile = dir_pisa)

unzip(dir_pisa, exdir = temp_dir)

pisa_2015 <- read_spss(file.path(temp_dir, "CY6_MS_CMB_STU_QQQ.sav"))

# Download PISA 2012 files
url <- "http://www.oecd.org/pisa/pisaproducts/INT_STU12_DEC03.zip"
pisa2012_syntax <- "http://www.oecd.org/pisa/pisaproducts/PISA2012_SAS_student.sas"

# Download the SAS syntax to determine column width
download.file(pisa2012_syntax, file.path(temp_dir, "PISA2012_SAS_student.sas"))
dic_student <- parse.SAScii(sas_ri = file.path(temp_dir, 'PISA2012_SAS_student.sas'))

# Download PISA 2012 actual data
dir_pisa <- file.path(temp_dir, "pisa2012.zip")
download.file(url, destfile = dir_pisa)
unzip(dir_pisa, exdir = temp_dir)

# Read the data using the fixed width from the SAS syntax
student2012 <- read_fwf(file = file.path(temp_dir, 'INT_STU12_DEC03.txt'),
                        col_positions = fwf_widths(dic_student$width), progress = T)
colnames(student2012) <- dic_student$varname

# Remove temporary files
file.remove(file.path(temp_dir, list.files(temp_dir)))

student2015 <- pisa_2015 # toy dataset

# Provisional code for separate PISA2012
# Vector with database names except student2015
databases <- c("math2000", paste0("student", seq(2003, 2009, 3)))

# Create a list with all data sets
pisa_list <- map(c(databases, paste0("student", c(2012, 2015))), get)

# Give each dataset their own name
names(pisa_list) <- c("student2000", databases[-1], "student2012","student2015")

# Fix the country variable name which is different for PISA 2000
pisa_list$math2000$CNT <- pisa_list$math2000$COUNTRY

# Create a data frame with a list column containing all lists.
pisa_all <- enframe(pisa_list)

# See:
pisa_all

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
gender <- c("ST03Q01", "ST03Q01", "ST04Q01", "ST04Q01", "ST04Q01", "ST004D01T")

# Loop through each data frame and assign the gender variable name
# and turn it into character
pisa_all$value <- map(seq_along(pisa_all$value), function(pisa_data) {
  
  pisa_all$value[[pisa_data]] %>%
    rename_("gender" = gender[pisa_data]) %>%
    mutate(gender = as.character(gender),
           country = pisa_countrynames[CNT],
           region = countrycode(country, "country.name", "region")) %>%
    as_tibble()
})

write_rds(pisa_all, path = "./data/pisa_listcol.Rdata")