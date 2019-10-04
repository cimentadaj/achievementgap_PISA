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

plan <-
  drake_plan(
    pisa_data = target(read_harmonize_pisa(raw_data_dir), format = "fst"),
    escs_data = read_escs(raw_data_dir),
  )
