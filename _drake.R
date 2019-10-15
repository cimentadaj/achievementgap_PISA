# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source("code/01-packages.R")  # Load your packages, e.g. library(drake).
source("code/02-analysis.R") # Define your custom code as a bunch of functions.
source("code/plan.R")      # Create your drake plan.

# options(clustermq.scheduler = "multicore")
# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# options(clustermq.scheduler = "multicore") # For parallel computing.
dconf <- drake_config(
  plan,
  verbose = 2,
  # See https://github.com/ropensci/drake/issues/960
  env = new.env(parent = globalenv()), 
  memory_strategy = "memory"
)

# Read the raw data which is very heavy to have in drake
read_raw_data <- FALSE

if (read_raw_data) {
  print("PISA packages loaded")
  library(PISA2000lite)
  library(PISA2003lite)
  library(PISA2006lite)
  library(PISA2009lite)

  # read_pisa unloads the packages when it reads the data
  big_pisa_dt <- read_pisa(raw_data_dir)
  iwalk(big_pisa_dt, ~ write_fst(.x, here("processed_data", paste0(.y, ".fst"))))
  rm(big_pisa_dt)
}

pisa_files <-
  sort(
    list.files(here("processed_data"), pattern = ".fst", full.names = TRUE)
  )

if ("pisa_data" %in% outdated(dconf)) {
  print("Reading raw data outside of drake")
  raw_data <- map(pisa_files, ~ as_tibble(read_fst(.x))) %>% enframe()
  print("Raw data read")  
}

# These pkgs take up a lot of memory. Only
# of the target that uses the data is outdated
# make sure to load them. Right after I use
# them I unload them again from memory
# in the same function, so it's safe
# to load them ONLY when pisa_data || pisa_school_data
# has to be run again.
if ("pisa_school_data" %in% outdated(dconf)) {
  print("PISA packages loaded")
  library(PISA2000lite)
  library(PISA2003lite)
  library(PISA2006lite)
  library(PISA2009lite)
}

dconf
