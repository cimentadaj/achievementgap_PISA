# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source(here::here("code/01-packages.R"))  # Load your packages, e.g. library(drake).
source(here::here("code/02-analysis.R")) # Define your custom code as a bunch of functions.
source(here::here("code/plan.R"))      # Create your drake plan.

# options(clustermq.scheduler = "multicore")
# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# options(clustermq.scheduler = "multicore") # For parallel computing.
dconf <- drake_config(
  plan,
  verbose = 2,
  ## See https://github.com/ropensci/drake/issues/960
  env = new.env(parent = globalenv()),
  ## lock_envir = TRUE,
  memory_strategy = "memory",
  garbage_collection = TRUE
)

source(here("code/read_raw_data.R"))

if ("harmonized_student" %in% outdated(dconf)) {
  print("Reading raw data outside of drake")
  raw_student <- map(pisa_files, ~ as_tibble(read_fst(.x))) %>% enframe()
  print("Raw data read")  
}

dconf
