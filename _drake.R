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
  env = new.env(parent = globalenv()), # See https://github.com/ropensci/drake/issues/960
  memory_strategy = "memory"
  )

# These pkgs take up a lot of memory. Only
# of the target that uses the data is outdated
# make sure to load them. Right after I use
# them I unload them again from memory
# in the same function, so it's safe
# to load them ONLY when pisa_data || pisa_school_data
# has to be run again.
if ("pisa_data" %in% outdated(dconf) | "pisa_school_data" %in% outdated(dconf)) {
  print("PISA packages loaded")
  library(PISA2000lite)
  library(PISA2003lite)
  library(PISA2006lite)
  library(PISA2009lite)
}

dconf
