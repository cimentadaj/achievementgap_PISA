# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source("code/01-packages.R")  # Load your packages, e.g. library(drake).
source("code/02-analysis.R") # Define your custom code as a bunch of functions.
source("code/plan.R")      # Create your drake plan.

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# options(clustermq.scheduler = "multicore") # For parallel computing.
drake_config(
  plan,
  verbose = 2
)
