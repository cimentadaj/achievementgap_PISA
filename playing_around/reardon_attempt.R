# This is, so far, a failed attempt at calculating the 90/10
# percentiles for an ordinal variable. This is an implementation taken
# from Reardon (2011), so please check the paper first.

library(GoFKernel)
library(tidyverse)

x <- rep(1:7, each = 10)
y <- seq(250, 500, length.out = 70)

x_density <- density(x, adjust = 2)
plot(our_density)

lookup <- tapply(X = our_density$y, INDEX = rep(1:7, each = 73), FUN = mean)
prop <- c(0, unique(cume_dist(sort(m$high_edu_broad))) * 100)

our_dens_fun <- function(x) x * density(x, adjust = 2, n = length(x))$y
our_dens_fun(x)

x <- 1
integrate(our_dens_fun, lower = c_k[1], upper = c_k[1 + 1])

inverse_cdf <- inverse(integrate(our_dens_fun, lower = prop[x], upper = prop[x + 1]))

# given a ck and ck_prev
theta_bottom <- inverse_cdf(prop[x])
theta_top <- inverse_cdf(prop[x + 1])

# A(1) from paper
fn <- function (x) {
  x * our_density(x)
}

theta_hat <- integrate(fn, theta_bottom, theta_top) / (ck - ck_prev)

lm(y ~ 1 + theta_hat + (theta_hat^2 + ck_bla bla))