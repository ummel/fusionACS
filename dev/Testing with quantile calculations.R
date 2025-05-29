library(tidyverse)
library(data.table)
library(collapse)
library(microbenchmark)

#---

d <- data.frame(x = rnorm(1e6), y = rnorm(1e6), z = rnorm(1e6))
g <- sample(letters, size = nrow(d), replace = TRUE)
w <- runif(nrow(d))

# fmedian() can utilize multithreading
microbenchmark(fmedian(d, g = g, w = w),
               fmedian(d, g = g, w = w, nthreads = 3),
               times = 10)

# If I want multiple quantiles for a given x, fastest to presort. This is because fnth() doesn't accept multiple percentiles
x <- rlnorm(1e6, meanlog = 5, sdlog = 0.5)
hist(x, breaks = 100)
g <- sample(letters, size = length(x), replace = TRUE)
w <- runif(length(x))
#o <- radixorder(g, x)

# What is the KDE density at the median? (whole sample)
m <- fmedian(x, w = w)
d <- density(x, weights = w / sum(w))
f.med <- approx(d$x, d$y, xout = m)$y

# Returns data.table containing the median and its approximate density, by group 'g'
median_density <- function(x, g, w, eps = 0.05) {
  p <- 0.5 + c(-eps, 0, eps)
  if (inherits(g, "GRP")) g <- g$group.id
  o <- collapse::radixorder(g, x)
  q <- sapply(p, function(n) collapse::fnth(x, n = n, g = g, w = w, o = o, use.g.names = FALSE, check.o = FALSE))  # Also accepts 'nthreads' argument
  f.med <- (p[3] - p[1]) / (q[, 3] - q[, 1]) # Approximate density at median
  return(data.table(median = q[, 2], density = f.med))
}

# Assumed data.frame for which we want to calculate group-wise medians and SE
df <- data.frame(x = rlnorm(1e6, meanlog = 5, sdlog = 0.5), y = rlnorm(1e6, meanlog = 10, sdlog = 0.2))
g <- sample(letters, size = nrow(df), replace = TRUE)
w <- runif(nrow(df))

test <- df %>%
  lapply(median_density, g = g, w = w) %>%
  rbindlist(idcol = "var")

# FROM HERE: Would merge with existing results and then calculate approx. SE using formula below
#"The standard error of the sample median was estimated using the large-sample approximation SE(m^)≈12f(m)⋅1neffSE(m^)≈2f(m)1​⋅neff​
#​1​, where f(m)f(m) is the estimated density at the median. This formula arises from the asymptotic variance of sample quantiles under regularity conditions (Serfling, 1980; van der Vaart, 1998)."

# SE of the median via analytical approximation
n_eff <- sum(w)^2 / sum(w^2)
1 / (2 * f.med * sqrt(n_eff))

# SE of the median via Tukey's approxmation
fsd(x, w = w) / sqrt(n_eff) * (pi / 2)


