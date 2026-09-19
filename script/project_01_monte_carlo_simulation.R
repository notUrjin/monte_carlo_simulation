# Project-01 Monte Carlo Simulation
# version 1.0


# 1.) Population: Standard Normal
# Parameters: μ=0, σ=1
# Sample sizes: n = 10, 20, 30, 40
student_id <- 9438
set.seed(student_id)
B <- 10000
means_a <- replicate(B, mean(rnorm(10)))
means_b <- replicate(B, mean(rnorm(20)))
means_c <- replicate(B, mean(rnorm(30)))
means_d <- replicate(B, mean(rnorm(40)))

configs <- list(
  list(dat = means_a, n = 10, col = "tomato"),
  list(dat = means_b, n = 20, col = "darkorange"),
  list(dat = means_c, n = 30, col = "steelblue"),
  list(dat = means_d, n = 40, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 1a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = cfg$n, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean",
       xlim = c(-1, 1))
  curve(dnorm(x, mean = 0, sd = 1/sqrt(cfg$n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 1b.) Normal Q-Q plots
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

# Test 1c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(10, 20, 30, 40),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 1d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(10, 20, 30, 40),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 1e.) Shapiro-Wilk Test
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(10, 20, 30, 40), sw_results, row.names = NULL)





# 2.) Population: Continuous Uniform
# Parameters: a=0, b=1
# Sample sizes: n = 2, 4, 6, 8
student_id <- 9438
set.seed(student_id)
B <- 10000
a <- 0 # lower (left) bound of x-axis
b <- 1 # upper (right) bound of x-axis
means_a <- replicate(B, mean(runif(2, min=a, max=b)))
means_b <- replicate(B, mean(runif(4, min=a, max=b)))
means_c <- replicate(B, mean(runif(6, min=a, max=b)))
means_d <- replicate(B, mean(runif(8, min=a, max=b)))

configs <- list(
  list(dat = means_a, n = 2, col = "tomato"),
  list(dat = means_b, n = 4, col = "darkorange"),
  list(dat = means_c, n = 6, col = "steelblue"),
  list(dat = means_d, n = 8, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 2a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean",
       xlim = c(a, b))
  curve(dnorm(x, mean = (a+b)/2, sd = 1/sqrt(12 * cfg$n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 2b.) Normal Q-Q Plot
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

# Test 2c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(6, 7, 8, 9),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 2d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(6, 7, 8, 9),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 2e.) Shapiro-Wilk Test






# 3.) Population: Six-Sided Die (Discrete Uniform Distribution)
# Parameters/assumptions: values=1-6 (equally likely)
# Sample Sizes: n = 6, 12, 18, 24
student_id <- 9438
set.seed(student_id)
B <- 10000
a <- 1
b <- 6
means_a <- replicate(B, mean(sample(1:6, size=6, replace=TRUE)))
means_b <- replicate(B, mean(sample(1:6, size=12, replace=TRUE)))
means_c <- replicate(B, mean(sample(1:6, size=18, replace=TRUE)))
means_d <- replicate(B, mean(sample(1:6, size=24, replace=TRUE)))

configs <- list(
  list(dat = means_a, n = 6, col = "tomato"),
  list(dat = means_b, n = 12, col = "darkorange"),
  list(dat = means_c, n = 18, col = "steelblue"),
  list(dat = means_d, n = 24, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 3a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean",
       xlim = c(a, b))
  curve(dnorm(x, mean = (a+b)/2, sd = sqrt(
    ((b-a+1)^2 - 1) / (12 * cfg$n)
  )),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 3b.) Normal Q-Q Plot
# Test 3c.) Skewness
# Test 3d.) Kurtosis
# Test 3e.) Shapiro-Wilk Test


# 4.) Population: Exponential
# Parameters: λ=1 (mean=1)
# Sample sizes: n = 2, 4, 6, 8
student_id <- 9438
set.seed(student_id)
B <- 10000
lambda <- 1
means_a <- replicate(B, mean(rexp(2, rate=lambda)))
means_b <- replicate(B, mean(rexp(4, rate=lambda)))
means_c <- replicate(B, mean(rexp(6, rate=lambda)))
means_d <- replicate(B, mean(rexp(8, rate=lambda)))

configs <- list(
  list(dat = means_a, n = 2, col = "tomato"),
  list(dat = means_b, n = 4, col = "darkorange"),
  list(dat = means_c, n = 6, col = "steelblue"),
  list(dat = means_d, n = 8, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 4a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 40, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean"
  )
  curve(dnorm(x, mean = 1/lambda, sd = 1/(sqrt(cfg$n)*lambda)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 4b.) Normal Q-Q Plot
# Test 4c.) Skewness
# Test 4d.) Kurtosis
# Test 4e.) Shapiro-Wilk Test


# 5.) Population: Poisson
# Parameter: λ=1
# Sample sizes: n = 6, 12, 18, 24
student_id <- 9438
set.seed(student_id)
B <- 10000
lambda <- 1
means_a <- replicate(B, mean(rpois(6, lambda)))
means_b <- replicate(B, mean(rpois(12, lambda)))
means_c <- replicate(B, mean(rpois(18, lambda)))
means_d <- replicate(B, mean(rpois(24, lambda)))

configs <- list(
  list(dat = means_a, n = 6, col = "tomato"),
  list(dat = means_b, n = 12, col = "darkorange"),
  list(dat = means_c, n = 18, col = "steelblue"),
  list(dat = means_d, n = 24, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 5a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean"
  )
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda/cfg$n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 5b.) Normal Q-Q Plot
# Test 5c.) Skewness
# Test 5d.) Kurtosis
# Test 5e.) Sharpiro-Wilk Test





# 6.) Population: Binomial Small
# Parameters: n=5, p=0.10
# Sample sizes (number of independent trials): N = 5, 10, 15, 30
student_id <- 9438
set.seed(student_id)
B <- 10000
n <- 5
p <- 0.10
means_a <- replicate(B, mean(rbinom(n, 5, p)))
means_b <- replicate(B, mean(rbinom(n, 10, p)))
means_c <- replicate(B, mean(rbinom(n, 15, p)))
means_d <- replicate(B, mean(rbinom(n, 30, p)))

configs <- list(
  list(dat = means_a, N = 5, col = "tomato"),
  list(dat = means_b, N = 10, col = "darkorange"),
  list(dat = means_c, N = 15, col = "steelblue"),
  list(dat = means_d, N = 30, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 6a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("N =", cfg$N), xlab = "Sample mean"
  )
  curve(dnorm(x, mean = (cfg$N)*p, sd = sqrt(cfg$N*p*(1-p)/n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 6b.) Normal Q-Q Plot
# Test 6c.) Skewness
# Test 6d.) Kurtosis
# Test 6e.) Shapiro-Wilk Test





# 7.) Population: Binomial Large
# Parameters: n=20, p=0.50
# Sample sizes (number of independent trials): N = 5, 10, 15, 30
student_id <- 9438
set.seed(student_id)
B <- 10000
n <- 20
p <- 0.50
means_a <- replicate(B, mean(rbinom(n, 5, p)))
means_b <- replicate(B, mean(rbinom(n, 10, p)))
means_c <- replicate(B, mean(rbinom(n, 15, p)))
means_d <- replicate(B, mean(rbinom(n, 30, p)))

configs <- list(
  list(dat = means_a, N = 5, col = "tomato"),
  list(dat = means_b, N = 10, col = "darkorange"),
  list(dat = means_c, N = 15, col = "steelblue"),
  list(dat = means_d, N = 30, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 7a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("N =", cfg$N), xlab = "Sample mean"
  )
  curve(dnorm(x, mean = (cfg$N)*p, sd = sqrt(cfg$N*p*(1-p)/n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 7b.) Normal Q-Q Plot
# Test 7c.) Skewness
# Test 7d.) Kurtosis
# Test 7e.) Shapiro-Wilk Test





# 8.) Population: Cauchy
# Parameters: Location=0, scale=1
# Sample sizes: n = 5, 10, 15, 30
student_id <- 9438
set.seed(student_id)
B <- 10000
loc <- 0
s <- 1
means_a <- replicate(B, mean(rcauchy(1, loc, s)))
means_b <- replicate(B, mean(rcauchy(2, loc, s)))
means_c <- replicate(B, mean(rcauchy(3, loc, s)))
means_d <- replicate(B, mean(rcauchy(4, loc, s)))

configs <- list(
  list(dat = means_a, n = 1, col = "tomato"),
  list(dat = means_b, n = 2, col = "darkorange"),
  list(dat = means_c, n = 3, col = "steelblue"),
  list(dat = means_d, n = 4, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 8a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean"
  )
}

# Test 8b.) Normal Q-Q Plot
# Test 8c.) Skewness
# Test 8d.) Kurtosis
# Test 8e.) Shapiro-Wilk Test





# 9.) Population: Dependent Machine Failure (TBD)
# Test 9a.) Histograms with theoretical normal curve
# Test 9b.) Normal Q-Q Plot
# Test 9c.) Skewness
# Test 9d.) Kurtosis
# Test 9e.) Shapiro-Wilk Test





# 10.) Population: Non-identical Bernoulli
# Test 10a.) Histograms with theoretical normal curve
# Test 10b.) Normal Q-Q Plot
# Test 10c.) Skewness
# Test 10d.) Kurtosis
# Test 10e.) Shapiro-Wilk Test





# 11.) Population: Life/Death Population
# Test 11a.) Histograms with theoretical normal curve
# Test 11b.) Normal Q-Q Plot
# Test 11c.) Skewness
# Test 11d.) Kurtosis
# Test 11e.) Shapiro-Wilk Test
