# Project-01 Monte Carlo Simulation
# version 1.2


# 1.) Population: Standard Normal
# Parameters: μ=0, σ=1
student_id <- 9438
set.seed(student_id)
B <- 10000
na <- 10
nb <- 20
nc <- 30
nd <- 100
means_a <- replicate(B, mean(rnorm(na)))
means_b <- replicate(B, mean(rnorm(nb)))
means_c <- replicate(B, mean(rnorm(nc)))
means_d <- replicate(B, mean(rnorm(nd)))

configs <- list(
  list(dat = means_a, n = na, col = "tomato"),
  list(dat = means_b, n = nb, col = "darkorange"),
  list(dat = means_c, n = nc, col = "steelblue"),
  list(dat = means_d, n = nd, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 1a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 40, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean",
       xlim = c(-1, 1))
  curve(dnorm(x, mean = 0, sd = 1/sqrt(cfg$n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 1b.) Normal Q-Q plots
## Define this function only once, and use it for the other distributions ##
qq_difference_table <- function(x) {
  x <- sort(na.omit(x))
  n <- length(x)
  
  theoretical <- qnorm(ppoints(n))
  
  q_theoretical <- qnorm(c(0.25, 0.75))
  q_sample <- quantile(x, c(0.25, 0.75))
  
  slope <- diff(q_sample) / diff(q_theoretical)
  intercept <- q_sample[1] - slope * q_theoretical[1]
  
  fitted <- intercept + slope * theoretical
  difference <- x - fitted
  
  data.frame(
    Theoretical_Quantile = theoretical,
    Observed_Quantile = x,
    Fitted_Line = fitted,
    Difference = difference,
    Absolute_Difference = abs(difference)
  )
}

## Copy this part up until next test section "Skewness"
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

results <- do.call(rbind, lapply(configs, function(cfg) {
  qq_table <- qq_difference_table(cfg$dat)
  
  data.frame(
    n = cfg$n,
    Total_Deviation_From_Ideal_Line = sum(qq_table$Difference)
  )
}))
rownames(results) <- NULL
print(results)

# Test 1c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)


# Test 1d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 1e.) Shapiro-Wilk Test
# H0: The data comes from a normal distribution.
# Ha: The data does not come from a normal distribution.
# Fail to reject H0 when p > 0.05 (data is normal)
# Reject H0 when p <= 0.05 (data is not normal)
student_id <- 9438
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(na, nb, nc, nd), sw_results, row.names = NULL)


curve(dunif(x, min = 1, max = 6), from = 0, to = 7,
      ylim = c(0, 0.22), lwd = 3,
      xlab = "x", ylab = "Density",
      main = "Uniform(1, 6)")


# 2.) Population: Six-Sided Die (Discrete Uniform Distribution)
# Parameters/assumptions: values=1-6 (equally likely)
student_id <- 9438
set.seed(student_id)
B <- 10000
na <- 6
nb <- 30
nc <- 60
nd <- 78
means_a <- replicate(B, mean(sample(1:6, size=na, replace=TRUE)))
means_b <- replicate(B, mean(sample(1:6, size=nb, replace=TRUE)))
means_c <- replicate(B, mean(sample(1:6, size=nc, replace=TRUE)))
means_d <- replicate(B, mean(sample(1:6, size=nd, replace=TRUE)))

configs <- list(
  list(dat = means_a, n = na, col = "tomato"),
  list(dat = means_b, n = nb, col = "darkorange"),
  list(dat = means_c, n = nc, col = "steelblue"),
  list(dat = means_d, n = nd, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 2a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean",
       xlim = c(1, 6))
  curve(dnorm(x, mean = (6+1)/2, sd = sqrt(
    ((6-1+1)^2 - 1) / (12 * cfg$n)
  )), add = TRUE, col = "firebrick", lwd = 2)
}

# Test 2b.) Normal Q-Q Plot
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

results <- do.call(rbind, lapply(configs, function(cfg) {
  qq_table <- qq_difference_table(cfg$dat)
  
  data.frame(
    n = cfg$n,
    Total_Deviation_From_Ideal_Line = sum(qq_table$Difference)
  )
}))
rownames(results) <- NULL
print(results)

# Test 2c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 2d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 2e.) Shapiro-Wilk Test
student_id <- 9438
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(na, nb, nc, nd), sw_results, row.names = NULL)





# 3.) Population: Exponential
# Parameters: λ=1 (mean=1)
student_id <- 9438
set.seed(student_id)
B <- 10000
lambda <- 1
na <- 2
nb <- 30
nc <- 50
nd <- 300
means_a <- replicate(B, mean(rexp(na, rate=lambda)))
means_b <- replicate(B, mean(rexp(nb, rate=lambda)))
means_c <- replicate(B, mean(rexp(nc, rate=lambda)))
means_d <- replicate(B, mean(rexp(nd, rate=lambda)))

configs <- list(
  list(dat = means_a, n = na, col = "tomato"),
  list(dat = means_b, n = nb, col = "darkorange"),
  list(dat = means_c, n = nc, col = "steelblue"),
  list(dat = means_d, n = nd, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 3a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 40, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("n =", cfg$n), xlab = "Sample mean", xlim = c(0,4)
  )
  curve(dnorm(x, mean = 1/lambda, sd = 1/(sqrt(cfg$n)*lambda)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 3b.) Normal Q-Q Plot
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

results <- do.call(rbind, lapply(configs, function(cfg) {
  qq_table <- qq_difference_table(cfg$dat)
  
  data.frame(
    n = cfg$n,
    Total_Deviation_From_Ideal_Line = sum(qq_table$Difference)
  )
}))
rownames(results) <- NULL
print(results)

# Test 3c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 3d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 3e.) Shapiro-Wilk Test
student_id <- 9438
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(na, nb, nc, nd), sw_results, row.names = NULL)




# 4.) Population: Binomial Small
# Parameters: n=5, p=0.10
# number of independent trials: N
student_id <- 9438
set.seed(student_id)
B <- 10000
n <- 5
p <- 0.10
na <- 30
nb <- 60
nc <- 200
nd <- 400
means_a <- replicate(B, mean(rbinom(n, na, p)))
means_b <- replicate(B, mean(rbinom(n, nb, p)))
means_c <- replicate(B, mean(rbinom(n, nc, p)))
means_d <- replicate(B, mean(rbinom(n, nd, p)))

configs <- list(
  list(dat = means_a, N = na, col = "tomato"),
  list(dat = means_b, N = nb, col = "darkorange"),
  list(dat = means_c, N = nc, col = "steelblue"),
  list(dat = means_d, N = nd, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 4a.) Histograms with theoretical normal curve
for (cfg in configs) {
  hist(cfg$dat, breaks = 30, probability = TRUE,
       col = cfg$col, border = "white",
       main = paste("N =", cfg$N), xlab = "Sample mean"
  )
  curve(dnorm(x, mean = (cfg$N)*p, sd = sqrt(cfg$N*p*(1-p)/n)),
        add = TRUE, col = "firebrick", lwd = 2)
}

# Test 4b.) Normal Q-Q Plot
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$N),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

results <- do.call(rbind, lapply(configs, function(cfg) {
  qq_table <- qq_difference_table(cfg$dat)
  
  data.frame(
    n = cfg$N,
    Total_Deviation_From_Ideal_Line = sum(qq_table$Difference)
  )
}))
rownames(results) <- NULL
print(results)

# Test 4c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 4d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 4e.) Shapiro-Wilk Test
student_id <- 9438
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(na, nb, nc, nd), sw_results, row.names = NULL)



# 5.) Population: Cauchy
# Parameters: Location=0, scale=1
# Sample sizes: n = 5, 10, 15, 30
student_id <- 9438
set.seed(student_id)
B <- 10000
loc <- 0
s <- 1
na <- 1
nb <- 2
nc <- 3
nd <- 4
means_a <- replicate(B, mean(rcauchy(na, loc, s)))
means_b <- replicate(B, mean(rcauchy(nb, loc, s)))
means_c <- replicate(B, mean(rcauchy(nc, loc, s)))
means_d <- replicate(B, mean(rcauchy(nd, loc, s)))

configs <- list(
  list(dat = means_a, n = na, col = "tomato"),
  list(dat = means_b, n = nb, col = "darkorange"),
  list(dat = means_c, n = nc, col = "steelblue"),
  list(dat = means_d, n = nd, col = "seagreen")
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Test 5a.) Histograms with theoretical normal curve
plot_min <- -50
plot_max <- 50
bin_width <- 1

for (cfg in configs) {
  # Keep only values in the displayed range
  visible_data <- cfg$dat[
    cfg$dat >= plot_min & cfg$dat <= plot_max
  ]
  
  hist(
    visible_data,
    breaks = seq(plot_min, plot_max, by = bin_width),
    probability = TRUE,
    col = cfg$col,
    border = "white",
    main = paste("n =", cfg$n),
    xlab = "Sample mean",
    xlim = c(plot_min, plot_max),
    ylim = c(0, 0.35)
  )
  
  curve(
    dcauchy(x, location = loc, scale = s),
    from = plot_min,
    to = plot_max,
    add = TRUE,
    col = "firebrick",
    lwd = 2
  )
}

# Test 5b.) Normal Q-Q Plot
for (cfg in configs) {
  qqnorm(cfg$dat, main = paste("Q–Q Plot: n =", cfg$n),
         col = cfg$col, pch = 16, cex = 0.3)
  qqline(cfg$dat, col = "firebrick", lwd = 2)
}

results <- do.call(rbind, lapply(configs, function(cfg) {
  qq_table <- qq_difference_table(cfg$dat)
  
  data.frame(
    n = cfg$n,
    Total_Deviation_From_Ideal_Line = sum(qq_table$Difference)
  )
}))
rownames(results) <- NULL
print(results)

# Test 5c.) Skewness
skew <- function(x) {
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  m3 / m2^(3/2)
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  skewness = round(sapply(list(means_a, means_b, means_c, means_d), skew), 3)
)

# Test 5d.) Kurtosis
kurt <- function(x) {
  m4 <- mean((x - mean(x))^4)
  m2 <- mean((x - mean(x))^2)
  m4 / m2^2 - 3
}
data.frame(
  sample_size = c(na, nb, nc, nd),
  excess_kurtosis = round(sapply(list(means_a, means_b, means_c, means_d), kurt), 3)
)

# Test 5e.) Shapiro-Wilk Test
student_id <- 9438
set.seed(student_id)
sw <- function(x) {
  result <- shapiro.test(sample(x, 5000))
  c(W = round(result$statistic, 4), p_value = round(result$p.value, 4))
}
sw_results <- do.call(
  rbind,
  lapply(list(means_a, means_b, means_c, means_d), sw)
)
data.frame(sample_size = c(na, nb, nc, nd), sw_results, row.names = NULL)

