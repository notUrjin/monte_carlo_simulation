---
title: "Final Summary Document"
output: html_document
---

| Population | Smallest (n) judged normal | Skewness at chosen (n) | Kurtosis | Key observation |
| --- | --- | --- | --- | --- |
| Standard Normal | 20 | 0.019 | 0.003 | Symmetrical at mean = 0 |
| Six-Sided Die | 78 | 0.004 | -0.033 | Requires more than twice of standard normal's sample size to approximate to normal |
| Exponential | 300 | 0.107 | -0.005 | Sample-means distribution was right-skewed with small sample size |
| Binomial Small | 400 | 0.022 | 0.000 | The shape of the sample-means distribution at n = 200 and n = 400 looks similar to the sample-means distributions of the Poisson distribution |
| Cauchy | 1 | -5.466 | 601.582 | Approximation to normal distribution got closer as n got smaller |

## 11.1 Sample Size
<p>
  No, there is not evidence for a universal sample size such as n = 30.
</p>

## 11.2 Population Shape
<p>
  The populations that converged the quickest (in order from fastest to slowest)
  were Cauchy, Standard Normal, and Six-Sided Die.
  The populations that converged the slowest (from slowest to fastest) were Binomial Small and
  Exponential.
  The main reason certain populations converge to a normal distribution
  faster (or slower) than other populations is because of differences
  in skewness, symmetry, and kurtosis in the original population distribution.
  If a population is distributed symmetrically (e.g. Standard Normal and Six-Sided Die [Discrete Uniform])
  then there are equal deviations on both sides of the mean, thus requiring a smaller sample size to
  appear normal.
  If a population has heavy skewness (e.g. Exponential and Binomial Small),
  the unevenly-distributed tails can influence the skewness of the sample-means distribution (often
  at smaller sample sizes), thus requiring a larger sample size to appear normal.
</p>

## 11.3 Assumptions
<p>
  Extremely heavy tails tend to yield a large quantity of excess kurtosis. This means that
  there is a greater-than-normal density of extreme values at the tails of the distribution.
</p>

## Conclusion regarding the Central Limit Theorem
<p>
  After completing all analyses, I gained a better understanding of the Central Limit Theorem.
  I can confirm that a distribution's approximation to a normal distribution depends on how
  large n (i.e. sample size) is; the larger that n is, the closer the distribution approximates to
  a normal distribution. 
</p>
<p>
  I also can confirm that n = 30 is not always the minimal sample size for a population's sample
  distribution to approximate to normal. Each distribution has its own characteristics such as 
  skewness and symmetry that influence how quickly it can converge to normality. 
</p>
<p>
  In conclusion, the Central Limit Theorem remains as a key theorem in statistical analysis.
  The Monte Carlo simulations show the CLT in action with different populations
  and how those populations converge to normality, just at different sample sizes.
</p>