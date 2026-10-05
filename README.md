Project title:
"Project 01 - Monte Carlo Simulation"

## What is the project about?
- The project is about testing the Central Limit Theorem with different distribution populations.
  The Central Limit Theorem states that for any given distribution, the distribution of sample
  means approaches a normal distribution as the sample size gets larger.
  
## What I am investigating?
- The project aims to address one question: for a given distribution, what sample size can
  you use at minimum to say that the sample means distribution is "normal"?
  
## Brief description of methods:
- Histograms:
  - Create a histogram for each sample-means distribution of each n and
  draw alongside each histogram a theoretical normal curve
- Q-Q plots and table: 
  - Create a plot of a red normal regression line for each sample-means distribution
  of each n and measure the total deviation of all sample means from the red line
- Skewness: 
  - Measure skewness for each sample-means distribution at a given n
- Kurtosis:
  - Measure kurtosis for each sample-means distribution at a given n
- Shapiro-Wilk test:
  - Measure a test-statistic (W.W.) and a p-value for each sample-means distribution
  at a given n. Set up a null hypothesis (H<sub>O</sub>: data is normal)
  and an alternative hypothesis (H<sub>A</sub>: data is non-normal) for the test.
  P-value determines if null hypothesis is rejected (if p > 0.05) or is "failed to reject"
  (if p <= 0.05).

## Important results/conclusions
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