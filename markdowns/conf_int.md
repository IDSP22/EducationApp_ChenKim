---
output: 
  html_document:
    keep_md: TRUE
---

# Confidence Interval

&nbsp;

When sample population is used to calculate the OR, The confidence interval (CI) is used to estimate the precision of the OR. A large CI indicates a low level of precision of the OR, whereas a small CI indicates a higher precision of the OR.
Under the assumption of multinomial sampling, the log-odds ratio (log OR) has a better normal approximation than natural OR does. Therefore, we usually obtain a confidence interval on the log scale (log here means natural log). Variance of the log OR is calculated as below:

$$
\hat{V}(log\text{ }{\hat\theta})=\frac{1}{a} + \frac{1}{b} + \frac{1}{c} +\frac{1}{d}
$$

The most common level of confidence used is 95% CI, which corresponds to the Z-score value of 1.96. We get a 95% confidence interval of the OR by exponentiating the endpoints of:

$$
log\text{ }{\hat\theta} \pm 1.96 \sqrt{\frac{1}{a} + \frac{1}{b} + \frac{1}{c} +\frac{1}{d}}
$$

Specifying different confidence level only changes the value 1.96 in the above equation. You can get the corresponding Z-score of your confidence level of choice in R by:


```r
conf.level <- 0.95 # confidence level to be specified
abs(qnorm((1-conf.level)/2))
```

### Let’s go back to the previous example

Specify the sample size and the confidence level for the investigation of party attendants using the sliders. 
