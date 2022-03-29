---
output: 
  html_document:
    keep_md: TRUE
---

# Calculating the Odds Ratio (OR)

&nbsp;

Odds ratios (OR) can be calculated using a two-by-two frequency table (also called cross-tabulation or contingency table)

<img src="/Users/sookim/Documents/Courses/2nd_year/R Shiny/Assignment/OR_app/images/fig_definition.jpg" width="60%" />

* Odds of event under exposure to treatment = $\frac{a}{b}$
* Odds of event without exposure to treatment = $\frac{c}{d}$
* Odds ratio of event between treatment and control = $\frac{\frac{a}{b}}{\frac{c}{d}} = \frac{ac}{bd}$

### Let’s look at the previous example: 

Upon calling all her friends who attended the party, Ruby gathered information that 4 out of 12 friends who ate pizza at the party got sick, and 3 out of 8 friends who did not eat pizza got sick.

