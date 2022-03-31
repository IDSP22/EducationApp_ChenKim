---
output: 
  html_document:
    keep_md: TRUE
---

# Calculating the Odds Ratio (OR)

&nbsp;

Odds ratios (OR) can be calculated using a two-by-two frequency table (also called cross-tabulation or contingency table)

<img src="/Users/sookim/Documents/Courses/2nd_year/R Shiny/Assignment/EducationApp_ChenKim/images/fig_definition.jpg" width="96%" />

* Odds of event under exposure to treatment = $\frac{a}{b}$
* Odds of event without exposure to treatment = $\frac{c}{d}$
* Odds ratio of event between treatment and control = $\frac{\frac{a}{b}}{\frac{c}{d}} = \frac{ac}{bd}$

### Let’s look at the previous example: 

Upon calling several friends who attended the party, Beyonce and Jay-Z gathered information that 8 out of 27 friends who ate pizza at the party got sick, and 4 out of 31 friends who did not recall eating pizza got sick.
