# ClickTEST
This is a R package developed and maintained by Yushu Zou. This package is aimed to provide the hypothesis test as well as correlation calculation for the data frame, the function inside will autotest all the assumptions for the test, and return multiple dataframe as the test result.

## Installment
```{r}
library(devtools)
devtools::install_github("reidbrok/ClickTest")
library(ClickTEST)
```
## Code Example
### Normal Test
```{r}
normality_test(df, threshold = 0.05)
```

will return with a list, calling `$normal` for a list of column names that are normally distributed; calling `$non_normal` for a list of column names that are not normally distributed

### One Sample Test
If the `num_var` parameter is not determined on the test, then all the columns in the data will entered and tested on the datatype as well as the normality test, and suitable columns will be selected.
```
### Parametric mean testing
normal_test(df, mu_values = c(1,1,1), threshold = 0.05)
```
perform one sample t-test for all normal distributed columns with null hypothesis $H_0: \mu = 1$

```
### Nonparametric sign testing
nonpar_test(df, mu_values = c(1,1,1), threshold = 0.05)
```
perform one-sample Wilcoxon test for all non-normal distributed columns with null hypothesis $H_0: \mu = 1$
```
### Parametric variance testing
normal_test(df, mu_values = c(1,1,1), threshold = 0.05, test = "variance")
```
perform one sample chi-square test for all normal distributed columns with null hypothesis $H_0: \sigma = 1$

### Multiple Group Test
```
### Parametric mean testing
normal_test(df, group_var = "group", threshold = 0.05, paired = F, exact = F, equal.variance = F)
```
Perform a multiple-group parametric test on strata on "group". If `equal. variance = F`, a bralette variance test will be tested as part of the result. 
If data in each group (more than three) has the same variance, then an ANOVA test and Turkey test result would be returned; otherwise, a Welch test and Games-Howell test will be returned.
If data that in each group (two groups), checking the pairedness and variance (F test), then a t-test will be performed.
Note, there is also a Multiple group test on variance, by adding the parameter `test = "variance"`. 

```
### Nonparametric mean testing
nonpar_test(df, group_var = "group", threshold = 0.05, paired = F, exact = F)
```
Perform a multiple-group nonparametric test on strata on "group". 
If data has more than three groups, then a Krustal test and Dunn's test result would be returned;
If data has two groups, a Wilcoxon test result will be returned based on the pairedness.
Note, there is also a Multiple group test on variance (Levene's), by adding the parameter `test = "variance"`. 

```
### Categorical Test
categorical_test(df, group_var = "group")
```
Perform a multiple-group categorical test on strata on categorical tests. If the contingency table has less than 10% of observations, a Fisher test will be performed while there is two groups, otherwise, a Chi-square test result will be returned.

### Correlation Matrix
```
correlation(df, base_cols = c("var1, var2"), other_cols = c("var3","var4"), method = "pearson")
```

