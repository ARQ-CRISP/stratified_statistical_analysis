# Statistical analysis of stratified benchmarks

This repository contains R scripts allowing for statistical analysis of benchmark results obtained by a stratified approach.

## Regression with a single categorical variable

The method called `regression_single_variable` runs a cumulative logit model over data explained by a single variable. The input data can take two formats: a contingency table or a dataframe with one trial per row.

### Data as a contingency table
 The dataframe should represent the contingency table, i.e. should contain the number of occurrences of each outcome (column) for each level of the explanatory variable (rows). If we assume that we have 3 levels to compare based on 4 outcomes, the dataframe (let's call it `dataframe_example`) should be formatted as follow:
|  levels | outcome 1 | outcome 2 | outcome 3 | outcome 4 |
|---------|-----------|-----------|-----------|-----------|
| level 1 | n11       | n12       | n13       | n14       |
| level 2 | n21       | n22       | n23       | n24       |
| level 3 | n31       | n32       | n33       | n34       |

In this case, the formula needs to contain indicator variables that we can manually define
```R
level1 <- ifelse(levels=="level 1",1,0)
level2 <- ifelse(levels=="level 2",1,0)
level3 <- ifelse(levels=="level 3",1,0)
```
Then running
```R
res <- regression_single_variable(cbind(outcome 1, outcome 2, outcome 3, outcome 4) ~ level1 + level2, dataframe_example, pod_assumption=TRUE)
```
would return all the pairwise comparisons (with level 3 as reference) with the corresponding p-values allowing you to rank the three levels over the entire distribution. Using `pod_assumption=FALSE` will return the pairwise comparisons values and p-values for each outcome separately.

Note that if you want to have level 1 set as reference, you should call the function as follows:
```R
res <- regression_single_variable(cbind(outcome 1, outcome 2, outcome 3, outcome 4) ~ level2 + level3, dataframe_example, pod_assumption=TRUE)
```

### Data with one trial per row
If the dataframe follows this format:

| levels  | outcome   |
|---------|-----------|
| level 1 | outcome 1 |
| level 1 | outcome 2 |
| level 1 | outcome 1 |
| ...     | ...       |
| level 2 | outcome 4 |
| ...     | ...       |

then you need to be careful with the format of each column of your dataframe (let's call it `second_dataframe`). Indeed, `levels` must be a factor (the first provided level will set as a reference), and `outcome` should be an ordered factor. If the data follow these conditions, then you can call the same function as follows:
```R
res <- regression_single_variable(outcome ~ levels, second_dataframe, pod_assumption=TRUE)
```
Feel free to compare the output with the one obtained with a contingency table and it will be the same!

**Coming soon**

## Regression with several categorical variables
 Coming soon
