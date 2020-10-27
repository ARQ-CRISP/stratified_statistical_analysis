# Statistical analysis of stratified benchmarks

This repository contains R scripts allowing for statistical analysis of bechmark results obtained by a stratified approach.

## Regression with a single categorical variable

### Data as a contingency table
The method called `regression_contingency_table` runs a cumulative logit model over data presented as a contingency table. It should contain the number of ocurrences of each outcome (column) for each level of the explanatory variable (rows). If we assume that we have 3 levels to compare based on 4 outcomes, the dataframe (let's call it `dataframe_example`) should be formatted as follow:
|  levels | outcome 1 | outcome 2 | outcome 3 | outcome 4 |
|---------|-----------|-----------|-----------|-----------|
| level 1 | n11       | n12       | n13       | n14       |
| level 2 | n21       | n22       | n23       | n24       |
| level 3 | n31       | n32       | n33       | n34       |

In this case, the formula needs to contain indicator variables that we can manually define
```R
z1 <- ifelse(levels=="level 1",1,0)
z2 <- ifelse(levels=="level 2",1,0)
z3 <- ifelse(levels=="level 3",1,0)
```
Then running 
```R
res <- regression_contingency_table(cbind(outcome 1, outcome 2, outcome 3, outcome 4) ~ z1 + z2, dataframe_example, pod_assumption=TRUE)
```
would return all the pairwise comparisons (with level 3 as reference) with the corresponding p-values allowing you to rank the three levels over the entire distribution. Using `pod_assumption=FALSE` will return the pairwise comparisons values and p-values for each outcome separately.

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

then you need to be careful with the format of each column of your dataframe (let's call it `second_dataframe`). 

**Coming soon**

## Regression with several categorical variables
 Coming soon
