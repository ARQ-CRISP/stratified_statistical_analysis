# Statistical analysis of stratified benchmarks

This repository contains R scripts allowing for statistical analysis of benchmark results obtained by a stratified approach.

## Regression with a single categorical variable

The method called `regression_single_variable` runs a cumulative logit model over data explained by a single variable. The input data can take two formats: a contingency table or a dataframe with one trial per row.

### Data as a contingency table
The dataframe should represent the contingency table, i.e. should contain the number of occurrences of each outcome (column) for each level of the explanatory variable (rows). If we assume that we have 3 levels to compare based on 4 outcomes, the dataframe (let's call it `dataframe_example`) should be formatted as follow (make sure to have `levels` set a factor):
|  levels | outcome 1 | outcome 2 | outcome 3 | outcome 4 |
|---------|-----------|-----------|-----------|-----------|
| level 1 | n11       | n12       | n13       | n14       |
| level 2 | n21       | n22       | n23       | n24       |
| level 3 | n31       | n32       | n33       | n34       |

In this case, the formula needs to contain indicator variables that we can manually define
```R
level1 <- ifelse(dataframe_example$levels=="level 1",1,0)
level2 <- ifelse(dataframe_example$levels=="level 2",1,0)
level3 <- ifelse(dataframe_example$levels=="level 3",1,0)
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

## Regression with several categorical variables

The method called `regression_multi_variables` runs a cumulative logit model over data explained by the joint effect of two variables. The input data can take two formats: a contingency-like table or a dataframe with one trial per row.

### Data as a contingency-like table

If we assume that we have two variables, one with 3 levels and a second with 4 levels that we want to analyse based on 4 outcomes, the dataframe (let's call it `dataframe_multi_example`) should be formatted as follow (make sure to have either `levels` and `second_variable` set as factors if you want them to be treated as categorical variables):
|  levels | second_variable |outcome 1 | outcome 2 | outcome 3 | outcome 4 |
|---------|-----------------|-----------|-----------|-----------|-----------|
| level 1 |       var1      | n111      | n112      | n113      | n114      |
| level 1 |       var2      | n121      | n122      | n123      | n124      |
| level 1 |       var3      | n131      | n132      | n133      | n134      |
| level 1 |       var4      | n141      | n142      | n143      | n144      |
| level 2 |       var1      | n211      | n212      | n213      | n214      |
| level 2 |       var2      | n221      | n222      | n223      | n224      |
| level 2 |       var3      | n231      | n232      | n233      | n234      |
| level 2 |       var4      | n241      | n242      | n243      | n244      |
| level 3 |       var1      | n311      | n312      | n313      | n314      |
| level 3 |       var2      | n321      | n322      | n323      | n324      |
| level 3 |       var3      | n331      | n332      | n333      | n334      |
| level 3 |       var4      | n341      | n342      | n343      | n344      |

In this case, the formula needs to contain indicator variables that we can manually define
```R
level1 <- ifelse(dataframe_multi_example$levels=="level 1",1,0)
level2 <- ifelse(dataframe_multi_example$levels=="level 2",1,0)
level3 <- ifelse(dataframe_multi_example$levels=="level 3",1,0)
second_var1 <- ifelse(dataframe_multi_example$second_variable=="var1",1,0)
second_var2 <- ifelse(dataframe_multi_example$second_variable=="var2",1,0)
second_var3 <- ifelse(dataframe_multi_example$second_variable=="var3",1,0)
second_var4 <- ifelse(dataframe_multi_example$second_variable=="var4",1,0)
```
Then running
```R
res <- regression_multi_variables(cbind(outcome 1, outcome 2, outcome 3, outcome 4) ~ (level1 + level2)*(second_var1 + second_var2 +second_var3), dataframe_multi_example)
```
would return all the pairwise comparisons (with level 3 as reference for `levels` and `var4` as reference for `second_variable`) with the corresponding p-values allowing you to account for the interaction between the two variables. Note that if `second_variable` is numerical you don't need to use the corresponding set of indicators and use directly

```R
res <- regression_multi_variables(cbind(outcome 1, outcome 2, outcome 3, outcome 4) ~ (level1 + level2)*(second_var)
```

### Data with one trial per row
If the dataframe follows this format:

| levels  | second_variable | outcome   |
|---------|---------------- | -----------|
| level 1 | var1            | outcome 1 |
| level 1 |  var3           | outcome 2 |
| level 1 | var2            | outcome 1 |
| ...     | ...             | ...       |
| level 2 |  var4           | outcome 4 |
| ...     |       ...       | ...       |

then you need to be careful with the format of each column of your dataframe (let's call it `second_dataframe_multi`). Indeed, `levels` and `second_variable` must be set as factors (the first level will set as a reference), and `outcome` should be an ordered factor. If the data follow these conditions, then you can call the same function as follows:
```R
res <- regression_single_variable(outcome ~ levels*second_variable, second_dataframe_multi)
```
Please note that if `second_variable` is not categorical, you can use the same function here!

Feel free to compare the output with the one obtained with a contingency table and it will be the same!
