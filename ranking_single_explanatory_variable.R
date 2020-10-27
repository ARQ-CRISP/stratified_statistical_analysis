# Install VGAM if not already installed
list.of.packages <- c("VGAM")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Makes sure VGAM is loaded
require(VGAM)

# First function for which data are represented as a contigency table.
# Therefore, this function only supports regression with a single categorical explanatory variable, expressed using indicators (see examples)
# @param formula: Relation between the response and explanatory variables, must follow the syntax a~b.
# @param data: Data frame that contains the data used for the regression
# @param pod_assumption: Whether the regression should be run using the proportional odds assumption (TRUE or FALSE)
# @return: A named matrix with all the coefficients and their corresponding p-values
regression_contingency_table <- function(formula, data, pod_assumption){
  # Run the regression using vglm from the VGAM package
  regression_results <- vglm(formula, family = cumulative(parallel = pod_assumption), data=data)
  # Get the number of output
  output_number <- length(regression_results@coefficients)
  # Outcome numbers
  outcome_numbers <- length(regression_results@extra$colnames.y)
  # Extract only the coefficients that are related to the effect of each level of the explanatory variable (ignore the intercepts)
  effect_coefficents <- regression_results@coefficients[outcome_numbers:output_number]
  # Extract the number of levels of the explanatory variable
  number_levels <- length(regression_results@assign)
  # Name given to each level (expect the one taken as reference)
  effect_names <- names(effect_coefficents)
  # Values corresponding to each level (expect the one taken as reference)
  effect_values <-unname(effect_coefficents)
  # Create a vector initialized with the same values as the effect of each level against the reference
  all_values <- effect_values
  # Vector initialized with the same of each level (except the reference)
  all_names <- effect_names
  # Get the pvalues of the coefficients for each level (except the reference)
  pvalues <- unname(summary(regression_results)@coef3[outcome_numbers:output_number, 4])
  # Get the variance covariance values for the effect coefficients
  cov_mat = vcov(regression_results)[outcome_numbers:output_number, outcome_numbers:output_number]
  if (pod_assumption){
    # For each pairwise difference compute the value and associated p-value of the effect
    for (index_ref in 1:(number_levels-2))
    {
      for (second_index in (index_ref+1):(number_levels-1))
      {
        # Append the difference between two effects
        all_values <- c(all_values, effect_values[index_ref] - effect_values[second_index])
        # Get the corresponding name
        all_names <- c(all_names, paste(effect_names[index_ref], "-", effect_names[second_index]))
        # Compute the z^2 statistics that allows to get the corresponding pvalues
        zsqaure <- (tail(all_values, n=1))**2/(cov_mat[index_ref, index_ref] + cov_mat[second_index, second_index] - 2*cov_mat[index_ref, second_index])
        # Append the pvalue
        pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1))
      }
    }
  }
  else
  {
    number_sub_outcomes <- outcome_numbers - 1
    for (level_ref in 1:(number_levels-2))
    {
      for (second_level in (level_ref + 1):(number_levels - 1))
      {
        for (sub_outcome in 1:number_sub_outcomes)
        {
          # Index of the level+outcome we want to compare with the other
          ref_index <- (level_ref - 1) * number_sub_outcomes + sub_outcome
          # Index of the level+outcome that is being compared to the reference
          second_index <- (second_level - 1) * number_sub_outcomes + sub_outcome
          # Append the difference between two level+outcomes (for the same outcomes)
          all_values <- c(all_values, effect_values[ref_index] - effect_values[second_index])
          # Get the corresponding name
          all_names <- c(all_names, paste(effect_names[ref_index], "-", effect_names[second_index]))
          # Compute the z^2 statistics that allows to get the corresponding pvalues
          zsqaure <- (tail(all_values, n=1))**2/(cov_mat[ref_index, ref_index] + cov_mat[second_index, second_index] - 2*cov_mat[ref_index, second_index])
          # Append the pvalue
          pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1))
        }
      }
    }
  }
  # Concatenate everything inside a matrix
  final_matrix <- matrix(c(all_values, pvalues), ncol = 2)
  # Name the columns and rows for a nice display
  colnames(final_matrix) <- c("value", "p-value")
  rownames(final_matrix) <- all_names
  # Return the whole matrix
  return(final_matrix)
}
