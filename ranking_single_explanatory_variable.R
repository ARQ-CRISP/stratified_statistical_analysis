# Install VGAM if not already installed
list.of.packages <- c("VGAM")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Makes sure VGAM is loaded
require(VGAM)

# This function only supports regression with a single explanatory variable. The data can be provided following two different formats; as a contingency table or as a list of experiments (see the examples in the README).
# @param formula: Relation between the response and explanatory variable, must follow the syntax a~b.
# @param data: Data frame that contains the data used for the regression
# @param pod_assumption: Whether the regression should be run using the proportional odds assumption (TRUE or FALSE)
# @return: A named matrix with all the coefficients and their corresponding p-values
regression_single_variable <- function(formula, data, pod_assumption){
  # Run the regression using vglm from the VGAM package
  regression_results <- vglm(formula, family = cumulative(parallel = pod_assumption), data=data)
  # Get the number of output
  output_number <- length(regression_results@coefficients)
  # Outcome numbers
  outcome_numbers <- length(regression_results@extra$colnames.y)
  # Extract only the coefficients that are related to the effect of each level of the explanatory variable (ignore the intercepts)
  effect_coefficents <- regression_results@coefficients[outcome_numbers:output_number]
  # Extract the number of levels of the explanatory variable
  number_levels <- sum(lengths(regression_results@assign))
  # Name given to each level (except the one taken as reference)
  effect_names <- names(effect_coefficents)
  if (!pod_assumption){
    for (i in 1:length(effect_names)){
      split_coefficient <- as.list(strsplit(effect_names[i], ':')[[1]])
      index_outcome = strtoi(split_coefficient[2])
      effect_names[i] <- paste(split_coefficient[1], regression_results@extra$colnames.y[index_outcome], sep=":")
    } 
  }
  # Values corresponding to each level (except the one taken as reference)
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



# This function supports regression two explanatory variables. The data can be provided following two different formats; a contingency-like table or as a list of experiments (see the examples in the README).
# @param formula: Relation between the response and explanatory variables, must follow the syntax a~b*c. b must be the main explanatory variable corresponding to the thetas
# @param data: Data frame that contains the data used for the regression
# @param include_pure_etas: Whether to also display the values corresponding to the etas (i.e. the second variable) WITHOUT the interaction term
# @return: A named matrix with all the coefficients and their corresponding p-values
regression_multi_variables <- function(formula, data, include_pure_etas=FALSE){
  regression_results <- vglm(formula, family = cumulative(parallel = TRUE), data=data)
  nb_unique <- length(regression_results@misc$colnames.x)
  # Extract all necessary information if the data is provided as a contingency-like table (this if) or if the data is provided as a a table with one trial per row (in else)
  if (lengths(regression_results@assign[2]) == 1){
    number_total_single_effects <- which(unlist(gregexpr(":", names(regression_results@assign))) != -1)[1]
    number_effect_variable1 <- length(grep(names(regression_results@assign)[number_total_single_effects-1], names(regression_results@assign)[number_total_single_effects:nb_unique])) + 1
    number_effect_variable2 <- number_total_single_effects - number_effect_variable1
    effect_variable1_names <- names(regression_results@assign)[2:number_effect_variable1]
    effect_variable2_names <- names(regression_results@assign)[number_effect_variable1+1:number_total_single_effects-1]
  } else {
    number_effect_variable1 <- lengths(regression_results@xlevels[1])
    number_effect_variable2 <- lengths(regression_results@xlevels[2])
    number_total_single_effects <- number_effect_variable1 + number_effect_variable2
    effect_variable1_names <- unlist(unname(no_contingency@xlevels[1]))[2:number_effect_variable1]
    effect_variable2_names <- unlist(unname(no_contingency@xlevels[2]))[2:number_effect_variable2]
    }
  # Get the number of output
  output_number <- length(regression_results@coefficients)
  # Outcome numbers
  outcome_numbers <- length(regression_results@extra$colnames.y)

  pure_effect_coefficents <- regression_results@coefficients[outcome_numbers:(outcome_numbers+number_total_single_effects-3)]
  mixed_effect_coefficients <- regression_results@coefficients[(outcome_numbers+number_total_single_effects-2):output_number]

  # Name given to each level (except the one taken as reference)
  pure_effect_names <- names(pure_effect_coefficents)
  mixed_effect_names <- names(mixed_effect_coefficients)
  pure_effect_values <- unname(pure_effect_coefficents)
  if (include_pure_etas == FALSE)
  {
    effect_names <- pure_effect_names[1:(number_effect_variable1-1)]
    effect_values <- pure_effect_values[1:(number_effect_variable1-1)]
  }
  else
  {
    effect_names <- c(pure_effect_names)
    effect_values <- c(pure_effect_values)
  }
  # Create a vector initialized with the same values as the effect of each level against the reference
  all_values <- effect_values
  # Vector initialized with the same of each level (except the reference)
  all_names <- effect_names
  # Get the pvalues of the coefficients for each level (except the reference)
  pure_effect_pvalues <- unname(summary(regression_results)@coef3[outcome_numbers:(outcome_numbers+number_total_single_effects-3), 4])
  if (include_pure_etas==TRUE)
  {
    pvalues <- c(pure_effect_pvalues)
  }
  else {
    pvalues <- pure_effect_pvalues[1:(number_effect_variable1-1)]
  }
  # Get the variance covariance values for the effect coefficients
  # For pure effects only
  cov_mat_pure = vcov(regression_results)[outcome_numbers:(outcome_numbers+number_total_single_effects-3), outcome_numbers:(outcome_numbers+number_total_single_effects-3)]
  # For interaction terms only
  cov_mat_mixed = vcov(regression_results)[(outcome_numbers+number_total_single_effects-2):output_number, (outcome_numbers+number_total_single_effects-2):output_number]
  # Covariance between pure and interaction terms
  cov_mat_cross = vcov(regression_results)[outcome_numbers:(outcome_numbers+number_total_single_effects-3), (outcome_numbers+number_total_single_effects-2):output_number]

  # For each pairwise difference in first variable compute the value and associated p-value of the effect
  for (index_ref in 1:(number_effect_variable1-2))
  {
    for (second_index in (index_ref+1):(number_effect_variable1-1))
    {
      # Append the difference between two effects
      all_values <- c(all_values, effect_values[index_ref] - effect_values[second_index])
      # Get the corresponding name
      all_names <- c(all_names, paste(effect_names[index_ref], "-", effect_names[second_index]))
      # Compute the z^2 statistics that allows to get the corresponding pvalues
      zsqaure <- (tail(all_values, n=1))**2/(cov_mat_pure[index_ref, index_ref] + cov_mat_pure[second_index, second_index] - 2*cov_mat_pure[index_ref, second_index])
      # Append the pvalue
      pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1))
    }
  }
  # Do the same for etas if required
  if (include_pure_etas == TRUE){
    for (index_ref in (number_effect_variable1+1):(number_effect_variable2-2))
    {
      for (second_index in (index_ref+1):(number_effect_variable2-1))
      {
        # Append the difference between two effects
        all_values <- c(all_values, effect_values[index_ref] - effect_values[second_index])
        # Get the corresponding name
        all_names <- c(all_names, paste(effect_names[index_ref], "-", effect_names[second_index]))
        # Compute the z^2 statistics that allows to get the corresponding pvalues
        zsqaure <- (tail(all_values, n=1))**2/(cov_mat_pure[index_ref, index_ref] + cov_mat_pure[second_index, second_index] - 2*cov_mat_pure[index_ref, second_index])
        # Append the pvalue
        pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1))
      }
    }
  }

  # Simple interaction between single theta and single eta
  for (var1_index in 1:(number_effect_variable1-1))
  {
    for (var2_index in 1:(number_effect_variable2-1))
    {
      combined_index_mixed <- match(paste(pure_effect_names[var1_index], pure_effect_names[number_effect_variable1-1 + var2_index], sep=":"), mixed_effect_names)
      # Append the sum between two effects
      all_values <- c(all_values, pure_effect_coefficents[number_effect_variable1-1 + var2_index] + mixed_effect_coefficients[combined_index_mixed])
      # Get the corresponding name
      all_names <- c(all_names, paste(pure_effect_names[var1_index], pure_effect_names[number_effect_variable1-1 + var2_index], sep=":"))
      # Compute the z^2 statistics that allows to get the corresponding pvalues
      zsqaure <- (tail(all_values, n=1))**2/(cov_mat_pure[number_effect_variable1-1 + var2_index, number_effect_variable1-1 + var2_index] + cov_mat_mixed[combined_index_mixed, combined_index_mixed] + 2*cov_mat_cross[number_effect_variable1-1 + var2_index, combined_index_mixed])
      # Append the pvalue
      pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1))
    }
  }

  # Interaction between difference of two thetas for a single eta
  for (index_ref in 1:(number_effect_variable1-2))
  {
    for (second_index in (index_ref+1):(number_effect_variable1-1))
    {
      for (var2_index in 1:(number_effect_variable2-1))
      {
        combined_index_mixed_first <- match(paste(pure_effect_names[index_ref], pure_effect_names[number_effect_variable1-1 + var2_index], sep=":"), mixed_effect_names)
        combined_index_mixed_second <- match(paste(pure_effect_names[second_index], pure_effect_names[number_effect_variable1-1 + var2_index], sep=":"), mixed_effect_names)
        # Append the difference between two effects
        all_values <- c(all_values, mixed_effect_coefficients[combined_index_mixed_first] - mixed_effect_coefficients[combined_index_mixed_second])
        # Get the corresponding name
        all_names <- c(all_names, paste(paste(effect_names[index_ref], effect_names[second_index], sep="-"), pure_effect_names[number_effect_variable1-1 + var2_index], sep=":"))
        # Compute the z^2 statistics that allows to get the corresponding pvalues
        zsqaure <- (tail(all_values, n=1))**2/(cov_mat_mixed[combined_index_mixed_first, combined_index_mixed_first] + cov_mat_mixed[combined_index_mixed_second, combined_index_mixed_second] - 2*cov_mat_mixed[combined_index_mixed_first, combined_index_mixed_second])
        # Append the pvalue
        pvalues <- c(pvalues, 1 - pchisq(zsqaure, 1)) 
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

