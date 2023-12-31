leneve_test <- function(df, num_var, group_var, threshold){
  leneve_results <- list()
  for (col_name in num_var) {
  # Check for equal variances for Independent T-Test
    levene_test <- car::leveneTest(df[[col_name]] ~ df[[group_var]], center = mean)
    leneve_results[[col_name]] <- levene_test$`Pr(>F)`[1]
  }
  result_df <- data.frame(p_value = unlist(leneve_results))
  result_df[["significate"]] = ifelse( result_df[["p_value"]] < threshold, "***", "")
  return(result_df)
}
