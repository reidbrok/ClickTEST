onesample_t <- function(df,num_var, paired, exact,alternative,mu_values, threshold){
  t_results <- list()
  for (col_name in num_var) {
    ### one sample t test
    test_result <- t.test(df[[col_name]], mu = mu_values[[col_name]], alternative = alternative)
    t_results[[col_name]] <- test_result$p.value
  }
  result_df <- data.frame(p_value = unlist(t_results))
  result_df[["significate"]] = ifelse(result_df[["p_value"]] < threshold, "***", "")
  return (result_df)
}
