twosample_t <- function(df,num_var, group_var, paired, alternative, threshold, test){
  t_results <- list()
  for (col_name in names(df%>% dplyr::select(all_of(num_var)))) {
    if (test == "mean"){
    group1 <- df[df[[group_var]] == levels(df[[group_var]])[1], col_name]
    group2 <- df[df[[group_var]] == levels(df[[group_var]])[2], col_name]
    ### one sample t test
    if (paired) {
      # #perform Welch's two sample t-test (Paired)
      test_result <- t.test(group1, group2, paired = TRUE, alternative = alternative)
    } else {
      # Check for equal variances for Independent T-Test
      f_test <- var.test(df[[col_name]] ~ df[[group_var]], alternative = alternative, conf.level = 1 - threshold)
      equal_variances <- f_test$p.value > threshold

      # #perform independent samples t-test
      test_result <- t.test(group1, group2, var.equal = equal_variances, alternative = alternative)
      }
    t_results[[col_name]] <- test_result$p.value
    }
  else{
    f_test <- var.test(df[[col_name]] ~ df[[group_var]], alternative = alternative, conf.level = 1 - threshold)
    t_results[[col_name]] <- f_test$p.value
  }
  }
  result_df <- data.frame(p_value = unlist(t_results))
  result_df[["significate"]] = ifelse(result_df[["p_value"]]< threshold, "***", "")
  return (result_df)
}


