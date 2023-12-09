wilcox_test<- function(df,num_var, group_var, paired, exact,alternative,mu_values, threshold){
  wilcox_results <- list()
  mu_values <- setNames(mu_values, num_var)
  if(length(levels(df[[group_var]])) == 2){
    for (col_name in num_var) {
      ### one sample wilcox test
      test_result <- wilcox.test(reformulate(group_var, response = col_name), data = df,paired = paired,exact= exact, alternative = alternative,conf.level = 1- threshold)
      wilcox_results[[col_name]] <- test_result$p.value
    }
  }
 else{
    for (col_name in num_var) {
      test_result <- wilcox.test(df[[col_name]], mu = mu_values[[col_name]], alternative = alternative,conf.level = 1- threshold, exact = exact)
      wilcox_results[[col_name]] <- test_result$p.value
    }
  }
  result_df <- data.frame(p_value = unlist(wilcox_results))
  result_df[["significate"]] = ifelse(result_df[["p_value"]] < threshold, "***", "")
  return (result_df)
}
