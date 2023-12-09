fisher <- function(df, cat_var, group_var, threshold, correct = correct, alternative){
    fisher_results <- list()
    for (col_name in cat_var){
      temp_conti_table = table(df[[col_name]], df[[group_var]])
      if (sum(temp_conti_table < 0.1 * nrow(df)) == 0){
        test_result <- chisq.test(temp_conti_table,correct = correct)
      }
      else{
        test_result <- fisher.test(temp_conti_table,conf.level = 1- threshold, alternative = alternative)
      }
      fisher_results[[col_name]] <- test_result$p.value
    }
    result_df <- data.frame(p_value = unlist(fisher_results))
    result_df[["significate"]] = ifelse(result_df[["p_value"]] < threshold, "***", "")
    return (result_df)
}
