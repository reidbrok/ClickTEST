chisquare <- function(df, cat_var, group_var, threshold, correct){
  chi_square.result = list()
  for (col_name in cat_var){
    temp_conti_table = table(df[[col_name]], df[[group_var]])
    test_result <- chisq.test(temp_conti_table,correct = correct)
    chi_square.result[[col_name]] <- test_result$p.value
  }
  result_df <- data.frame(p_value = unlist(chi_square.result))
  result_df[["significate"]] = ifelse(result_df[["p_value"]] < threshold, "***", "")
  return (result_df)
}

