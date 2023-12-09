Kruskal_test <- function(df, num_var,group_var, threshold){
  Krustal_results <- list()
  for (col_name in num_var) {
    formula <- as.formula(paste(col_name, "~", group_var))
    test_result <- kruskal.test(formula, data = df)
    Krustal_results[[col_name]] <- test_result$p.value
  }
  Result <- data.frame(p_value = unlist(Krustal_results))
  Result[["significate"]] = ifelse(Result[["p_value"]] < threshold, "***", "")
  return(Result)
}
