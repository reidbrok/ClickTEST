Dunn_test <- function(df,var,group_var,method, threshold){
  dunn_results <- list()

  for (col_name in var){

    test_result <- dunn.test::dunn.test(x = df[[col_name]],g= df[[group_var]],method=method, alpha = threshold)
    temp <- test_result$P.adjusted
    temp <- setNames(temp,test_result$comparisons)
    dunn_results[[col_name]]$"Dunn's Test" <- create_matrix(temp)
    dunn_results[[col_name]]$"Signicate: Dunn's Test" <-ifelse(create_matrix(temp) < threshold, "***", "")
  }
  return(dunn_results)
}

