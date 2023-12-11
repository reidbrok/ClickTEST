group_norm <- function(df,num_var, group_var, test, threshold,equal_variances, alternative, method){
  group_results <- data.frame(row.names = num_var)
  post_results <- list()
  for (col_name in num_var) {
    formula <- as.formula(paste(col_name, "~", group_var))
    if (equal_variances | test != "mean"){ # test for variance
      barlett_test <- bartlett.test(formula, df)
      equal_variances <- barlett_test$p.value > threshold
      group_results[col_name,"Barlett Test on Variance"]<- barlett_test$p.value
      group_results[col_name,"Signicate: Barlett Test on Variance"] <-ifelse(barlett_test$p.value < threshold, "***", "")
    }
    if (test == "mean"){
      if (equal_variances){
        test_result <- aov(formula, data = df)
        group_results[col_name,"ANOVA"]<- summary(test_result)[[1]]$'Pr(>F)'[1]
        group_results[col_name,"Signicate: ANOVA"] <-ifelse(summary(test_result)[[1]]$'Pr(>F)'[1] < threshold, "***", "")
        if (summary(test_result)[[1]]$'Pr(>F)'[1]< threshold){
          tukey_result <- TukeyHSD(test_result)
          post_results[[col_name]]$"Tukey's HSD" <- create_matrix(tukey_result$group[,4])
          post_results[[col_name]]$"Signicate: Tukey's HSD" <-ifelse(create_matrix(tukey_result$group[,4]) < threshold, "***", "")
        }
      }
      else{
        test_result <- onewaytests::welch.test(formula, data = df)
        group_results[col_name,"Welch"] <- test_result$p.value
        group_results[col_name,"Signicate: Welch"] <-ifelse(test_result$p.value < threshold, "***", "")
        if (test_result$p.value < threshold){
          gh_result <- PMCMRplus::gamesHowellTest(x = df[[col_name]], g = df[[group_var]], alternative = alternative, p.adjust.method = method)
          post_results[[col_name]]$"Games-Howell" <- gh_result$p.value
        }
      }
    }
  }
  if (test == "variance"){
    return(group_results)
  }
  else{
  #post_results_sign <- ifelse(post_results < threshold/length(unique(df[[group_var]]))-1, "***","")
  return (list(group_results,post_results))
  }
}
