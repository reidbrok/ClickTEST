chisquare_onesample <- function(df,num_var, alternative, mu_values, threshold){
  chisquare_results <- list()
  for (col_name in num_var) {
    if (mu_values[[col_name]] <= 0){
      stop("the null hypothesis for variance test must be a single positive numeric value.")
    }
    ### one sample t test
    test_result <- EnvStats::varTest(df[[col_name]], sigma.squared = mu_values[[col_name]], alternative = alternative, conf.level = 1-threshold)
    chisquare_results[[col_name]] <- test_result$p.value
  }
  result_df <- data.frame(p_value = unlist(chisquare_results))
  result_df <- result_df %>% mutate(significate = ifelse(p_value < threshold, "***", ""))
  return (result_df)
}
