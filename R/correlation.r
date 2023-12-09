#' @title Correlation Calculation for different data type
#' @description a function that perform multiple correlation calculation, accept "pearson", "kendall", "spearman"
#' @param df Dataframe
#' @param base_cols a list of column name as the baseline covariates
#' @param other_cols a list of column name as the other covariates
#' @param method the default method for calculate the correlation, default value is "pearson"
#' @param special_methods a list with name of different correlation method, and value of multiple column names
#' @return a list of dataframe for every column in the based_cols, contains hte correaltion index.
#' @author Yushu Zou
#' @examples
#' df <- data.frame(col1 = rnorm(100), col2 = rnorm(100), col3 = rnorm(100))
#' result <- correlation(df, c("col1", "col2"), c("col2", "col3"))
#' @export
#' @import stats

correlation <- function(df, base_cols, other_cols, method = "pearson", special_methods = list()) {
  results <- list()

  for (base_col in base_cols) {
    base_results <- lapply(other_cols, function(col) {
      # Skip if base_col and col are the same
      if (base_col == col) {
        return(list(correlation = 1, missing_count_col = NA))
      }

      # Determine the method to use
      current_method <- ifelse(col %in% names(special_methods), special_methods[[col]], method)

      # Pairwise complete observations
      pair_complete <- na.omit(df[, c(base_col, col)])

      # Calculate correlation
      correlation <- if (nrow(pair_complete) > 0) {
        cor(pair_complete[[base_col]], pair_complete[[col]], method = current_method)
      } else {
        NA
      }

      # Count missing values in each column
      missing_count_col <- sum(is.na(df[[col]]))

      # Return a list with correlation and missing counts
      list(correlation = correlation,
           missing_count_col = missing_count_col)
    })

    # Format and transpose the results for the current base column
    formatted_results <- sapply(base_results, function(x) {
      c(x$correlation, x$missing_count_col)
    })
    rownames(formatted_results) <- c("Correlation", "Missing_Value_Count")
    colnames(formatted_results) <- other_cols
    transposed_results <- t(formatted_results)

    # Add to the overall results
    results[[base_col]] <- data.frame(transposed_results)
  }
  return(results)
}
