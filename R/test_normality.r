#' @title Testing Normality
#' @description a function that test the normality of all numeric column in one dataframe
#' @param df Dataframe
#' @param threshold the threshold for Shapiro-Wilk test, default value is 0.05
#' @return a list where the first element contains the column names of normal data, the second element contains the column name for those data are not normal
#' @author Yushu Zou
#' @examples
#' set.seed(123)
#' n <- 1000
#' df <- data.frame(
#' normal1 = rnorm(n, mean = 50, sd = 10),    # Normally distributed
#' normal2 = rnorm(n, mean = 100, sd = 20),   # Normally distributed
#' uniform = runif(n, min = 0, max = 50),     # Uniform distribution (non-normal)
#' poisson = rpois(n, lambda = 20),           # Poisson distribution (non-normal)
#' exponential = rexp(n, rate = 0.1)          # Exponential distribution (non-normal)
#' )
#' test_normality(df)
#' @export
#' @import stats
test_normality <- function(df, threshold = 0.05) {
    # Lists to store column names
    normal_cols <- c()
    non_normal_cols <- c()

    # Loop through each column
    for (col_name in names(df)) {
      # Check if the column is numeric
      if (is.numeric(df[[col_name]])) {
        # Perform the Shapiro-Wilk test for normality
        test_result <- shapiro.test(df[[col_name]])

        # Categorize based on the p-value
        if (test_result$p.value > threshold) {
          normal_cols <- c(normal_cols, col_name)
        } else {
          non_normal_cols <- c(non_normal_cols, col_name)
        }
      }
    }

    # Return the lists
    return(list(normal = normal_cols, non_normal = non_normal_cols))
}
