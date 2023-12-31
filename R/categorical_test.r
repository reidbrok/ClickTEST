#' @title Categorical Hypothesis Testing
#' @description a function that perform hypothesis test result for all categorical columns in a dataframe
#' @param df Dataframe
#' @param group_var a column name indicate the group variable
#' @param cat_var a list of column names that contains categorical data
#' @param alternative a variable indicate the hypothesis testing, default is "two.sided"
#' @param threshold the threshold for Test, default value is 0.05
#' @param correct a Boolean variable to indicate whether the chi-square test need correction
#' @return a list where the first element contains the column names of normal data, the second element contains the column name for those data are not normal
#' @author Yushu Zou
#' @import stats
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'  group = factor(rep(c("Group1", "Group2"), each = 50)), # Two groups
#'  category1 = factor(sample(c("Cat1", "Cat2", "Cat3"), 100, replace = TRUE)),
#'  category2 = factor(sample(c("TypeA", "TypeB", "TypeC"), 100, replace = TRUE))
#')
#' categorical_test(df, group_var = "group")
#' @export
#'
categorical_test <- function(df, group_var = NA, cat_var = NA, alternative = "two.sided", threshold = .05, correct = T){
  if (is.na(cat_var)){
    cat_var <- names(df)[sapply(df, function(col) (is.factor(col) || is.character(col)))]
    cat_var = setdiff(cat_var,group_var)
  }
  if (is.null(cat_var)){
    stop("there is no categorical data in your dataframe")
  }
  if (min(cat_var %in% names(df)) == 0){
    stop("At least one of the cat_var you entered is not in the dataframe, please double check")
  }
  if (!is.na(group_var) & group_var %in% names(df)){
    df[[group_var]] <- as.factor(df[[group_var]])
  }
  if(is.na(group_var)){
    stop("we only support compared it with the group")
  }

  if(!group_var %in% names(df)){
    stop("there is no such group column")
  }
  if (group_var %in% names(df) & length(levels(as.factor(df[[group_var]]))) == 1){
    stop("the group_var you entered here only have one category, please double check")
  }
  if(length(levels(df[[group_var]])) == 2){
    print("two sample categorical test")
    result <- fisher(df,cat_var, group_var, threshold = threshold, correct = correct,alternative = alternative)

  }
  else{
    print("more than two sample categorical test")
    result <- chisquare(df,cat_var,group_var, threshold = threshold, correct = correct)
  }
  return(result)
}
