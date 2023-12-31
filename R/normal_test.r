#' @title Parametric Hypothesis Testing
#' @description a function that perform parametric hypothesis test result for all normal numeric columns in a dataframe
#' @param df Dataframe
#' @param test string to indicate if user want to mean test or variance test. Value could be either "mean" or "variance", the default value is mean.
#' @param group_var a column name indicate the group variable
#' @param num_var a list of column names that contains normal data
#' @param paired a Boolean value indicate if its a paired test, default value is False
#' @param exact a Boolean value indicate if its a exact test, default value is False
#' @param mu_values a list of variable that indicate the null hypothesis assumption value, default is a list of 0
#' @param alternative a variable indicate the hypothesis testing, default is "two.sided"
#' @param threshold the threshold for Test, default value is 0.05
#' @param method correction method for Dunn's test, default value is "none"
#' @param equal_variances a Boolean value for equal variance in more than 3 groups test. Default value is T, indicate that one-way anova test will be run.
#' @return a list where the first element contains the column names of normal data, the second element contains the column name for those data are not normal
#' @author Yushu Zou
#' @examples
#' normal_test(example_data)
#' normal_test(example_data, group_var = "group_two")
#' normal_test(example_data, group_var = "group_three")
#' normal_test(example_data, group_var = "group_two", test = "variance")
#' normal_test(example_data, group_var = "group_three", test = "variance")
#' @export
#' @import stats
normal_test <-  function(df, test = "mean", group_var = NA, num_var = NA, paired = F, exact = F, mu_values = rep(NA,1), alternative = "two.sided", threshold = .05, method = "none", equal_variances = T){
    # if the numeric column name is not provide, that any column in the dataframe that does follow normality will be used
  if (max(is.na(num_var)) == 1){
      normal_result <- test_normality(df,threshold =0.05)
      num_var <- normal_result$normal
  }
  # Indicate if all numeric column are not normal distributed or there is at least one column in num_var is not in dataframe
    if (is.null(num_var)){
      stop("No variable in your dataframe are normal distributed under threshold; try using num_var to forcing process")
    }
    if((!is.null(num_var)) & (min(num_var %in% names(df)) == 0)){
      stop("At least one num_var you entered is not in the dataframe, please double check")
    }

    if (length(mu_values) == 1 & max(is.na(mu_values)) == 1){
      mu_values <- rep(1, length(num_var))
        }
    mu_values <- setNames(mu_values, num_var)
    if (!is.na(group_var) & group_var %in% names(df)){
      df[[group_var]] <- as.factor(df[[group_var]])
      if (group_var %in% num_var){
        num_var = setdiff(num_var,group_var)
      }
    }
    if ((is.na(group_var))|| (group_var %in% names(df) & length(levels(df[[group_var]])) == 1)){ ## one sample test
      if (test == "mean"){ ### one sample mean test
        result <- onesample_t(df,num_var, paired = paired, exact = exact,alternative = alternative, mu_values = mu_values, threshold = threshold)
      }
      else{ ### one sample variance test
        print("one sample Chi-square test")
        result <- chisquare_onesample(df,num_var,alternative = alternative, mu_values = mu_values, threshold = threshold)
      }
      return(result)
    }
  if (!group_var %in% names(df)){
    stop("there is no such group column")
  }
  if(length(levels(df[[group_var]])) > 1){## 2 or more sample test
      if(length(levels(df[[group_var]])) == 2){
        print("Two groups comparison mean test")
        # two sample t test
        result <- twosample_t(df,num_var, test = test, group_var, paired = paired, alternative = alternative, threshold = threshold)
      }
      else{ # more than 3 group test
        # one way anova test / welch test for more than 3 groups
        print("More than 3 groups")
        result <- group_norm(df,num_var, group_var, test = test, threshold = threshold, equal_variances = equal_variances, method = method, alternative= alternative)
        return(result)
      }
    }
    return(result)
  }

