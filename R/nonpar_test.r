#' @title Nonparametric Hypothesis Testing
#' @description a function that perform non-parametric hypothesis test result for all non-normal numeric columns in a dataframe
#' @param df Dataframe
#' @param group_var a column name indicate the group variable
#' @param num_var a list of column names that contains non-normal data
#' @param test string to indicate if user want to mean test or variance test (not available for one sample test). Value could be either "sign" or "variance", the default value is sign.
#' @param paired a Boolean value indicate if its a paired test, default value is False
#' @param exact a Boolean value indicate if its a exact test, default value is False
#' @param mu_values a list of variable that indicate the null hypothesis assumption value, default is a list of 0
#' @param alternative a variable indicate the hypothesis testing, default is "two.sided"
#' @param threshold the threshold for Test, default value is 0.05
#' @param method correction method for Dunn's test, default value is "none"
#' @return a list where the first element contains the column names of normal data, the second element contains the column name for those data are not normal
#' @author Yushu Zou
#' @examples
#' nonpar_test(example_data)
#' nonpar_test(example_data, group_var = "group_two")
#' nonpar_test(example_data, group_var = "group_three")
#' nonpar_test(example_data, group_var = "group_two", test ="variance")
#' nonpar_test(example_data, group_var = "group_three", test ="variance")
#' @export
#'

nonpar_test <- function(df, test = "sign", group_var = NA, num_var = NA, paired = F, exact = F, mu_values = NA, alternative = "two.sided", threshold = .05, method = "none"){
  # if the numeric column name is not provide, that any column in the dataframe that does not follow normality will be used
  if (max(is.na(num_var)) == 1){
    normal_result <- test_normality(df,threshold)
    num_var <- normal_result$non_normal
  }
  if (is.null(num_var) | max(num_var %in% names(df)) == 0){
    stop("the variable you entered is not satisfy the condition for process")
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
    if(test == "variance"){
      stop("We don't support one sample variance test for non-parametric test.")
    }
      print("one sample Wilcox test")
      result <- wilcox_test_one_sample(df,num_var, group_var, paired = paired, exact = exact,alternative = alternative, mu_values = mu_values, threshold = threshold)
  }
  else{ # 2 or more sample test
    if(!group_var %in% names(df)){
      stop("there is no such group column")
    }
    if(length(levels(df[[group_var]])) == 2){
      if( test == "sign"){
      # two sample wilcox test

        print("two sample Wilcox test")
        result <- wilcox_test(df,num_var, group_var, paired = paired, exact = exact,alternative = alternative, mu_values = mu_values, threshold = threshold)
        }
      else{ # two sample variance test

        print("two sample variance leneve test")
        result <- leneve_test(df,num_var, group_var, threshold = threshold)
       }
      }
    else{ # more than 3 groups
      # krustal test for more than 3 groups
      if( test == "sign"){ # Median Test
        print("krustal test for more than 3 groups")
        result <- Kruskal_test(df,num_var, group_var, threshold = threshold)
        var <- rownames(result[result$significate == "***", ])
        if(length(var)>1){ # exists difference among three groups
        # Dunn's test for post-hoc
          print("Dunn's test for post-hoc")
          dunn_results <- Dunn_test(df,var,group_var,method = method, threshold = threshold)
          return(list(result,dunn_results))
        }
        return(result)
        }
      else{ # Variance Test
        print("variance leneve test")
        result <- leneve_test(df,num_var, group_var, threshold = threshold)
        return(result)
      }
    }
  return(result)
  }
  return(result)
}
