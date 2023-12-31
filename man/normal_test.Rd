% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal_test.r
\name{normal_test}
\alias{normal_test}
\title{Parametric Hypothesis Testing}
\usage{
normal_test(
  df,
  test = "mean",
  group_var = NA,
  num_var = NA,
  paired = F,
  exact = F,
  mu_values = rep(NA, 1),
  alternative = "two.sided",
  threshold = 0.05,
  method = "none",
  equal_variances = T
)
}
\arguments{
\item{df}{Dataframe}

\item{test}{string to indicate if user want to mean test or variance test. Value could be either "mean" or "variance", the default value is mean.}

\item{group_var}{a column name indicate the group variable}

\item{num_var}{a list of column names that contains normal data}

\item{paired}{a Boolean value indicate if its a paired test, default value is False}

\item{exact}{a Boolean value indicate if its a exact test, default value is False}

\item{mu_values}{a list of variable that indicate the null hypothesis assumption value, default is a list of 0}

\item{alternative}{a variable indicate the hypothesis testing, default is "two.sided"}

\item{threshold}{the threshold for Test, default value is 0.05}

\item{method}{correction method for Dunn's test, default value is "none"}

\item{equal_variances}{a Boolean value for equal variance in more than 3 groups test. Default value is T, indicate that one-way anova test will be run.}
}
\value{
a list where the first element contains the column names of normal data, the second element contains the column name for those data are not normal
}
\description{
a function that perform parametric hypothesis test result for all normal numeric columns in a dataframe
}
\examples{
normal_test(example_data)
normal_test(example_data, group_var = "group_two")
normal_test(example_data, group_var = "group_three")
normal_test(example_data, group_var = "group_two", test = "variance")
normal_test(example_data, group_var = "group_three", test = "variance")
}
\author{
Yushu Zou
}
