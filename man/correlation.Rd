% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlation.r
\name{correlation}
\alias{correlation}
\title{Correlation Calculation for different data type}
\usage{
correlation(
  df,
  base_cols,
  other_cols,
  method = "pearson",
  special_methods = list()
)
}
\arguments{
\item{df}{Dataframe}

\item{base_cols}{a list of column name as the baseline covariates}

\item{other_cols}{a list of column name as the other covariates}

\item{method}{the default method for calculate the correlation, default value is "pearson"}

\item{special_methods}{a list with name of different correlation method, and value of multiple column names}
}
\value{
a list of dataframe for every column in the based_cols, contains hte correaltion index.
}
\description{
a function that perform multiple correlation calculation, accept "pearson", "kendall", "spearman"
}
\examples{
df <- data.frame(col1 = rnorm(100), col2 = rnorm(100), col3 = rnorm(100))
result <- correlation(df, c("col1", "col2"), c("col2", "col3"))
}
\author{
Yushu Zou
}
