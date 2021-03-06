% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.table.R
\name{str_dt_col_combine}
\alias{str_dt_col_combine}
\title{Combine columns of a data.table into a list in a new column, wraps list(unlist(c(...)))}
\usage{
str_dt_col_combine(...)
}
\arguments{
\item{...}{Unquoted column names of a data.table.}
}
\value{
A list, with columns combined into a vector if grouped properly
}
\description{
Combine columns of a data.table into a list in a new column, wraps list(unlist(c(...)))
}
\examples{
as.data.table(
list(
  col1 = c(
    "a",
    "b"
  ),
  col2 = c(
    tolower("The dog is nice because it picked up the newspaper."),
    tolower("The dog is extremely nice because it does the dishes.")
  ),
  col3 = c(
    "test 1",
    "test 2"
  )
)
)[, col4 := .(str_dt_col_combine(col2, col3)), col1]
}
