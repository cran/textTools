% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.table.R
\name{str_rm_punctuation}
\alias{str_rm_punctuation}
\title{Remove and replace punctuation from strings.}
\usage{
str_rm_punctuation(x, replacement = "")
}
\arguments{
\item{x}{A vector or string.}

\item{replacement, }{A string to replace the punctuation with, defaults to "".}
}
\value{
x, with punctuation removed/replaced.
}
\description{
Remove and replace punctuation from strings.
}
\examples{
str_rm_punctuation(c("wait, is this is a test?", "Tests: . ! ?"))
}
