% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.table.R
\name{rm_no_overlap}
\alias{rm_no_overlap}
\title{Delete rows in a text.table where the records within a group are not also found in other groups (overlapping records)}
\usage{
rm_no_overlap(x, text, group_by = c())
}
\arguments{
\item{x}{A text.table created by as.text.table().}

\item{text}{A string, the name of the column in x to determine deletion of rows based on the lack of presence of overlapping records.}

\item{group_by}{A vector of column names to group by. Doesn't work if the group by column is a list column.}
}
\value{
A text.table, with rows not having records found in multiple groups (overlapping records) deleted.
}
\description{
Delete rows in a text.table where the records within a group are not also found in other groups (overlapping records)
}
\examples{
rm_no_overlap(
as.text.table(
  x = as.data.table(
    list(
      col1 = c(
        "a",
        "b"
      ),
      col2 = c(
        tolower("The dog is nice because it picked up the newspaper."),
        tolower("The dog is extremely nice because it does the dishes.")
      )
    )
  ),
  text = "col2",
  split = " "
),
text = "col2",
group_by = "col1"
)
}
