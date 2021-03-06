% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text.table.R
\name{rm_parts_of_speech}
\alias{rm_parts_of_speech}
\title{Delete rows in a text.table where the word has a certain part of speech}
\usage{
rm_parts_of_speech(
  x,
  text,
  pos_delete = c("adjective", "adverb", "conjunction", "definite article",
    "interjection", "noun", "noun phrase", "plural", "preposition", "pronoun",
    "verb (intransitive)", "verb (transitive)", "verb (usu participle)")
)
}
\arguments{
\item{x}{A text.table created by as.text.table().}

\item{text}{A string, the name of the column in x used to determine deletion of rows based on the part of speech.}

\item{pos_delete}{A vector of parts of speech to delete. At least one of the following:
'adjective',
'adverb',
'conjunction',
'definite article',
'interjection',
'noun',
'noun phrase',
'plural',
'preposition',
'pronoun',
'verb (intransitive)',
'verb (transitive)',
'verb (usu participle)'}
}
\value{
A text.table, with rows matching a part of speech deleted.
}
\description{
Delete rows in a text.table where the word has a certain part of speech
}
\examples{
rm_parts_of_speech(
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
pos_delete = "conjunction"
)
}
