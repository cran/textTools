#' Vector of lowercase English stop words.
#'
#' @description Unique lowercase English stop words from 3 lexicons combined into one vector.
#' Combines snowball, onix, and SMART lists of stopwords.
#' @format A vector of 728 unique English stop words in lowercase
#' @source \url{http://snowball.tartarus.org/algorithms/english/stop.txt}
#' @source \url{http://www.lextek.com/manuals/onix/stopwords1.html}
#' @source \url{http://www.lextek.com/manuals/onix/stopwords2.html}
"stopwords"

#' Parts of speech for English words from the Moby Project.
#'
#' @description Parts of speech for English words/phrases from the Moby Project by Grady Ward.
#' Words with non-ASCII characters have been removed. One row per word + part of speech
#' @format Data.table with 246690 rows and 3 variables
#' #' \describe{
#'   \item{word}{Lowercase English word or phrase}
#'   \item{pos}{Lowercase English part of speech, one per row}
#'   \item{multi_word}{TRUE if the word record has a space (contains multiple words), else FALSE.}
#' }
#' @source \url{https://archive.org/details/mobypartofspeech03203gut}
"pos"

#' Parts of speech for English words from the Moby Project.
#'
#' @description Parts of speech for English words/phrases from the Moby Project by Grady Ward.
#' Words with non-ASCII characters have been removed. One row per word.
#' @format Data.table with 227519 rows and 3 variables
#' #' \describe{
#'   \item{word}{Lowercase English word or phrase}
#'   \item{pos}{Lowercase English part of speech, grouped by word into a vector if a word has multiple parts of speech.}
#'   \item{multi_word}{TRUE if the word record has a space (contains multiple words), else FALSE.}
#' }
#' @source \url{https://archive.org/details/mobypartofspeech03203gut}
"l_pos"

#' Regular expression that might be used to split strings of text into component words.
#'
#' @description " ", A regular expression to split strings when encountering a space.
#' @format A string
"regex_word"

#' Regular expression that might be used to split strings of text into component sentences.
#'
#' @description "[.?!]\\s", A regular expression to split strings when encountering a common end of sentence punctuation pattern.
#' @format A string
"regex_sentence"

#' Regular expression that might be used to split strings of text into component paragraphs.
#'
#' @description "\\n", A regular expression to split strings when encountering a new line.
#' @format A string
"regex_paragraph"

#' Generates (pseudo)random strings of the specified char length
#'
#' @param char A integer, the number of chars to include in the output string.
#' @return A string.
#' @examples
#' sampleStr(10)
sampleStr <- function(char){

  x <- c()

  for(i in 1:char){
    x <- c(x, sample(c(letters, LETTERS, 0:9), 1))
  }

  return(
    paste0(x, collapse = "")
  )
}

#' Create a vector of English words associated with particular parts of speech.
#'
#' @param parts A vector, at least one of the following:
#' 'adjective',
#' 'adverb',
#' 'conjunction',
#' 'definite article',
#' 'interjection',
#' 'noun',
#' 'noun phrase',
#' 'plural',
#' 'preposition',
#' 'pronoun',
#' 'verb (intransitive)',
#' 'verb (transitive)',
#' 'verb (usu participle)'
#' @param include_multi_word TRUE/FALSE, if TRUE, includes records from the pos data.table where multi_word == TRUE, otherwise excludes these records.
#' @return A vector of words matching the part of speech shown in the data.table pos.
#' @examples
#' str_stopwords_by_part_of_speech('adjective')
str_stopwords_by_part_of_speech <- function(
  parts = c(
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
    'verb (usu participle)'
  ),
  include_multi_word = FALSE
  ){

  word <- "word"
  multi_word <- "multi_word"

  if(include_multi_word == TRUE){
    return(unique(pos[pos %chin% parts, word]))
  }else{
    return(unique(pos[pos %chin% parts & multi_word == FALSE, word]))
  }

}

#' Calls base::tolower(), which converts letters to lowercase. Only included to point
#' out that base::tolower exists and should be used directly.
#'
#' @param x A vector or string.
#' @return x, converted to lowercase.
#' @examples
#' str_tolower(c("ALLCAPS", "Some capS"))
str_tolower <- function(x){
  return(tolower(x))
}

#' Remove and replace excess white space from strings.
#'
#' @param x A vector or string.
#' @param replacement, A string to replace the blank space with, defaults to " ", which replaces excess space with a single space.
#' @return x, with extra white space removed/replaced.
#' @examples
#' str_rm_blank_space(c("this     is   a test.  ", "    will it    work? "))
str_rm_blank_space <- function(x, replacement = " "){
  return(trimws(gsub(pattern ="\\s+", replacement = replacement, x = x)))
}

#' Remove and replace punctuation from strings.
#'
#' @param x A vector or string.
#' @param replacement, A string to replace the punctuation with, defaults to "".
#' @return x, with punctuation removed/replaced.
#' @examples
#' str_rm_punctuation(c("wait, is this is a test?", "Tests: . ! ?"))
str_rm_punctuation <- function(x, replacement = ""){
  return(gsub(pattern ="[[:punct:]]", replacement = replacement, x = x))
}

#' Remove and replace numbers from strings.
#'
#' @param x A vector or string.
#' @param replacement, A string to replace the numbers with, defaults to "".
#' @return x, with numbers 0-9 removed/replaced.
#' @examples
#' str_rm_numbers(c("test 1a234b5", "test 67890"))
str_rm_numbers <- function(x, replacement = ""){
  return(gsub(pattern ="[[:digit:]]", replacement = replacement, x = x))
}

#' Remove and replace non-alphanumeric characters from strings.
#'
#' @param x A vector or string.
#' @param replacement, A string to replace the numbers with, defaults to " ".
#' @return x, with non-alphanumeric (A-z, 0-9) characters removed/replaced.
#' @examples
#' str_rm_non_alphanumeric(c("test 67890 * % $ "))
str_rm_non_alphanumeric <- function(x, replacement = " "){
  return(gsub(pattern ="[^[:alnum:]]", replacement = replacement, x = x))
}

#' Remove and replace non-printable characters from strings.
#'
#' @param x A vector or string.
#' @param replacement, A string to replace the numbers with, defaults to " ".
#' @return x, with non-printable characters removed/replaced.
#' @examples
#' str_rm_non_printable(c("test \n\n\n67890 * % $ "))
str_rm_non_printable <- function(x, replacement = " "){
  return(gsub(pattern ="[^[:print:]]", replacement = replacement, x = x))
}

#' Remove words from a vector of words found in another vector of words.
#'
#' @param x A vector of words.
#' @param y, A vector of words to delete from x, defaults to English stop words.
#' @return x, with the words found in y removed.
#' @examples
#' str_rm_words(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = stopwords
#' )
#'
#' str_rm_words(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "store")
#' )
str_rm_words <- function(x, y = stopwords){

  x <- list(x, data.table::chmatch(x, y))

  return(x[[1]][is.na(x[[2]]) == TRUE])

}

#' Remove words from a vector based on the number of characters in each word.
#'
#' @param x A vector of words.
#' @param min_char_length, An integer, the minimum number of characters a word can have to not be removed.
#' @param max_char_length, An integer, the maximum number of characters a word can have to not be removed.
#' @return x, with the words not having a character count of at least the min_char_length and at most the max_char_length removed.
#' @examples
#' str_rm_words_by_length(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' min_char_length = 3
#' )
str_rm_words_by_length <- function(x, min_char_length = 0, max_char_length = Inf){

  return(x[data.table::between(nchar(x), min_char_length, max_char_length)])

}

#' Remove words from a vector that don't have a minimum number of characters.
#'
#' @param x A vector of words.
#' @param min_char_length, An integer, the minimum number of characters a word can have to not be removed.
#' @return x, with the words not having a character count greater than or equal to the min_char_length removed.
#' @examples
#' str_rm_short_words(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' min_char_length = 3
#' )
str_rm_short_words <- function(x, min_char_length){

  return(x[nchar(x) >= min_char_length])

}

#' Remove words from a vector that have more than a maximum number of characters.
#'
#' @param x A vector of words.
#' @param max_char_length, An integer, the maximum number of characters a word can have to not be removed.
#' @return x, with the words not having a character count less than or equal to the max_char_length removed.
#' @examples
#' str_rm_long_words(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' max_char_length = 2
#' )
str_rm_long_words <- function(x, max_char_length){

  return(x[nchar(x) <= max_char_length])

}

#' Remove words from a vector that match a regular expression.
#'
#' @param x A vector of words.
#' @param pattern, A regular expression.
#' @return x, with the words matching the regular expression removed.
#' @examples
#' str_rm_regexp_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' pattern = "to"
#' )
str_rm_regexp_match <- function(x, pattern){

  return(x[grepl(pattern, x) == FALSE])

}

#' Extract words from a vector that are found in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return x, with the words not found in y removed.
#' @examples
#' str_extract_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
str_extract_match <- function(x, y){

  x <- list(x, data.table::chmatch(x, y))

  return(x[[1]][is.na(x[[2]]) == FALSE])

}

#' Extract words from a vector that are not found in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return x, with the words found in y removed.
#' @examples
#' str_extract_nomatch(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
str_extract_nomatch <- function(x, y){

  x <- list(x, data.table::chmatch(x, y))

  return(x[[1]][is.na(x[[2]]) == TRUE])

}

#' Extract words from a vector that are found in the same position in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return x, with the words without matches in the same position in y removed.
#' @examples
#' str_extract_positional_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("this", "dog", "ran", "from", "the", "store")
#' )
str_extract_positional_match <- function(x, y){

  x <- list(x, data.table::chmatch(x, y), 1:length(x))

  return(x[[1]][x[[2]] == x[[3]] & is.na(x[[2]]) == FALSE])

}

#' Extract words from a vector that are not found in the same position in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return x, with the words with matches found in the same position in y removed.
#' @examples
#' str_extract_positional_nomatch(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("a", "crazy", "dog", "ran", "from", "the", "store")
#' )
str_extract_positional_nomatch <- function(x, y){

  x <- list(x, data.table::chmatch(x, y), 1:length(x))

  return(x[[1]][x[[2]] != x[[3]] | is.na(x[[2]]) == TRUE])

}

#' Detect if there are any words in a vector also found in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return TRUE/FALSE, TRUE if any words in x are also in y
#' @examples
#' str_any_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("the")
#' )
#' str_any_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("apple")
#' )
str_any_match <- function(x, y){

  x <- data.table::chmatch(x, y)

  if(length(x[is.na(x) == FALSE]) > 0){
    return(TRUE)
  }else{
    return(FALSE)
  }

}

#' Count the words in a vector that are found in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @param ratio TRUE/FALSE, if TRUE, returns the number of words in x with a match in y divided by the number of words in x.
#' @return A number, the count of words in x also in y
#' @examples
#' str_count_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
#' str_count_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store"),
#' ratio = TRUE
#' )
str_count_match <- function(x, y, ratio = FALSE){

  x <- list(x, data.table::chmatch(x, y))

  return(
    if(ratio==TRUE){
      length(x[[1]][is.na(x[[2]]) == FALSE])/length(x[[1]])
    }else{
      length(x[[1]][is.na(x[[2]]) == FALSE])
    }
  )
}

#' Count the words in a vector that are not found in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @param ratio TRUE/FALSE, if TRUE, returns the number of words in x without a match in y divided by the number of words in x.
#' @return A number, the count of words in x and not in y
#' @examples
#' str_count_nomatch(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
#' str_count_nomatch(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "store"),
#' ratio = TRUE
#' )
str_count_nomatch <- function(x, y, ratio = FALSE){

  x <- list(x, data.table::chmatch(x, y))

  return(
    if(ratio==TRUE){
      length(x[[1]][is.na(x[[2]]) == TRUE])/length(x[[1]])
    }else{
      length(x[[1]][is.na(x[[2]]) == TRUE])
    }
  )

}

#' Create a list of a vector of unique words found in x and a vector of the counts of each word in x.
#'
#' @param x A vector of words.
#' @return A list, position one is a vector of unique and sorted words from x, position two is a vector of the counts for each word.
#' @examples
#' str_counts(
#' x = c("a", "dog", "went", "to", "the", "store", "and", "a", "dog", "went", "to", "another", "store")
#' )
str_counts <- function(x){

  x <- base::table(x)

  return(list(names(x), unname(x)))

}

#' Weighted count of the words in a vector that are found in another vector.
#'
#' @param x A list of words and counts created by str_counts(x).
#' @param y A list of words and counts created by str_counts(y).
#' @return A number, the count of words in x also in y scaled by the number of times each word appears in x and y.
#' If a word appears 3 times in x and 2 times in y, the result is 6, assuming no other words match.
#' @examples
#' str_weighted_count_match(
#' x = str_counts(c("a", "dog", "dog", "went", "to", "the", "store")),
#' y = str_counts(c("dog", "dog", "dog"))
#' )
str_weighted_count_match <- function(x, y){

  z <- list(x[[1]], data.table::chmatch(x[[1]], y[[1]]), x[[2]])

  return(
    sum(z[[3]][is.na(z[[2]]) == FALSE]*y[[2]][is.na(data.table::chmatch(y[[1]], x[[1]])) == FALSE])
  )

}

#' Count words from a vector that are found in the same position in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @param ratio TRUE/FALSE, if TRUE, returns the number of words in x with a positional match in y divided by the number of words in x.
#' @return A count of the words in x with matches in the same position in y.
#' @examples
#' str_count_positional_match(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("this", "dog", "ran", "from", "the", "store")
#' )
str_count_positional_match <- function(x, y, ratio = FALSE){

  x <- list(x, data.table::chmatch(x, y), 1:length(x))

  return(
    if(ratio==TRUE){
      length(x[[1]][x[[2]] == x[[3]] & is.na(x[[2]]) == FALSE])/length(x[[1]])
    }else{
      length(x[[1]][x[[2]] == x[[3]] & is.na(x[[2]]) == FALSE])
    }
  )

}

#' Count words from a vector that are not found in the same position in another vector.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @param ratio TRUE/FALSE, if TRUE, returns the number of words in x without a positional match in y divided by the number of words in x.
#' @return A count of the words in x without matches in the same position in y.
#' @examples
#' str_count_positional_nomatch(
#' x = c("a", "cool", "dog", "went", "to", "the", "store"),
#' y = c("a", "dog", "ran", "from", "the", "store")
#' )
str_count_positional_nomatch <- function(x, y, ratio = FALSE){

  x <- list(x, data.table::chmatch(x, y), 1:length(x))

  return(
    if(ratio==TRUE){
      length(x[[1]][x[[2]] != x[[3]] | is.na(x[[2]]) == TRUE])/length(x[[1]])
    }else{
      length(x[[1]][x[[2]] != x[[3]] | is.na(x[[2]]) == TRUE])
    }
  )

}

#' Count the intersecting words in a vector that are found in another vector (only counts unique words).
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return A number, the count of unique words in x also in y
#' @examples
#' str_count_intersect(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
str_count_intersect <- function(x, y){

  return(length(intersect(x, y)))

}

#' Count the words in a vector that don't intersect with another vector (only counts unique words).
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return A number, the count of unique words in x not also in y
#' @examples
#' str_count_setdiff(
#' x = c("a", "dog", "dog", "went", "to", "the", "store"),
#' y = c("dog", "to", "store")
#' )
str_count_setdiff <- function(x, y){

  return(length(setdiff(x, y)))

}

#' Calculates the intersect divided by union of two vectors of words.
#'
#' @param x A vector of words.
#' @param y A vector of words to test against.
#' @return A number, the intersect divided by union of two vectors of words.
#' @examples
#'
#' str_count_jaccard_similarity(
#' x = c("a", "dog", "went", "to", "the", "store"),
#' y = c("this", "dog", "went", "to", "the", "store")
#' )
str_count_jaccard_similarity <- function(x, y){

  return(
    length(intersect(x, y))/length(union(x, y))
  )

}

#' Convert a data.table column of character vectors into a column with one row per word grouped by a grouping column.
#' Optionally will split a column of strings into vectors of constituents.
#'
#' @param x A data.table.
#' @param text A string, the name of the column in x containing text to un-nest.
#' @param split A string with a pattern to split the text in text column into constituent parts.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @return A data.table, text column un-nested to one row per word.
#' @examples
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' )
as.text.table <- function(x, text, split = NULL, group_by = NULL){

  x <- copy(x)

  if(is.null(group_by) == TRUE){
    group_by <- setdiff(colnames(x), text)
  }

  if(is.null(split) == TRUE){
    x <- x[, as.character(unlist(base::get(text), use.names = FALSE)), by = group_by]
  }else{
    x <- x[, as.character(unlist(strsplit(base::get(text), split = split, fixed = TRUE), use.names = FALSE)), by = group_by]
  }

  colnames(x) <- c(group_by, text)

  return(x)

}

#' Remove rows from a text.table with specific words
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to check for words to delete.
#' @param words A vector of words to delete from x.
#' @return A text.table, with rows deleted if the words in those rows are in the vector of words to delete.
#' @examples
#' rm_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2"
#' )
rm_words <- function(x, text, words = stopwords){

  return(x[base::get(text) %chin% words == FALSE, ])

}

#' Flag rows in a text.table with specific words
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to check for words to flag.
#' @param flag A string, the name of the column created with the flag indicator.
#' @param words A vector of words to flag x.
#' @return A text.table, with rows marked with a 1 if the words in those rows are in the vector of words to delete, otherwise 0.
#' @examples
#' flag_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' flag = "is_stopword",
#' words = stopwords
#' )
flag_words <- function(x, text, flag = "flag", words){

  x[, paste0(flag) := 0]
  x[base::get(text) %chin% words == TRUE, paste0(flag) := 1]

  return(x)

}

#' Add a column with the parts of speech for each word in a text.table
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to label the parts of speech.
#' @return A text.table, with columns added for the matching part of speech and  for flagging if the part of speech is for a multi-word phrase.
#' @examples
#' label_parts_of_speech(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2"
#' )
label_parts_of_speech <- function(x, text){

  colorder <- colnames(x)

  x <- merge(
    x,
    l_pos,
    by.x = paste0(text),
    by.y = "word",
    all.x = TRUE
  )

  setcolorder(x, c(colorder, setdiff(colnames(l_pos), "word")))

  return(x)

}

#' Delete rows in a text.table where the word has a certain part of speech
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the part of speech.
#' @param pos_delete A vector of parts of speech to delete. At least one of the following:
#' 'adjective',
#' 'adverb',
#' 'conjunction',
#' 'definite article',
#' 'interjection',
#' 'noun',
#' 'noun phrase',
#' 'plural',
#' 'preposition',
#' 'pronoun',
#' 'verb (intransitive)',
#' 'verb (transitive)',
#' 'verb (usu participle)'
#' @return A text.table, with rows matching a part of speech deleted.
#' @examples
#' rm_parts_of_speech(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' pos_delete = "conjunction"
#' )
rm_parts_of_speech <- function(
  x,
  text,
  pos_delete = c(
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
    'verb (usu participle)'
  )
){

  word <- "word"

  words <- unique(pos[pos %chin% pos_delete, word])

  return(x[base::get(text) %chin% words == FALSE, ])

}

#' Delete rows in a text.table where the word has less than a minimum number of characters
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the number of characters.
#' @param min_char_length A number, the minimum number of characters required to not delete a row.
#' @return A text.table, with rows having less than a certain number of characters deleted.
#' @examples
#' rm_short_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' min_char_length = 4
#' )
rm_short_words <- function(x, text, min_char_length){

  return(x[nchar(base::get(text)) >= min_char_length, ])

}

#' Delete rows in a text.table where the word has more than a minimum number of characters
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the number of characters.
#' @param max_char_length A number, the maximum number of characters allowed to not delete a row.
#' @return A text.table, with rows having more than a certain number of characters deleted.
#' @examples
#' rm_long_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' max_char_length = 4
#' )
rm_long_words <- function(x, text, max_char_length){

  return(x[nchar(base::get(text)) <= max_char_length, ])

}

#' Delete rows in a text.table where the record has a certain pattern indicated by a regular expression
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the regular expression.
#' @param pattern A regular expression, gets passed to grepl().
#' @return A text.table, with rows having a certain pattern indicated by a regular expression deleted.
#' @examples
#' rm_regexp_match(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' pattern = "do"
#' )
rm_regexp_match <- function(x, text, pattern){

  return(x[grepl(pattern, base::get(text)) == FALSE, ])

}

#' Delete rows in a text.table where the number of identical records within a group is less than a certain threshold
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the term frequency.
#' @param count_col_name A string, the name to assign to the new column containing the count of each word. If NULL, does not return the counts.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @param min_count A number, the minimum number of times a word must occur to keep.
#' @param min_count_is_ratio TRUE/FALSE, if TRUE, implies the value passed to min_count should be considered a ratio.
#' @param total_count_col Name of the column containing the denominator (likely total count of records within a group) to use to calculate the ratio of a word count vs total if min_count_is_ratio is TRUE.
#' @return A text.table, with rows having a duplicate count of less than a certain threshold deleted.
#' @examples
#' rm_infrequent_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' count_col_name = "count",
#' min_count = 4
#' )
#'
#' rm_infrequent_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the
#'         newspaper and it is the nice kind of dog."),
#'         tolower("The dog is extremely nice because it does the dishes
#'         and it is cool.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' count_col_name = "count",
#' group_by = "col1",
#' min_count = 2
#' )
rm_infrequent_words <- function(x, text, count_col_name = NULL, group_by = c(), min_count, min_count_is_ratio = FALSE, total_count_col = NULL){

  group_by <- unique(c(group_by, text))

  return_count <- TRUE

  if(is.null(count_col_name) == TRUE){

    cols <- colnames(x)

    count_col_name <- sampleStr(10)

    while(count_col_name %in% cols){
      count_col_name <- sampleStr(10)
    }

    return_count <- FALSE

  }

  x[, paste0(count_col_name) := .N, by = group_by]

  return(
    if(return_count == TRUE){
      if(min_count_is_ratio == FALSE){
        x[base::get(count_col_name) >= min_count, ]
      }else{
        x[, paste0(count_col_name) := base::get(count_col_name)/base::get(total_count_col)]
        x[base::get(count_col_name) >= min_count, ]
      }
    }else{
      if(min_count_is_ratio == FALSE){
        x[base::get(count_col_name) >= min_count, ][, paste0(count_col_name) := NULL]
      }else{
        x[, paste0(count_col_name) := base::get(count_col_name)/base::get(total_count_col)]
        x[base::get(count_col_name) >= min_count, ][, paste0(count_col_name) := NULL]
      }
    }
  )

}

#' Delete rows in a text.table where the number of identical records within a group is more than a certain threshold
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x used to determine deletion of rows based on the term frequency.
#' @param count_col_name A string, the name to assign to the new column containing the count of each word. If NULL, does not return the counts.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @param max_count A number, the maximum number of times a word can occur to keep.
#' @param max_count_is_ratio TRUE/FALSE, if TRUE, implies the value passed to max_count should be considered a ratio.
#' @param total_count_col Name of the column containing the denominator (likely total count of records within a group) to use to calculate the ratio of a word count vs total if max_count_is_ratio is TRUE.
#' @return A text.table, with rows having a duplicate count over a certain threshold deleted.
#' @examples
#' rm_frequent_words(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' count_col_name = "count",
#' max_count = 1
#' )
rm_frequent_words <- function(x, text, count_col_name = NULL, group_by = c(), max_count, max_count_is_ratio = FALSE, total_count_col = NULL){

  group_by <- unique(c(group_by, text))

  return_count <- TRUE

  if(is.null(count_col_name) == TRUE){

    cols <- colnames(x)

    count_col_name <- sampleStr(10)

    while(count_col_name %in% cols){
      count_col_name <- sampleStr(10)
    }

    return_count <- FALSE

  }

  x[, paste0(count_col_name) := .N, by = group_by]

  return(
    if(return_count == TRUE){
      if(max_count_is_ratio == FALSE){
        x[base::get(count_col_name) <= max_count, ]
      }else{
        x[, paste0(count_col_name) := base::get(count_col_name)/base::get(total_count_col)]
        x[base::get(count_col_name) <= max_count, ]
      }
    }else{
      if(max_count_is_ratio == FALSE){
        x[base::get(count_col_name) <= max_count, ][, paste0(count_col_name) := NULL]
      }else{
        x[, paste0(count_col_name) := base::get(count_col_name)/base::get(total_count_col)]
        x[base::get(count_col_name) <= max_count, ][, paste0(count_col_name) := NULL]
      }
    }
  )

}

#' Delete rows in a text.table where the records within a group are also found in other groups (overlapping records)
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to determine deletion of rows based on the presence of overlapping records.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @return A text.table, with rows having records found in multiple groups (overlapping records) deleted.
#' @examples
#' rm_overlap(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' group_by = "col1"
#' )
rm_overlap <- function(x, text, group_by = c()){

  z <- c(group_by, text)

  y <- x[, z, with = FALSE]

  y <- unique(y)

  cols <- colnames(x)

  count_col_name <- sampleStr(10)

  while(count_col_name %chin% cols){
    count_col_name <- sampleStr(10)
  }

  y[, paste0(count_col_name) := .N, c(text)]

  y <- y[base::get(count_col_name) == 1, ]

  x <- merge(
    x,
    y,
    by.x = z,
    by.y = z
  )

  return(x[, paste0(count_col_name) := NULL])

}

#' Delete rows in a text.table where the records within a group are not also found in other groups (overlapping records)
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to determine deletion of rows based on the lack of presence of overlapping records.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @return A text.table, with rows not having records found in multiple groups (overlapping records) deleted.
#' @examples
#' rm_no_overlap(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' group_by = "col1"
#' )
rm_no_overlap <- function(x, text, group_by = c()){

  z <- c(group_by, text)

  y <- x[, z, with = FALSE]

  y <- unique(y)

  cols <- colnames(x)

  count_col_name <- sampleStr(10)

  while(count_col_name %in% cols){
    count_col_name <- sampleStr(10)
  }

  y[, paste0(count_col_name) := .N, c(text)]

  y <- y[base::get(count_col_name) > 1, ]

  x <- merge(
    x,
    y,
    by.x = z,
    by.y = z
  )

  return(x[, paste0(count_col_name) := NULL])

}

#' Create n-grams
#'
#' @param x A text.table created by as.text.table().
#' @param text A string, the name of the column in x to build n-grams with.
#' @param group_by A vector of column names to group by. Doesn't work if the group by column is a list column.
#' @param count_col_name A string, the name of the output column containing the number of times each base record appears in the group.
#' @param n A integer, the number of grams to make.
#' @param ngram_prefix A string, a prefix to add to the output n-gram columns.
#' @return A text.table, with columns added for n-grams (the word, the count, and percent of the time the gram follows the word).
#' @examples
#' ngrams(
#' as.text.table(
#'   x = as.data.table(
#'     list(
#'       col1 = c(
#'         "a",
#'         "b"
#'       ),
#'       col2 = c(
#'         tolower("The dog is nice because it picked up the newspaper."),
#'         tolower("The dog is extremely nice because it does the dishes.")
#'       )
#'     )
#'   ),
#'   text = "col2",
#'   split = " "
#' ),
#' text = "col2",
#' group_by = "col1",
#' n = 2
#' )
ngrams <- function(x, text, group_by = c(), count_col_name = "count", n, ngram_prefix = NULL){

  y <- copy(x)

  group_by_text <- c(group_by, text)

  y[, paste0(count_col_name) := .N, group_by_text]

  for(i in 1:n){

    y[, paste(paste0(ngram_prefix, "gram", collapse = "_"), i, sep = "_") := shift(base::get(text), n = i, type = "lead"), group_by]
    y[, paste(paste0(ngram_prefix, "gram", collapse = "_"), i, "count", sep = "_") := .N, c(group_by_text, paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:i, sep = "_"))]
    y[, paste(paste0(ngram_prefix, "gram", collapse = "_"), i, "ratio", sep = "_") := base::get(paste(paste0(ngram_prefix, "gram", collapse = "_"), i, "count", sep = "_"))/base::get(count_col_name)]

  }

  base_cols <- setdiff(colnames(x), c(text, paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, sep = "_"), paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, "count", sep = "_"), paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, "ratio", sep = "_"), paste0(count_col_name)))

  y <- subset(y, TRUE, select = c(base_cols, text, paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, sep = "_"), paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, "count", sep = "_"), paste(paste0(ngram_prefix, "gram", collapse = "_"), 1:n, "ratio", sep = "_"), paste0(count_col_name)))

  setorderv(y, c(paste0(count_col_name), paste(paste0(ngram_prefix, "gram", collapse = "_"), min(1:n), "ratio", sep = "_")), order = c(1, -1))

  return(unique(y))

}

#' Combine columns of a data.table into a list in a new column, wraps list(unlist(c(...)))
#'
#' @param ... Unquoted column names of a data.table.
#' @return A list, with columns combined into a vector if grouped properly
#' @examples
#' as.data.table(
#' list(
#'   col1 = c(
#'     "a",
#'     "b"
#'   ),
#'   col2 = c(
#'     tolower("The dog is nice because it picked up the newspaper."),
#'     tolower("The dog is extremely nice because it does the dishes.")
#'   ),
#'   col3 = c(
#'     "test 1",
#'     "test 2"
#'   )
#' )
#' )[, col4 := .(str_dt_col_combine(col2, col3)), col1]
str_dt_col_combine <- function(...){
  return(list(unlist(c(...))))
}
