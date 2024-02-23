
#' Explode Words into Characters
#'
#' A function that takes a vector of words and splits each word into its constituent characters. Used in the defining of contrast subgroups.
#'
#' @param words A character vector, where each element is a word that you want to split into characters.
#' @return A list where each element is a character vector corresponding to the characters of a word.
#' @examples
#' explode_word(c("tomato"))
#' explode_word(c("apple", "banana"))
#' @export
explode_word <- function(words) {
  lapply(words, function(x) strsplit(x, "")[[1]])
}

#' Multiply Two Words
#'
#' This function takes the "product" of two words, combining their letters and dropping duplicates.
#' It optionally uses the previously defined \code{\link{explode_word}} function to split the words into characters.
#'
#' @param word1 The first word (a character string or a vector of characters if `exploded` is TRUE).
#' @param word2 The second word (a character string or a vector of characters if `exploded` is TRUE).
#' @param exploded Logical, indicating whether the words are already split into characters.
#' @return A sorted character vector containing the unique letters from both words, minus duplicates.
#' @examples
#' multiply_words(c("A", "B"), c("A", "D"), TRUE) # returns c("B", "D")
#' multiply_words("Tuhmaytoe", "Tuhmaatoe") # returns c("y")
#' @seealso \code{\link{explode_word}} for the function used to split words into characters.
#' @export
multiply_words <- function(word1, word2, exploded = FALSE) {
  if (!is.logical(exploded) || length(exploded) != 1) {
    stop("'exploded' should be a single logical value.")
  }

  if (!exploded) {
    word1 = unlist(explode_word(word1))
    word2 = unlist(explode_word(word2))
  }

  word <- unique(c(word1, word2))
  for (factor in intersect(word1, word2)) {
    word <- setdiff(word, factor)
  }

  return(sort(word))
}


#' Generate All Contrast Subgroups
#'
#' This function generates all possible unique contrast subgroups (combinations) from
#' a given set of definitions or relationships. It's designed to explore all potential
#' arrangements and their unique compositions, particularly useful in statistical
#' analysis or fractional factorial designs.
#'
#' @param def_relations A character vector representing different definitions or
#' relationships from which the contrast subgroups are to be generated.
#' @return A list of character vectors, each representing a unique contrast subgroup,
#' ordered by the length of the subgroup. The list always starts with "I" to denote
#' the identity or baseline subgroup. Works well with \code{\link{explode_word}} to translate
#' a list of characters into a suitable list of lists.
#' @seealso \code{\link{explode_word}} for translating lists of words into letters.
#' @examples
#' IA = c("1235", "1246", "12347")
#' all_contrast_subgroups(explode_word(IA))
#' # will return inputs, I, and then ABCD x ABED = CE,
#' all_contrast_subgroups(list( c("A", "B", "C", "D"), c("A", "B", "E", "D")))
#'
#' @export
all_contrast_subgroups <- function(def_relations) {
  # make sure inputs are valid
  if (!is.list(def_relations)) {
    stop("'def_relations' must be a list.")
  }
  if (length(def_relations) < 1) {
    stop("'def_relations' must contain at least one element.")
  }
  # create an empty list for storing words
  all_words <- list()
  # for every word in the list of words we're given
  for (i in 1:length(def_relations)) {
    # find all possible combinations of word arrangements of size i
    # think of this as "all possible sentences of size i formed with words in the words list"
    possible_sentences <- combn(def_relations, i, simplify = FALSE)
    # then, for each of these sentences
    for (sentence in possible_sentences) {
      # make a new empty word for storing
      word <- c()
      # for each word in the sentence
      for (word_piece in sentence) {
        # use of our function from earlier to get the unique ones
        # comparing to our running tally of unique words
        word <- multiply_words(word, word_piece)
      }
      # append all the unique word combos we found to our running tally
      all_words <- c(all_words, list(word))
    }
  }
  # remove all duplicates
  unique_words <- unique(all_words)
  # order by word length
  unique_words <- unique_words[order(sapply(unique_words, length))]
  # return with the "I = " added
  return(c("I", unique_words))
}
