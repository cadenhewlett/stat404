
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
  if (!is.character(words)) {
    stop("The 'words' parameter should be a character vector.")
  }
  if (length(words) == 0) {
    stop("The 'words' parameter should not be empty.")
  }
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
  # Input validation
  if (!is.character(word1) || !is.character(word2)) {
    stop("Both 'word1' and 'word2' should be character strings or character vectors.")
  }
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
