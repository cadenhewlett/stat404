
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


