#' Paste numbers from your clipboard
#'
#' Call this function as an addin to paste numbers from your clipboard upto 1024 characters
#' @importFrom magrittr %>%
#' @export
pasteNumbers <- function() {
    x <- clipr::read_clip() %>%
        splitIntoNumbers(end_pos=1024)
    rstudioapi::insertText(x)
    x
}

#' Extract number strings from a character vector excluding commas
#'
#' This function removes commas from an input character vector,
#'   treats consecutive non-numeric characters of them as delimiters,
#'   and extracts number strings from them.
#' @param x An character vector (which must not contain NA and NULL) or a NULL or a NA.
#' @param end_pos The position of the last character of concatenated x.
#'   -1 means using fully concatenated x without trimming.
#' @return The character which contains space-separated numbers in x.
#' @examples
#' splitIntoNumbers("3")
#' splitIntoNumbers("3.")
#' splitIntoNumbers("3.25")
#' splitIntoNumbers(".25")
#' splitIntoNumbers("1,234")
#' splitIntoNumbers(x="1,234", end_pos=4)
#' splitIntoNumbers("1 -2.5")
#' splitIntoNumbers(NULL)
#' splitIntoNumbers(NA)
#' splitIntoNumbers(character())
#' splitIntoNumbers(c("1.", "2", "3.5"))
#' @export
splitIntoNumbers <- function(x, end_pos=-1L) {
    x %>%
        stringr::str_c(sep="", collapse=" ") %>%
        ## Trim large clipboard data
        stringr::str_sub(start=1, end=end_pos) %>%
        stringr::str_replace_all(pattern=",", replacement="") %>%
        stringr::str_extract_all(pattern="-?(\\d*\\.\\d+|\\d+)") %>%
        unlist() %>%
        stringr::str_c(sep="", collapse=" ")
}
