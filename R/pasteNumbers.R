#' Paste numbers from your clipboard
#'
#' Call this function as an addin to paste numbers from your clipboard
#'
#' @export
pasteNumbers <- function() {
    clipr::read_clip() %>%
        splitIntoNumbers() %>%
        rstudioapi::insertText()
}

splitIntoNumbers <- function(s) {
    s %>%
        ## Trim large clipboard data
        stringr::str_sub(start=1, end=1024) %>%
        stringr::str_replace_all(pattern=",", replacement="") %>%
        stringr::str_extract_all(pattern="-?(\\d*\\.\\d+|\\d+)") %>%
        unlist() %>%
        stringr::str_c(sep="", collapse=" ")
}
