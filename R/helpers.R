

#' Get number of pages in a PDF file
#'
#' @param the_file character string with name of file
#'
#' @return integer with number of pages in the PDF
#' @export
#'
get_pages <- function(the_file) {
  x <- pdftools::pdf_info(the_file)
  return(x$pages)
}




#' Convert meeting agenda or minutes to text. Limit to max number of pages if desired
#'
#' @param the_file character string name of file
#' @param max_pages integer maxinum number of pages of text to return. If 0, return all
#'
#' @return character string with text of PDF
#' @export
#'
get_text_from_pdf <- function(the_file, max_pages = 0) {
  text_vec <- pdf_text(the_file)
  if(max_pages > 0) {
   if(length(text_vec) > max_pages) {
    text_vec <- text_vec[1:max_pages]
   }
  }
  my_text <- paste(text_vec, collapse = "\n\n")
  return(my_text)
}
