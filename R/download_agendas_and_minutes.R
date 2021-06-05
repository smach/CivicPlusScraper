

#' Get list of meeting page links from Framingham meeting portal
#'
#' @param the_url string root of URL for meeting listing
#' @param the_start string date in m/d/yyyy format
#' @param the_end string date in m/d/yyyy format
#'
#' @return vector of character strings with URLs of meeting pages
#' @export
#'
get_list_of_meeting_links <- function(the_url = "http://framinghamma.iqm2.com", the_start = "1/1/2021", the_end = "12/31/2021") {
  myurl <- glue::glue("{the_url}/Citizens/calendar.aspx?From={the_start}&To={the_end}")
  my_html <- xml2::read_html(myurl)
  # Step 2 - Find css you want with SelectorGadget
  my_css <- "#ContentPlaceholder1_pnlMeetings a"

  meeting_links <- my_html %>%
    rvest::html_nodes(my_css) %>%
    rvest::html_attr('href')

   meeting_links <- meeting_links[stringr::str_detect(meeting_links, "Detail_Meeting")]

  return(meeting_links)


}






#' Download agenda and/or minutes files from a meeting link page scraped from Framingham meeting portal
#'
#' @param the_meeting_link character string
#' @param the_root character string with website root
#' @param the_dir character string with directory where you want files downloaded
#'
#' @return data frame with info about meeting files downloaded
#' @export
#'
download_data_from_meeting_page <- function(the_meeting_link, the_root = "http://framinghamma.iqm2.com", the_dir = "tmp") {
  meeting_html <- xml2::read_html(paste0(the_root, the_meeting_link))

  meeting_info <- meeting_html %>%
    rvest::html_nodes("#table1 span") %>%
    rvest::html_text() %>%
    paste(collapse = " ")

  meeting_urls <- meeting_html %>%
    rvest::html_nodes("#ContentPlaceholder1_hlPublicMinutesFile , #ContentPlaceholder1_hlPublicAgendaFile") %>%
    rvest::html_attr('href')

  download_function <- function(my_meeting_url, my_meeting_info = meeting_info, my_root = the_root, my_dir = the_dir) {
    download_url <- glue::glue("{my_root}/Citizens/{my_meeting_url}")
    download_file_name <- paste0(my_dir, "/", my_meeting_url)
    download_file_name <- gsub("FileOpen.aspx?Type", "", download_file_name, fixed = TRUE)
    download_file_name <- gsub("=", "", download_file_name, fixed = TRUE)
    download_file_name <- gsub("&", "", download_file_name, fixed = TRUE)
    download_file_name <- gsub("InlineTrue", "", download_file_name, fixed = TRUE)
    download_file_name <- paste0(download_file_name, ".pdf")
    download_url <- gsub("Inline=True", "Inline=False", download_url, fixed = TRUE)
    if(!(file.exists(download_file_name))) {
   # download.file(download_url, destfile = download_file_name, mode = "wb")
      download.file(download_url, destfile = download_file_name, quiet = TRUE)
    if(grepl(paste0(my_dir, "/15"), download_file_name)) {
      the_type <- "Minutes"
    } else if (grepl(paste0(my_dir, "/14"), download_file_name)) {
      the_type <- "Agenda"
    } else {
      the_type <- "Other"
    }
    meeting_df <- data.frame(Meeting = my_meeting_info, File = download_file_name, Type = the_type)
    return(meeting_df)
  }
  }


  if(length(meeting_urls) > 0) {
    Sys.sleep(2)
    info_df <- purrr::map_df(meeting_urls, download_function)

  }



}

