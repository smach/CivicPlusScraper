#' Get list of meetings, agendas, minutes, and links from Framingham meeting portal
#'
#' @param the_url string root of URL for meeting listing
#' @param the_start string date in m/d/yyyy format
#' @param the_end string date in m/d/yyyy format
#' @param my_dir string subdirectory you want files downloaded; set to "" for none
#'
#' @return data frame with URLs of meeting pages, agendas, and minutes as well as meeting info and separate columns for Date and Board (City Council, etc.)
#' @export
#'

get_list_of_meeting_links <- function(the_url = "http://framinghamma.iqm2.com", the_start = "1/1/2021", the_end = "12/31/2021", my_dir = "www") {


  my_url <- glue::glue("{the_url}/Citizens/calendar.aspx?From={the_start}&To={the_end}")

# Step 1 - Load libraries and download HTML
library(dplyr)
library(rvest)

my_html <- xml2::read_html(my_url)
my_css <- ".MeetingLinks a"
my_links <- my_html %>%
  rvest::html_nodes(my_css) %>%
  rvest::html_attr('href')


all_links <- my_html %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr('href')

link_titles <- my_html %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr('title')

link_df <- data.frame(url = all_links, Title = link_titles, stringsAsFactors = FALSE)
link_df <- link_df[grepl("Type=14|Type=15|Detail_Meeting", link_df$url),]

link_df$Meeting <- dplyr::if_else(is.na(link_df$Title), lag(link_df$Title), link_df$Title)

link_df$Meeting <- dplyr::if_else(is.na(link_df$Meeting), lag(link_df$Title, n=2), link_df$Meeting)
link_df <- dplyr::select(link_df, -Title)

link_df$Date <- gsub(".*?DAY\\,\\s(.*202\\d).*?$", "\\1", link_df$Meeting)
link_df$Date <- lubridate::mdy(link_df$Date)
link_df$Board <- gsub("^.*?Board:\\\t(.*?)\\\r.*$", "\\1", link_df$Meeting)
# http://framinghamma.iqm2.com/Citizens/FileOpen.aspx?Type=14&ID=4681&Inline=True
# http://framinghamma.iqm2.com/Citizens/Detail_Meeting.aspx?ID=12600


if(my_dir != "") {
  link_df$download_file_name <- paste0(my_dir, "/", link_df$url)
}

link_df$download_file_name <- gsub("FileOpen.aspx?Type", "", link_df$download_file_name, fixed = TRUE)
link_df$download_file_name <- gsub("=", "", link_df$download_file_name, fixed = TRUE)
link_df$download_file_name <- gsub("&", "", link_df$download_file_name, fixed = TRUE)
link_df$download_file_name <- gsub("InlineTrue", "", link_df$download_file_name, fixed = TRUE)
link_df$download_file_name <- paste0(link_df$download_file_name, ".pdf")



link_df$url <- dplyr::if_else(grepl("FileOpen", link_df$url), paste0(the_url, "/Citizens/", link_df$url), paste0(the_url, link_df$url))
link_df$url <- gsub("Inline=True", "Inline=False", link_df$url, fixed = TRUE)
rownames(link_df) <- NULL
return(link_df)

}
