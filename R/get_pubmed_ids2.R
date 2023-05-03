#' Get PubMed Bibliography
#'
#' Harvests bibliographic content from PubMed static "My Bibliography" pages into
#' a data frame.
#'
#' @param url String of length 1 specifying the relevant PubMed URL, e.g.,
#' "https://www.ncbi.nlm.nih.gov/myncbi/<username>/bibliography/public/". For
#' lists that span multiple pages, the function automatically queries each page
#' to establish a complete list, thus only the base link should be fed into the
#' function.
#'
#' @return A data.frame with 11 columns (author list, publication title, journal,
#' year, full publication date, volume, issue, pages, doi, PubMed ID, and
#' PubMed Central ID) and number of rows equal to the number of publications
#' listed on the specified `url`.
#' @export
#'
get_pubmed_bib <- function(url) {
  myQuery <- as.character(url)

  if (!is_url(myQuery)) {
    stop(url, " is not a valid URL.")

  } else {
    scrape <- rvest::read_html(myQuery)

    pages <- scrape |>
      rvest::html_elements("span.totalPages") |>
      rvest::html_text2() |>
      as.integer() |>
      purrr::pluck(1)

    total.pub.count <- scrape |>
      rvest::html_elements("#total-count") |>
      rvest::html_text2() |>
      stringr::str_extract("(\\d)+") |>
      as.integer()

    output.list <- vector(mode = "list", length = pages)

    for (i in 1:pages) {
      if (i == 1) {
        query.url <- myQuery
      } else {
        query.url <- paste0(myQuery, "?page=", i)
      }

      re.scrape <- rvest::read_html(query.url)

      scrape.length <- re.scrape |>
        rvest::html_elements("div.citations") |>
        rvest::html_elements("div.citation-wrap") |>
        rvest::html_elements("div.ncbi-docsum") |>
        length()

      output.colnames <-
        c(
          "Authors",
          "Title",
          "Journal",
          "Year",
          "Pub.Date",
          "Volume",
          "Issue",
          "Pages",
          "doi",
          "PMID",
          "PMCID"
        )
      output.data <-
        data.frame(matrix(ncol = 11, nrow = scrape.length))
      colnames(output.data) <- output.colnames

      output.data$Authors <-
        scrape_pubmed_element(re.scrape, "authors")
      output.data$Title <-
        scrape_pubmed_element(re.scrape, "title")
      output.data$Journal <-
        scrape_pubmed_element(re.scrape, "source")
      output.data$Pub.Date <-
        scrape_pubmed_element(re.scrape, "pubdate")
      output.data$Volume <-
        scrape_pubmed_element(re.scrape, "volume")
      output.data$Issue <-
        scrape_pubmed_element(re.scrape, "issue")
      output.data$Pages <-
        scrape_pubmed_element(re.scrape, "pages")
      output.data$doi <- scrape_pubmed_element(re.scrape, "doi")
      output.data$PMID <- scrape_pubmed_element(re.scrape, "pmid")
      output.data$PMCID <-
        scrape_pubmed_element(re.scrape, "pmcid")
      output.data$Year <-
        stringr::str_sub(output.data$Pub.Date, 1, 4) |> as.integer()

      # some cleaning routines
      # output.data$Pub.Date <-
      #   stringr::str_remove(output.data$Pub.Date, ";")
      output.data <- output.data |>
        dplyr::mutate(Pub.Date = stringr::str_remove(Pub.Date, ";"),
                      # Pub.Date =
                      #   dplyr::if_else(is.na(as.Date(Pub.Date, format = "%Y %b %d")),
                      #     as.Date(paste0(Pub.Date, " 1"), format = "%Y %b %d"),
                      #     as.Date(Pub.Date, format = "%Y %b %d")),
                      Authors = stringr::str_remove(Authors, "\\."),
                      Issue = gsub("[()]", "", Issue),
                      Pages = stringr::str_remove(Pages, ":"),
                      doi = stringr::str_remove(doi, "doi: "),
                      PMID = stringr::str_remove(PMID, "PubMed PMID: "),
                      PMCID = stringr::str_remove(PMCID, "; PubMed Central PMCID: "))
      # output.data$Pub.Date1 <-
      #   as.Date(output.data$Pub.Date, format = "%Y %b %d")
      # if (is.na(output.data$Pub.Date1)) {
      #   output.data$Pub.Date1 <-
      #     as.Date(paste0(output.data$Pub.Date, " 1"), format = "%Y %b %d")
      # }
      # output.data$Authors <-
      #   stringr::str_remove(output.data$Authors, "\\.")
      # output.data$Issue <- gsub("[()]", "", output.data$Issue)
      # output.data$Pages <-
      #   stringr::str_remove(output.data$Pages, ":")
      # output.data$doi <-
      #   stringr::str_remove(output.data$doi, "doi: ")
      # output.data$PMID <-
      #   stringr::str_remove(output.data$PMID, "PubMed PMID: ")
      # output.data$PMCID <-
      #   stringr::str_remove(output.data$PMCID, "; PubMed Central PMCID: ")

      output.list[[i]] <- output.data
    }

  }

  do.call(rbind, output.list)

}


#' Scrape PubMed Element
#'
#' Internal function that harvests relevant data for each element of a citation.
#'
#' @param data the name of the scraped data list (from `rvest::read_html()`).
#' @param class the name of the citation element.
#'
#' @return Text string of data from the relevant citation element.
#' @export
#'
scrape_pubmed_element <- function(data, class) {
  if (class == "title") {
    clas <- "a"
  } else {
    clas <- paste0("span.", class)
  }
  data.r <- data |>
    rvest::html_elements("div.citations") |>
    rvest::html_elements("div.citation-wrap") |>
    rvest::html_elements("div.ncbi-docsum") |>
    rvest::html_element(clas) |>
    rvest::html_text2()

  return(data.r)
}


### Old version (which works)
# get_pubmed_ids2 <- function(url) {
#   myQuery <- as.character(url)
#   if (!is_url(myQuery)) {
#     stop(url, " is not a valid URL.")
#   } else {
#     scrape <- rvest::read_html(myQuery)
#     scrape.length <- scrape |>
#       html_elements("div.citations") |>
#       html_elements("div.citation-wrap") |>
#       html_elements("div.ncbi-docsum") |>
#       length()
#     output.colnames <- c("Authors", "Title", "Journal", "Year", "Pub.Date",
#                          "Volume", "Issue", "Pages", "doi", "PMID", "PMCID")
#     output.data <- data.frame(matrix(ncol = 11, nrow = scrape.length))
#     colnames(output.data) <- output.colnames
#
#     output.data$Authors <- scrape_pubmed_element(scrape, "authors")
#     output.data$Title <- scrape_pubmed_element(scrape, "title")
#     output.data$Journal <- scrape_pubmed_element(scrape, "source")
#     output.data$Pub.Date <- scrape_pubmed_element(scrape, "pubdate")
#     output.data$Volume <- scrape_pubmed_element(scrape, "volume")
#     output.data$Issue <- scrape_pubmed_element(scrape, "issue")
#     output.data$Pages <- scrape_pubmed_element(scrape, "pages")
#     output.data$doi <- scrape_pubmed_element(scrape, "doi")
#     output.data$PMID <- scrape_pubmed_element(scrape, "pmid")
#     output.data$PMCID <- scrape_pubmed_element(scrape, "pmcid")
#     output.data$Year <- stringr::str_sub(output.data$Pub.Date, 1, 4)
#
#     # some cleaning routines
#     output.data$Pub.Date <- stringr::str_remove(output.data$Pub.Date, ";")
#     output.data$Issue <- gsub("[()]", "", output.data$Issue)
#     output.data$Pages <- stringr::str_remove(output.data$Pages, ":")
#     output.data$doi <- stringr::str_remove(output.data$doi, "doi: ")
#     output.data$PMID <- stringr::str_remove(output.data$PMID, "PubMed PMID: ")
#     output.data$PMCID <- stringr::str_remove(output.data$PMCID, "; PubMed Central PMCID: ")
#     return(output.data)
#   }
#
# }
