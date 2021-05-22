library(tidyverse)

# Scrape functions ------------------------------------------------------------------

library(rvest)

SafelyRead <- possibly(.f = xml2::read_html, otherwise = NA, quiet = FALSE)

n_failed <- 0

SleepyRead <- function(url) {
  page <- SafelyRead(url)
  if (is.na(page)) {
    Sys.sleep(3); message("Wait 3 sec!")
    page <- SafelyRead(url)
    if (is.na(page)) {
      Sys.sleep(10); message("Wait 10 sec!")
      page <- SafelyRead(url)
      if (is.na(page)) {
        Sys.sleep(30); message("Wait 30 sec!")
        page <- SafelyRead(url)
        if (is.na(page)) {
          Sys.sleep(120); message("Wait 2 min!")
          page <- SafelyRead(url)
        }
      }
    }
  }
  if (is.na(page)) {
    message("I failed you! :(")
    n_failed <<- n_failed + 1
    if (n_failed == 10) {
      Sys.sleep(600); message("Failed 10 times in a row :'( >> 10 minutes rest!")
      n_failed <<- 0
    }
  } else {
    n_failed <<- 0
  }
  page
}

GetHTMLText <- function(page, node) {
html_nodes(page, node) %>% 
  html_text()
}

GetHTMLText <- possibly(GetHTMLText, NA, TRUE)

GetURL <- function(page, node) {
  html_nodes(page, node) %>% 
    html_attr("href")
}

GetURL <- possibly(GetURL, NA, TRUE)