library(tidyverse)

# Scrape functions ------------------------------------------------------------------

library(rvest)

SafelyRead <- possibly(.f = xml2::read_html, otherwise = NA, quiet = FALSE)

n_failed <- 0

SleepyRead <- function(url) {
  page <- SafelyRead(url); message(url)
  if (is.na(page)) {
    Sys.sleep(3); message("Wait 3 sec!")
    page <- SafelyRead(url)
    if (is.na(page)) {
      Sys.sleep(10); message("Wait 10 sec!")
      page <- SafelyRead(url)
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

GetFromTable <- function(df, SearchedInfo) {
answer <- pull(df, value)[pull(df, info) == SearchedInfo]
if (length(answer) == 0) {
  answer <- as.character(NA)
}
answer
}

GetFromTable <- possibly(GetFromTable, NA, TRUE)


# gg setup --------------------------------------------------------------------------

theme_set(
  theme_light() +
  theme(
    legend.position = "bottom"
  )
)

# Corvo color ======================================================================

CPCOLS <- c("#1B213E", "#BF8F55", "#5C6873", "#F5C832", "#100C08", "#855C24", "#3D454C", "#E0AA26", "#101226", "#D1AF84", "#898E97", "#F9D97C", "#4D4B66", "#DEC5A6", "#A9ABB2", "#FBE3A5", "#78748A") %>% 
  set_names(str_c(c("bl", "br", "g", "y"), 
                  c(rep(2, 4), rep(1, 4), rep(3, 4), rep(4, 4), 5)))
