library(rvest)
library(stringr)
library(data.table)

# define a function that can get a html table from the web
get_html_table <- function(url, xpath, header = NA, column = NULL) {

  if(!is.null(column)) {
    url %>%
      read_html() %>% # download page source then extract html table
      html_nodes(xpath = xpath) %>%
      html_table(header = header) %>%
      .[[1]][column] # get dataframe from list and get column
  } else {
    url %>%
      read_html() %>% # download page source then extract html table
      html_nodes(xpath = xpath) %>%
      html_table(header = header) %>%
      .[[1]] %>%
      as.data.table()
  }

}

# set variables for url and xpath to scrape a table of state closing and opening dates for COVID-19 response
my_url <- "https://ballotpedia.org/States_with_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020"
my_xpath <- '//*[@id="mw-content-text"]/table[1]'

# get the whole table
stay_at_home_table <- get_html_table(
  url = my_url,
  xpath = my_xpath,
  header = FALSE
)

# extract row 2 values as a vector for use as column names
col_names <- stay_at_home_table[2, paste(.SD)] %>%
  str_to_lower() %>%
  str_replace_all(" ", "_")

# keep rows 3 through the end, set new column names, drop link column
stay_at_home_table <- stay_at_home_table[3:.N]
setnames(stay_at_home_table, col_names)
stay_at_home_table[, link_to_order := NULL]

# clean clean clean
stay_at_home_table[ # remove citations
  , order_dates := str_remove_all(order_dates, "\\[..?\\]")
  ][ # fix Alaska dates
    state == "Alaska", order_dates := "March 28 - TBD"
    ][ # split date column into two columns and remove leading and trailing white space
      , c("start", "end") := tstrsplit(order_dates, "-", fixed = TRUE)
      ][ # trim white space
        , c("start", "end") := lapply(.SD, str_trim), .SDcols = c("start", "end")
        ][ # drop the original date column
          , order_dates := NULL
          ][ # replace none and tbd with NA
            , c("start", "end") := lapply(.SD, function(x) fifelse(x %in% c("None", "TBD"), NA_character_, x))
            , .SDcols = c("start", "end")
            ][ # replace NAs with word None
              official_name_of_order == "N/A", official_name_of_order := "None"
              ][ # add year to dates so we can convert them
                , c("start", "end") := lapply(.SD, function(x) fifelse(!is.na(x), paste(x, "2020"), x)),
                , .SDcols = c("start", "end")
                ][# then convert all to proper date class
                  , c("start", "end") := lapply(.SD, anytime::anydate), .SDcols = c("start", "end")
                  ]