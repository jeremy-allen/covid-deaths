library(here)
library(imputeTS)
library(data.table)
library(pins)

# get state data from The New York Times github
states <- fread(
  input = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
  key = c('state', 'date')
)

# Get state names for ones with more than 19 deaths
# if you want the total per group, but the group entries are
# cumulative, then you only want the last entry per group
# .N as an index on .SD, will give you the last row, then by = state
# then filter for deaths 20 or more
# and keep only the state names
state_totals <- states[, .SD[.N], by = state]

over19 <- state_totals[deaths >= 20, state]

states <- states[state %chin% over19,]


count_cols = c('cases', 'deaths')
states[ , paste0('new_', count_cols) := lapply(.SD, function(x) x - shift(x, n = 1L, type = "lag")),
        by = state, .SDcols = count_cols]

message("> starting download of national data")

# get national data from The New York Times github
us <- fread(
  input = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv",
  key = 'date'
)

# add column for new deaths
us[ , paste0('new_', count_cols) := lapply(.SD, function(x) x - shift(x, n = 1L, type = "lag")), .SDcols = count_cols]

message("> adding rolling means")

# add rolling means
us[, `:=`(nd_avg = frollmean(new_deaths, 7L, align = "right"),
          deaths_avg = frollmean(deaths, 7L, align = "right"))]
# fill NA with 0
setnafill(us, type = "const", fill = 0L, cols=c("deaths_avg","nd_avg"))
# round up and convert to integer
us[, `:=`(nd_avg = as.integer(ceiling(nd_avg)),
          deaths_avg = as.integer(ceiling(deaths_avg)))]

us_cases <- us[, last(cases)] %>% 
  formatC(digits = 0, format = "d", big.mark = ",")

us_deaths <- us[, last(deaths)] %>% 
  formatC(digits = 0, format = "d", big.mark = ",")

date_range <- states[, range(unique(date))]
names(date_range) <- c("first", "last")

# states a user can choose from
state_choices <- states[, unique(state)] 

# get_html_table <- function(url, xpath, header = NA, column = NULL) {
#   
#   if(!is.null(column)) {
#     url %>%
#       read_html() %>% # download page source then extract html table
#       html_nodes(xpath = xpath) %>%
#       html_table(header = header) %>% 
#       .[[1]][column] # get dataframe from list and get column
#   } else {
#     url %>%
#       read_html() %>% # download page source then extract html table
#       html_nodes(xpath = xpath) %>%
#       html_table(header = header) %>% 
#       .[[1]] %>% 
#       as.data.table()
#   }
#   
# }
# 
# set variables for url and xpath to scrape a table of state closing and opening dates for COVID-19 response
# my_url <- "https://ballotpedia.org/States_with_lockdown_and_stay-at-home_orders_in_response_to_the_coronavirus_(COVID-19)_pandemic,_2020"
# my_xpath <- '//*[@id="mw-content-text"]/table[1]'
# 
# # get the whole table
# stay_at_home_table <- get_html_table(
#   url = my_url,
#   xpath = my_xpath,
#   header = FALSE
# )
# 
# # extract row 2 values as a vector for use as column names
# col_names <- stay_at_home_table[2, paste(.SD)] %>%
#   str_to_lower() %>%
#   str_replace_all(" ", "_")
# 
# # keep rows 3 through the end, set new column names, drop link column
# stay_at_home_table <- stay_at_home_table[3:.N]
# setnames(stay_at_home_table, col_names)
# stay_at_home_table[, link_to_order := NULL]
# 
# # clean clean clean
# stay_at_home_table[ # remove citations
#   , order_dates := str_remove_all(order_dates, "\\[..?\\]")
#   ][ # fix Alaska dates
#     state == "Alaska", order_dates := "March 28 - TBD"
#     ][ # split date column into two columns and remove leading and trailing white space
#       , c("start", "end") := tstrsplit(order_dates, "-", fixed = TRUE)
#       ][ # trim white space
#         , c("start", "end") := lapply(.SD, str_trim), .SDcols = c("start", "end")
#         ][ # drop the original date column
#           , order_dates := NULL
#           ][ # replace none and tbd with NA
#             , c("start", "end") := lapply(.SD, function(x) fifelse(x %in% c("None", "TBD"), NA_character_, x))
#             , .SDcols = c("start", "end")
#             ][ # replace NAs with word None
#               official_name_of_order == "N/A", official_name_of_order := "None"
#               ][ # add year to dates so we can convert them
#                 , c("start", "end") := lapply(.SD, function(x) fifelse(!is.na(x), paste(x, "2020"), x)),
#                 , .SDcols = c("start", "end")
#                 ][# then convert all to proper date class
#                   , c("start", "end") := lapply(.SD, anytime::anydate), .SDcols = c("start", "end")
#                   ]

message("> reading in stay at home table")

stay_at_home_table <- readRDS("home_date.RDS")

states <- states[
  , `:=`( # rolling averages for new cases and new deaths
    nc_avg = frollmean(new_cases, 7L, align = "right"),
    nd_avg = frollmean(new_deaths, 7L, align = "right")
  )
  , by = state
][
  , c("nc_avg", "nd_avg") := lapply(.SD, ceiling), .SDcols = c("nc_avg", "nd_avg")
][
  stay_at_home_table, on = "state"
][
  , plot_label_end := "REOPENING"
][
  , plot_label_start := "CLOSING"
]


stats_list <- list(
  us_cases = us_cases,
  us_deaths = us_deaths,
  date_range = date_range,
  state_choices = state_choices,
  state_totals = state_totals,
  closure_table = stay_at_home_table
)




# ---- state accumulated data ----

#state_dat <- states[state %in% input$state_picker,]
state_dat <- states
setnafill(state_dat, type = "const", fill = 0L, cols=c("new_deaths"))

# names of top states in order
#state_levels <- state_dat[, .(n = last(new_deaths)), by = state][order(-n)][, state] 

message("> adding rolling means on 282")

# add rolling means
state_dat[, `:=`(nd_avg = frollmean(new_deaths, 7L, align = "right"),
                 deaths_avg = frollmean(deaths, 7L, align = "right")), by = state]
# fill NA with 0
setnafill(state_dat, type = "const", fill = 0L, cols=c("deaths_avg","nd_avg"))
# round up and convert to integer
state_dat[, `:=`(nd_avg = as.integer(ceiling(nd_avg)),
                 deaths_avg = as.integer(ceiling(deaths_avg)))]

# just each day after a state had at least 10 deaths
since10 <- state_dat[
  , days_since_10 := {
    date0 = date[which(deaths >= 10L)[1L]]
    if (is.na(date0)) NA_integer_ else date - date0
  }, by = state][days_since_10 >= 0, ][
    , !(c("fips", "cases", "new_cases"))
  ][ # change 0s to NA so we can impute them
    nd_avg == 0L, nd_avg := NA_integer_
  ][ # we impute them because 0s will crash our y axis log scale
    , nd_avg := as.integer(ceiling(imputeTS::na_kalman(nd_avg))), by = state
  ][
    , state := as.factor(state), by = state
  ]

message("> starting accumulate_by on 308")

# define function that will be intermediate cumulative states for animation
accumulate_by <- function(states, var) {
  var <- lazyeval::f_eval(var, states)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(states[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

# make list with each state its own element
since10 <- split(
  x = since10,
  by = "state"
)

since10 <- lapply(since10, as.data.frame)

# apply function to each data.table in the list
states_accumulated <- lapply(since10, accumulate_by, var = ~days_since_10)

# since10_accumulated <- as.data.frame(since10) %>% 
#   accumulate_by(var = ~days_since_10)


#---- pin things to Connect ----

board_register_rsconnect()

pin(I(states), name = "cd_states")
pin(I(us), name = "cd_us")
pin(I(stats_list), name = "cd_stats_list")
pin(I(states_accumulated), name = "cd_states_accumulated")
