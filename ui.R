## ui.R

library(shiny)
library(plotly)
library(jsonlite)
library(tsibble)
library(slider)
library(janitor)
library(tidyverse)
library(here)
library(showtext)


shinyUI(fluidPage(
  
  # where to get CSS
  includeCSS(path = here::here("www", "styles.css")),
  # Application title
  titlePanel("COVID-19 Deaths in the US"),
  
  div(
    class = "img-mask",
    img(src = "mask4.png")
  ),  
  
  #---- main sections ----
  div(
    class = "flex-outer-container", # container for everything except title and footer
    
    #---- three content rows ----
    div(
      class = "us-flex-wrap-row", # national-stats
      div(
        class = "top-card",
        tags$span(style = "font-size: .65em;", "US KNOWN DEATHS"),
        uiOutput("deaths")
      ),
      div(
        class = "top-card",
        plotOutput("us_line", width = "210px", height = "82px")
      ),
      div(
        class = "top-card",
        plotOutput("us_bars", width = "210px", height = "82px")
      ),
      div(
        class = "top-card",
        uiOutput("time"),
        uiOutput("range"),
        uiOutput("cases"),
        tags$a(style = "font-size: .5em;",  "https://github.com/nytimes/covid-19-data", href="https://github.com/nytimes/covid-19-data")
      )
    ),
    
    div(
      class = "flex-row", # to hold the state info cards AND ggplot
      div(
        class = "flex-card-col", # the state info cards
        div(
          class = "card",
          div(
            class = "state-card-header",
            uiOutput("my_state_total"),
            uiOutput("my_state_name")
          ),
          div(
            class = "card-body",
            p(
              "This graph plots the number of deaths per day, starting on March 11th when the World Health Organization declared the novel coronavirus to be a global pandemic."
            ),
            p(span(style="color:#444448;font-weight:600;margin-right:.5em;", "*"), "WHO declares global pandemic"),
            p(span(style="color:#f47171;font-weight:600;margin-right:.5em;", "x"), "Mobility restrictions begin"),
            p(span(style="color:#24a8a8;font-weight:600;margin-right:.5em;", "o"), "Reopening begins"),
          )
        )
      ),
      div(
        class = "flex-chart-col",
        div(
          class = "flex-title-wrap-row",
          div(
            class = "flex-title-before-picker",
            "Select a state:"
          ),
          div(
            class = "flex-state-picker",
            uiOutput(
              "state_picker"
            )
          )
        ),
        div(
          class = "chart-title",
          uiOutput("daily_chart_title")
        ),
        div(
          class = "flex-chart",
          plotOutput( # the ggplot for state deaths
            "state_facets",
            height = "675px",
            width = "100%"
          )
        )
      )
    ),
    
    div(
      class = "flex-row", # animation row
      div(
        class = "flex-card-col",
        div(
          class = "card",
          div(
            class = "card-header",
            tags$img(src = "growth.png")
          ),
          div(
            class = "card-body",
            div(class = "card-title", "Slopes"),
            p("When new deaths grow exponentially, you'll see a roughly 45 degreee upward line.")
          )
        ),
        div(
          class = "card",
          div(
            class = "card-header",
            tags$img(src = "falling.png")
          ),
          div(
            class = "card-body",
            div(class = "card-title", "Spacing"),
            p("When deaths slow down, dots will get closer. When new deaths stop growing exponentially, a state will begin dropping.")
          )
        ),
        div(
          class = "card",
          div(
            class = "card-header",
            tags$img(src = "rolling.png")
          ),
          div(
            class = "card-body",
            div(class = "card-title", "7-Day Avergaes"),
            p("The value for each day is a rolling 7-day average of raw daily counts. We use a 7-day rolling average because it smooths over reporting inconsistencies, such as when a hospital may not report deaths on a weekend, but on a Monday may report 3 days of deaths.")
          )
        )
      ),
      div(
        class = "flex-chart-col",
        div(
          class = "flex-title-wrap-row",
          div(
            class = "flex-title-before-picker",
            "Click play to animate the rate of new known deaths"
          )
        ),
        
        div(
          class = "flex-chart",
          plotlyOutput(
            "animation",
            height = "675px",
            width = "100%"
          )
        )
      )
    ),
    
    div(
      class = "flex-footer-container", # container for footer row
      div(
        class = "flex-footer-wrap-row", # footer cards
        div(
          class = "card-body", # footer card 1 of 2
          div(class = "card-title", "The Complications"),
          tags$p(
            tags$a("Why not per capita?", href="https://robjhyndman.com/hyndsight/logratios-covid19/") 
          ),
          tags$p(
            tags$a("Why not cases?", href="https://fivethirtyeight.com/features/coronavirus-case-counts-are-meaningless/") 
          ),
          tags$p(
            tags$a("Deaths are undercounted.", href="https://www.nytimes.com/2020/04/05/us/coronavirus-deaths-undercount.html") 
          ),
          tags$p(
            tags$a("Why log scales?", href="https://www.vice.com/en_us/article/7kz3a4/how-to-read-the-coronavirus-graphs") 
          )
        ),
        div(
          class = "card-body", # footer card 2 of 2
          div(class = "card-title", "The Makers"),
          tags$p(
            tags$a("Jeremy Allen on Twitter", href="https://twitter.com/jeremy_data") 
          ),
          tags$p(
            "Travis Knoche"
          ),
          tags$p(
            tags$a("The NYT data", href="https://github.com/nytimes/covid-19-data") 
          )
        )
      )
    )
  ), # closes outer-flex-container
 
)) # closes fluidPage and ShinyUI
