## server.R

library(shiny)
library(plotly)
library(janitor)
library(here)
library(stringr)
library(data.table)
library(showtext)
library(pins)

main_neutral <- "#d9d6d3"
darker_neutral <- "#cdc6bf"
lighter_neutral <- "#dbd9d7"
main_blue <- "#5387a6"
accent_orange <- "#d1814f"
accent_blue <- "#011a5f"
grey_dark <- "#444448"

restriction_red <- "#f47171"
reopen_green <- "#24a8a8"

font_add(family = "lato",
         regular = "Lato-Regular.ttf",
         bold = "Lato-Bold.ttf",
         italic = "Lato-Italic.ttf",
         bolditalic = "Lato-BoldItalic.ttf")
showtext_auto()



#---- get pinned data here ----

board_register_rsconnect()

# stats for opening graphs
stats_list <- pin_get("cd_stats_list", board = "rsconnect")

# list of all state's accumulated data
states_accumulated <- pin_get("cd_states_accumulated", board = "rsconnect")

# us data
us <- pin_get("cd_us", board = "rsconnect")

# states data
states <- pin_get("cd_states", board = "rsconnect")



#---- start server ----

shinyServer(function(input, output, session) {
  
  message("> starting render UIs at top of server")
  
  withProgress(message = "Building US charts ...",
               value = 0, {
                 
                 incProgress(1/4) 
                 
                 output$time <- renderUI({
                   div(style = "font-size: .7em;",
                       HTML(paste0(stats_list[["data_pulled"]], ": DATA PULLED"))
                   )
                 })
                 
                 output$range <- renderUI({
                   div(style = "font-size: .7em;",
                       HTML(paste0(tags$b(stats_list[["date_range"]][2]), ": LATEST DATE IN DATA"))
                   )
                 })
                 
                 output$cases <- renderUI({
                   div(style = "font-size: .7em;",
                       HTML(paste0(tags$b(stats_list[["us_cases"]]), ": TOTAL CASES IN US"))
                   )
                 })
                 
                 output$deaths <- renderUI({
                   div(style = "font-size: 30px;",
                       tags$p(tags$b(stats_list[["us_deaths"]]))
                   )
                 })
                 
                 message("> starting top row US date plots")
                 
                 incProgress(1/4)
                 
                 output$us_line <- renderPlot({
                   us %>% 
                     ggplot(aes(x = date, y = deaths)) +
                     geom_text(x = as.Date("2020-07-01"), y = 8, label = "Log",
                               color = grey_dark, size = 5) +
                     geom_line(size = .75, color = grey_dark) +
                     scale_x_date(limits = c(as.Date("2020-03-01"), NA),
                                  date_breaks = "8 months",
                                  date_labels = "%b-%y") +
                     scale_y_continuous(trans = "log2",
                                        position = "right",
                                        breaks = (c(500000)),
                                        limits = c(NA,1000000)) +
                     labs(x = "",
                          y = "") +
                     theme_minimal() +
                     theme(
                       text = element_text(family = "lato", color = grey_dark, size = 12*1.25),
                       plot.background = element_rect(fill = main_neutral, color = main_neutral),
                       panel.background = element_rect(fill = main_neutral, color = main_neutral),
                       panel.grid = element_blank(),
                       axis.text.x = element_text(angle = 0)
                     )
                 })
                 
                 incProgress(1/4)
                 
                 output$us_bars <- renderPlot({
                   us %>% 
                     ggplot(aes(x = date, y = new_deaths)) +
                     geom_col(fill = grey_dark, alpha = .3) +
                     geom_line(aes(x = date, y = nd_avg),
                               color = grey_dark,
                               size = .8,
                               alpha = .7) +
                     geom_text(x = as.Date("2020-03-28"), y = 3800, label = "Daily",
                               color = grey_dark, size  = 5) +
                     # ylim(0,160000) +
                     scale_x_date(limits = c(as.Date("2020-03-01"), NA),
                                  date_breaks = "8 months",
                                  date_labels = "%b-%y") +
                     labs(x = "",
                          y = "") +
                     theme_minimal() +
                     theme(
                       text = element_text(family = "lato", color = grey_dark, size = 12*1.25),
                       plot.background = element_rect(fill = main_neutral, color = main_neutral),
                       panel.background = element_rect(fill = main_neutral, color = main_neutral),
                       panel.grid = element_blank(),
                       axis.text.x = element_text(angle = 0)
                     )
                 })
                 
                 output$state_picker <- renderUI({
                   
                   selectInput(
                     inputId = "state_picker",
                     label = NULL,
                     choices = c(" ", stats_list[["state_choices"]]),
                     multiple = FALSE,
                     selected = "Georgia",
                     width = "180px"
                   )
                   
                 }) # ends state_picker output
                 
                 output$my_state_total <- renderUI({
                   num <- stats_list[["state_totals"]][state %in% input$state_picker, deaths]
                   HTML(formatC(num, big.mark = ","))
                 })
                 
                 output$my_state_name <- renderUI({
                   HTML(paste0(input$state_picker, ' Known Deaths'))
                 })
                 
                 since10_accumulated <- eventReactive(input$state_picker, {
                   
                   req(input$state_picker)
                   
                   states_accumulated[[input$state_picker]]
                   
                 })
                 
                 incProgress(1/4)
                 
               }) # ends withProgress
  
  
  
  #---- daily counts ----
  
  max_plot_date <- max(Sys.Date(), states[, max(end, na.rm = TRUE)])
  
  output$state_facets <- renderPlot({
    req(input$state_picker)
    
    withProgress(message = "Building chart of daily deaths for chosen state...",
                 value = 0, {
                   
                   incProgress(1/2)
                   
                   p <-  states %>% 
                     filter(state == input$state_picker) %>%
                     ggplot(aes(x = date, y = new_deaths)) + 
                     geom_bar(stat = "identity",
                              fill = main_blue,
                              #color = main_blue,
                              width = 1,
                              alpha = .3) +
                     geom_line(aes(x = date, y = nd_avg),
                               color = accent_orange,
                               size = 1,
                               alpha = 1) +
                     geom_text(aes(x = as.Date("2020-03-11"), y = .1, label = "*"),
                               size = 8, color = grey_dark) +
                     geom_text(aes(x = start, y = .1, label = "x"),
                               size = 6, color = restriction_red) +
                     geom_text(aes(x = end, y = .1, label = "o"),
                               size = 6, color = reopen_green) +
                     scale_x_date(limits = c(as.Date("2020-03-10"), max_plot_date),
                                  date_breaks = "3 months",
                                  date_labels = "%b-%y") +
                     labs(y = "",
                          x = "") +
                     theme(
                       text = element_text(family = "lato", size = 14),
                       axis.text = element_text(color = grey_dark),
                       axis.text.x = element_text(angle = 0),
                       axis.title = element_text(color = grey_dark),
                       panel.background = element_rect(fill = main_neutral, color = main_neutral),
                       plot.background = element_rect(fill = main_neutral, color = main_neutral),
                       panel.grid = element_blank(),
                       plot.title = element_text(color = grey_dark, size = 28),
                       plot.subtitle = element_text(color = grey_dark, size = 22),
                       plot.margin = unit(c(1,0,1,0), "pt")
                     )
                   
                   # ggplotly(p) %>% 
                   #   layout(margin = list(l = 0, r = 0, b = 50, t = 0))
                   
                   incProgress(1/2)
                   
                 }) #ends withProgress
    
    p
    
  }) # ends renderPlot for state
  
  
  # ---- animated plot ----
  output$animation <- renderPlotly({
    
    req(input$state_picker)
    message("> starting renderPlotly")
    
    withProgress(message = "Building awesome animated plot for you. Hit Play when it's ready...",
                 value = 0, {
                   
                   output$daily_chart_title <- renderUI({
                     
                     HTML(paste0("Daily Counts of New Deaths in ",input$state_picker))
                     
                   })
                   
                   
                   output$animation_chart_title <- renderUI({
                     
                     HTML(paste0("Rise and Fall of New Deaths in ",input$state_picker))
                     
                   })
                   
                   incProgress(2/4)
                   
                   p <- since10_accumulated() %>% 
                     plot_ly(
                       x = ~deaths_avg,
                       y = ~nd_avg,
                       split = ~state,
                       frame = ~frame,
                       type = "scatter",
                       mode = "lines+markers",
                       line = list(simplify = FALSE,
                                   color = accent_orange),
                       marker = list(color = main_blue),
                       hovertemplate = paste(
                         #"%{text}<br>",
                         "%{y:,.0f}: %{yaxis.title.text}<br>",
                         "%{x:,.0f}: %{xaxis.title.text}<br>"
                         #"<extra>{fullData.split}</extra>"
                       )
                     ) %>% layout(
                       font = list(family = "lato"),
                       plot_bgcolor = main_neutral,
                       paper_bgcolor = main_neutral,
                       margin = list(l = 0, r = 0, b = 50, t = 0),
                       xaxis = list(
                         type = "log",
                         range = c(.2, 5),
                         title = paste0("total known deaths since " , input$state_picker, " recorded at least 10 (7-day rolling average)"),
                         titlefont = list(color = grey_dark),
                         tickfont = list(color = grey_dark)
                       ),
                       yaxis = list(
                         type = "log",
                         range = c(-.3, 3.5),
                         title = "new known deaths (7-day rolling average)",
                         titlefont = list(color = grey_dark),
                         tickfont = list(color = grey_dark)
                       )
                     ) # ends layout
                   
                   fig <- p %>% ############## HOW CAN THIS BE FASTER?
                     plotly::animation_opts(
                       frame = 35, # changed from 250
                       transition = 35, # changed from 250
                       redraw = FALSE
                     ) 
                   
                   fig <- fig %>%
                     plotly::animation_slider(
                       hide = TRUE
                     )
                   
                   fig <- fig %>%
                     plotly::animation_button(
                       x = .48,
                       xanchor = "left",
                       y = .96,
                       yanchor = "bottom",
                       label = "PLAY >>",
                       bgcolor = accent_orange,
                       bordercolor = accent_orange,
                       font = list(color = accent_blue)
                     )
                   
                   incProgress(2/4)
                   
                 }) # ends withProgress
    fig
    
  }) # ends render plotly
  
  
}) # ends Shinyserver
