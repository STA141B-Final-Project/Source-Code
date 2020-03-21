library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(tidyverse)
library(jsonlite)
library(httr)
library(rvest)
library(plotly)
library(base)


# Get symbols list for all companies using the Symbols List API

url <- "https://financialmodelingprep.com/api/v3/company/stock/list"
r <- GET(url) # GET the url for the API
symbols_list <- content(r, as = "text") # Grab the content from the url in JSON format
comp_symbol <- fromJSON(symbols_list)$symbol # Convert JSON content into R object (data frame of symbols)

# Create a column within the data frame that has both the company name and symbol together
# Arrange by stock price in descending order
# Filter out all stocks associated with foreign stock exchanges (not United States)
comp <- na.omit(comp_symbol) %>%
  mutate(company = str_c(name, " ", "(", symbol, ")")) %>%
  arrange(desc(price)) %>%
  filter(exchange != "Paris" & exchange != "YHD" & exchange != "Brussels" & exchange != "Amsterdam" & exchange != "Toronto")

# Get all companies with the top 500 stock price
# Filter out all companies that are missing data or are causing error messages
"%ni%" <- Negate("%in%")
comp <- comp[1:500, ]
comp <- comp %>%
  filter(symbol %ni% c("BSPAX", "MSG", "SHW", "SAM", "BSPIX", "DJCO", "ROLA", "UNH", "VIIIX", "VINIX", "SFLA", "RTLA", "AMT", "MFLA", "CLX", "HD", "ACN", "EL", "AON", "GS", "BTIIX", "BTIEX", "DNB", "CCI", "LCIAX", "DODGX", "RYVLX", "RYVYX", "VTCLX", "VHCAX", "BTTRX", "PG", "EDU", "SJM", "TTMIX", "PRMTX", "BTTTX", "PXUS", "VPMAX", "ACTEX", "VPMCX", "GBIL", "EVFTC", "GRBIC", "BVNSC", "EVGBC", "EVLMC", "FOANC", "MOGLC", "RPIBC", "EVSTC", "IVFGC", "CRUSC", "HFGIC", "OKDCC", "IVENC", "IVFVC", "TBCIX", "TRBCX"))


# Scrape all news headlines with their url links from financial modeling prep

html <- read_html("https://financialmodelingprep.com/")
news_links <- html %>%
  html_nodes("div.articles") %>%
  html_nodes("a") %>%
  html_attr("href") # Grab url links under "href" attribute

# Add to the beginning of each url link the main website link
for (i in 1:10) {
  news_links[i] <- paste0("https://financialmodelingprep.com", news_links[i])
}

# Grab all the titles of the news headlines
news_titles <- html %>%
  html_nodes("h4.article-title") %>%
  html_text()

# Gather first ten urls and corresponding titles into a data frame
news <- tibble(urls = news_links[1:10], titles = news_titles[1:10])

# Create third column called links that combines urls and titles into html format
news$links <- paste0("<a href='", news$urls, "' target='_blank'>", news$titles, "</a>")


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"), # Give a theme to the UI

  useShinydashboard(),

  titlePanel("Stock Price Trends"),

  sidebarLayout(

    # Create Sidebar where user can input information
    sidebarPanel(
      width = 3,

      # Allow user to select company stock of choice from dropdown list
      selectInput("symbol", "Choose a company:",
        choices = comp$company,
        selected = "Apple Inc. (AAPL)"
      ),

      # User can check whether or not he or she would like to see a second company
      checkboxInput("comp2", "Add a second company for comparison", FALSE),

      # Creates option for user to select a second company stock from the same list of companies
      uiOutput("second_select"),

      # Input date range, starting from the earliest date of data, to today's date as the default date range
      dateRangeInput("dates",
        "Select date range:",
        start = as.Date("2015-03-09", "%Y-%m-%d"),
        end = as.Date(as.character(Sys.Date()), "%Y-%m-%d")
      ),

      # Input to select which variable the user would like on the Y-axis of the time series plot
      selectInput("yvar1", "Choose graph Y-axis variable:",
        choices = c(
          "Opening Price" = "open",
          "High Price" = "high",
          "Low Price" = "low",
          "Closing Price" = "close",
          "Percentage Change" = "changePercent"
        )
      )
    ),

    mainPanel(

      # Suppress warning messages in shiny webpage
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),

      tabsetPanel(

        # Tab for showing the company logos
        tabPanel(
          "Selected Companies",

          box(
            title = "Company 1 Logo", status = "info", solidHeader = TRUE,
            width = 5, align = "center",
            uiOutput("img")
          ),

          # Keep the second logo hidden unless user adds second company
          conditionalPanel(
            condition = "input.comp2 == true",
            box(
              title = "Company 2 Logo", status = "warning", solidHeader = TRUE,
              width = 5, align = "center",
              uiOutput("img2")
            )
          )
        ),

        # Tab for showing the time series plots of stock prices
        tabPanel(
          "Time Series Plots",

          box(
            title = "Company 1 Plot", width = 6, height = "400px", status = "primary",
            solidHeader = TRUE,
            plotlyOutput("stock_time", width = "350px", height = "300px")
          ),

          # Keep second plot hidden unless user adds second company
          conditionalPanel(
            condition = "input.comp2 == true",
            box(
              title = "Company 2 Plot", width = 6, height = "400px", status = "success",
              solidHeader = TRUE,
              plotlyOutput("stock_time2", width = "350px", height = "300px")
            )
          )
        ),

        # Tab for news headline links, formatted as one column of a data table
        tabPanel(
          "Top Market Headlines",

          DT::dataTableOutput("headlines")
        )
      )
    )
  )
)


# Define server for application
server <- function(input, output) {

  # Use the Company Profile API by first getting the url link, using the company selected by the user
  profile_url <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}",
      symbol = input$symbol %>%
        str_extract_all("(?<=\\().+?(?=\\))") %>% # Extract the symbol by using regex
        unlist()
    )
  })
  r_profile <- reactive({
    GET(profile_url())
  })
  json_profile <- reactive({
    content(r_profile(), as = "text")
  })

  # Grab the image link from the profile
  company_image <- reactive({
    fromJSON(json_profile())$profile$image %>% unlist()
  })

  # Use image link to output company logo in UI
  output$img <- renderUI({
    tags$img(src = company_image())
  })


  # Use the Historical Price API by first getting the url link, using the company selected by the user
  prices_url <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/historical-price-full/{symbol}",
      symbol = input$symbol %>%
        str_extract_all("(?<=\\().+?(?=\\))") %>%
        unlist()
    )
  })
  r_prices <- reactive({
    GET(prices_url())
  })
  json_prices <- reactive({
    content(r_prices(), as = "text")
  })

  # Obtain the historical prices data table for the inputted company stock
  stock_ts <- reactive({
    fromJSON(json_prices())$historical
  })

  # Tranform all dates in data table from characters to dates
  stock_ts2 <- reactive({
    data <- stock_ts()
    data$date <- as.Date(data$date, "%Y-%m-%d")
    data
  })

  # Filter the dates column from the data table based on the date range input from user
  stock_ts_range <- reactive({
    stock_ts2() %>%
      filter(date >= as.Date(input$dates[1]) & date <= as.Date(input$dates[2]))
  })


  # Create time series plot of stock price using plot_ly, with the y-axis based on the y-variable the user selects
  output$stock_time <- renderPlotly({
    ts_plot <- plot_ly(data = stock_ts_range(), x = ~date, y = ~ get(input$yvar1), mode = "lines")

    # Create different titles and axis labels if the y-variable is percentage change in stock price
    if (input$yvar1 != "changePercent") {
      ts_plot %>%
        layout(
          title = paste(
            str_to_title(input$yvar1), " Prices for ",
            input$symbol %>%
              str_extract_all("(?<=\\().+?(?=\\))") %>%
              unlist()
          ),
          xaxis = list(title = "Date"),
          yaxis = list(title = paste(str_to_title(input$yvar1), " Price (in Dollars)"))
        )
    } else {
      ts_plot %>%
        layout(
          title = paste(
            "Percentage Change in Prices for ",
            input$symbol %>%
              str_extract_all("(?<=\\().+?(?=\\))") %>%
              unlist()
          ),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Percentage Change of Prices")
        )
    }
  })


  # Create second company seletion list if checkbox is ticked, if not do not return anything
  comp2Select <- reactive({
    if (input$comp2 == TRUE) {
      selectInput("symbol2", "Choose a second company:",
        choices = comp$company,
        selected = "Amazon.com Inc. (AMZN)"
      )
    } else if (input$comp2 == FALSE) {
      return()
    }
  })

  # Show select input in sidebar panel of UI
  output$second_select <- renderUI({
    comp2Select()
  })


  # Use the Company Profile API for the second company
  profile2_url <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}",
      symbol = input$symbol2 %>%
        str_extract_all("(?<=\\().+?(?=\\))") %>%
        unlist()
    )
  })
  r_profile2 <- reactive({
    GET(profile2_url())
  })
  json_profile2 <- reactive({
    content(r_profile2(), as = "text")
  })

  # Grab the image link from the profile for the second company
  image2 <- reactive({
    fromJSON(json_profile2())$profile$image %>% unlist()
  })

  # Output second company image in UI if checkbox is ticked, if not do not return anything
  output$img2 <- renderUI({
    if (input$comp2 == TRUE) {
      tags$img(src = image2())
    } else {
      return()
    }
  })


  # Use the Historical Price API for the second company
  prices_url2 <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/historical-price-full/{symbol}",
      symbol = input$symbol2 %>%
        str_extract_all("(?<=\\().+?(?=\\))") %>%
        unlist()
    )
  })
  r_prices2 <- reactive({
    GET(prices_url2())
  })
  json_prices2 <- reactive({
    content(r_prices2(), as = "text")
  })

  # Obtain the historical prices data table for the second inputted company stock
  stock_series <- reactive({
    fromJSON(json_prices2())$historical
  })

  # Transform all dates in data table from characters to dates
  stock_series_2 <- reactive({
    data <- stock_series()
    data$date <- as.Date(data$date, "%Y-%m-%d")
    data
  })

  # Filter the dates column from the data table based on the same date range input from user
  stock_ts_range2 <- reactive({
    stock_series_2() %>%
      filter(date >= as.Date(input$dates[1]) & date <= as.Date(input$dates[2]))
  })

  # Create 2nd time series plot using plot_ly
  output$stock_time2 <- renderPlotly({

    # Plot only appears if checkbox is ticked
    if (input$comp2 == TRUE) {
      ts_plot <- plot_ly(data = stock_ts_range2(), x = ~date, y = ~ get(input$yvar1), mode = "lines")

      # Create different titles and axis labels if the y-variable is percentage change in stock price
      if (input$yvar1 != "changePercent") {
        ts_plot %>%
          layout(
            title = paste(
              str_to_title(input$yvar1), " Prices for ",
              input$symbol2 %>%
                str_extract_all("(?<=\\().+?(?=\\))") %>%
                unlist()
            ),
            xaxis = list(title = "Date"),
            yaxis = list(title = paste(str_to_title(input$yvar1), " Price (in Dollars)"))
          )
      } else {
        ts_plot %>%
          layout(
            title = paste(
              "Percentage Change in Prices for ",
              input$symbol2 %>%
                str_extract_all("(?<=\\().+?(?=\\))") %>%
                unlist()
            ),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Percentage Change of Prices")
          )
      }
    } else {
      return()
    }
  })


  # Output news headlines as a data table with url links embedded within titles, so that user can click them
  output$headlines <- DT::renderDataTable({
    DT::datatable(news[, "links", drop = FALSE],
      escape = FALSE,
      options = list(dom = "t", bPaginate = FALSE, bSort = FALSE), colnames = ""
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
