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

#Get symbols list for all companies using the Symbols List API
res <- httr::GET(url = 'https://financialmodelingprep.com/api/v3/company/stock/list', httr::add_headers(.headers=headers), query = params)

symbols_list <- content(res, as = "text")
comp_symbol <- fromJSON(symbols_list)$symbol
comp <- na.omit(comp_symbol) %>% 
    mutate(company = str_c(name, " ", "(", symbol, ")")) %>% 
    arrange(desc(price)) %>% 
    filter(exchange != "Paris" & exchange != "YHD" & exchange != "Brussels" & exchange != "Amsterdam" & exchange != "Toronto") 
'%ni%' <- Negate('%in%')
comp <- comp[1:500,]
comp <- comp %>% 
  filter(symbol %ni% c("BSPAX", "MSG", "SHW", "SAM", "BSPIX", "DJCO", "ROLA", "UNH", "VIIIX", "VINIX", "SFLA", "RTLA", "AMT", "MFLA", "CLX", "HD", "ACN", "EL", "AON", "GS", "BTIIX", "BTIEX", "DNB", "CCI", "LCIAX", "DODGX", "RYVLX", "RYVYX", "VTCLX", "VHCAX", "BTTRX", "PG", "EDU", "SJM", "TTMIX", "PRMTX", "BTTTX", "PXUS", "VPMAX", "ACTEX", "VPMCX", "GBIL", "EVFTC", "GRBIC", "BVNSC", "EVGBC", "EVLMC", "FOANC", "MOGLC", "RPIBC", "EVSTC", "IVFGC", "CRUSC", "HFGIC", "OKDCC", "IVENC", "IVFVC", "TBCIX", "TRBCX"))
#Scrape all news headlines with their url links from financial modeling prep
html <- read_html("https://financialmodelingprep.com/")
news_links <- html %>% 
  html_nodes("div.articles") %>% 
  html_nodes("a") %>% 
  html_attr("href") 
for (i in 1:10) {
  news_links[i] <- paste0("https://financialmodelingprep.com", news_links[i])
}
news_titles <- html %>% 
  html_nodes("h4.article-title") %>% 
  html_text()
news <- tibble(urls = news_links[1:10], titles = news_titles[1:10])
news$links <- paste0("<a href='", news$urls, "' target='_blank'>", news$titles, "</a>")    


# Define UI for application 
ui <- fluidPage(theme = shinytheme("flatly"),
  
  useShinydashboard(),

  titlePanel("Stock Market Trends"),
  
  sidebarLayout(
    
    sidebarPanel( 
      
      #Allow user to select company stock of choice from dropdown list
      selectInput("symbol", "Choose a company:",
                  choices = comp$company,
                  selected = "Apple Inc. (AAPL)"
      ),
      
      uiOutput("textbox_ui2"),
      
      checkboxInput("comp2", "Add a second company for comparison", FALSE),
      
      dateRangeInput("dates", 
                     "Select date range:",
                     start = as.Date("2015-03-09", '%Y-%m-%d'), 
                     end = as.Date(as.character(Sys.Date()), '%Y-%m-%d')
      ),
      
      selectInput("yvar1", "Choose graph Y-axis variable:",
                  choices = c("Opening Price" = "open",
                              "High Price" = "high",
                              "Low Price" = "low",
                              "Closing Price" = "close",
                              "Percentage Change" = "changePercent")
      )
    ),  
      
    mainPanel(
      
      #Suppress warning messages in shiny webpage
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
                 
      tabsetPanel(
        
        tabPanel(
          
          "Selected Companies",
          
          box(
            title = "Company 1 Logo", status = "info", solidHeader = TRUE,
            width = 5, align = 'center',
            uiOutput("img")
          ),
          
          conditionalPanel(
            condition = "input.comp2 == true",
            box(
              title = "Company 2 Logo", status = "warning", solidHeader = TRUE,
              width = 5, align = 'center',
              uiOutput("img2")
            )
          )
        ),
        
        tabPanel(
        
          "Time Series Plots",
          
          box(title = "Company 1 Plot", width = 12, height = "400px", status = "primary",
              solidHeader = TRUE, 
              plotlyOutput("stock_time", width = "500px", height = "300px")
          ),
          
          conditionalPanel(
            condition = "input.comp2 == true",
            box(title = "Company 2 Plot", width = 12, height = "400px", status = "success",
                solidHeader = TRUE, 
                plotlyOutput("stock_time2", width = "500px", height = "300px")
            )
          )
        ),
          
        tabPanel(
          
          "Top Market Headlines",
          
          DT::dataTableOutput("headlines")
        )
      )  
    )
  )
)    


server <- function(input, output) {
   
  #Use the Company Profile API by first getting the url link, using the company selected by the user
  profile_url <- reactive({
      str_glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}", 
               symbol = input$symbol%>% 
                   str_extract_all("(?<=\\().+?(?=\\))") %>% #Extract the symbol by using regex
                   unlist())
      
  })
  r_profile <- reactive({GET(profile_url())}) 
  json_profile <- reactive({content(r_profile(), as = "text")})
  #Grab the image link from the profile
  company_image <- reactive({fromJSON(json_profile())$profile$image %>% unlist()})
  
  #Use image link to output company logo in UI
  output$img <- renderUI({
      tags$img(src = company_image())
  })
  
  
  #Use the Historical Price API by first getting the url link, using the company selected by the user
  prices_url <- reactive({
      str_glue("https://financialmodelingprep.com/api/v3/historical-price-full/{symbol}", 
               symbol = input$symbol %>% 
                   str_extract_all("(?<=\\().+?(?=\\))") %>% 
                   unlist())
      
  })
  r_prices <- reactive({GET(prices_url())})
  json_prices <- reactive({content(r_prices(), as = "text")})
  #Obtain the historical prices data table for the inputted company stock
  stock_ts <- reactive({fromJSON(json_prices())$historical})
  
  #Tranform all dates in data table from characters to dates
  stock_ts2 <- reactive({
      data <- stock_ts()
      data$date <- as.Date(data$date, '%Y-%m-%d') 
      data
  })
  
  #Filter the dates column from the data table based on the date range input from user
  stock_ts_range <- reactive({
      stock_ts2() %>% 
          filter(date >= as.Date(input$dates[1]) & date <= as.Date(input$dates[2]))
  })
  
  
  #Create time series plot using plot_ly
  output$stock_time <- renderPlotly({
    ts_plot <- plot_ly(data = stock_ts_range(), x =~date, y=~get(input$yvar1), mode = "lines")
    if (input$yvar1 != "changePercent") {
      ts_plot %>% 
        layout(title = paste(str_to_title(input$yvar1), " Prices for ",
                           input$symbol%>% 
                             str_extract_all("(?<=\\().+?(?=\\))") %>% 
                             unlist()),
        xaxis = list(title = "Date"),
        yaxis = list(title = paste(str_to_title(input$yvar1), " Price (in Dollars)")))
    } else {
      ts_plot %>% 
        layout(title = paste("Percentage Change in Prices for ",
                             input$symbol%>% 
                               str_extract_all("(?<=\\().+?(?=\\))") %>% 
                               unlist()),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Percentage Change of Prices"))
    }
  })
  
  
  textboxToggle2 <- reactive({
    
    if (input$comp2 == TRUE) {
      selectInput("symbol3", "Choose a second company:",
                  choices = comp$company,
                  selected = "Amazon.com Inc. (AMZN)")
    } else if (input$comp2 == FALSE) {
      return()
    }
    
  })
  
  output$textbox_ui2 <- renderUI({ textboxToggle2() })
  
  url32 <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/company/profile/{symbol}", 
             symbol = input$symbol3%>% 
               str_extract_all("(?<=\\().+?(?=\\))") %>% 
               unlist())
    
  })
  
  r32 <- reactive({GET(url32())})
  json42 <- reactive({content(r32(), as = "text")})
  image2 <- reactive({fromJSON(json42())$profile$image %>% unlist()})

  
  output$img2 <- renderUI({
    if (input$comp2 == TRUE) {
      tags$img(src = image2())
    } else {
      return()
    }
  })
  
  prices_url2 <- reactive({
    str_glue("https://financialmodelingprep.com/api/v3/historical-price-full/{symbol}", 
             symbol = input$symbol3 %>% 
               str_extract_all("(?<=\\().+?(?=\\))") %>% 
               unlist())
    
  })
  r_prices2 <- reactive({GET(prices_url2())})
  json_prices2 <- reactive({content(r_prices2(), as = "text")})
  stock_ts22 <- reactive({fromJSON(json_prices2())$historical})
  
  stock_ts222 <- reactive({
    data <- stock_ts22()
    data$date <- as.Date(data$date, '%Y-%m-%d')
    data
  })
  
  stock_ts_range2 <- reactive({
    stock_ts222() %>% 
      filter(date >= as.Date(input$dates[1]) & date <= as.Date(input$dates[2]))
  })
  
  
  output$stock_time2 <- renderPlotly({
    if (input$comp2 == TRUE) {
      ts_plot <- plot_ly(data = stock_ts_range2(), x =~date, y=~get(input$yvar1), mode = "lines")
      if (input$yvar1 != "changePercent") {
        ts_plot %>% 
          layout(title = paste(str_to_title(input$yvar1), " Prices for ",
                               input$symbol3%>% 
                                 str_extract_all("(?<=\\().+?(?=\\))") %>% 
                                 unlist()),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = paste(str_to_title(input$yvar1), " Price (in Dollars)")))
      } else {
        ts_plot %>% 
          layout(title = paste("Percentage Change in Prices for ",
                               input$symbol3%>% 
                                 str_extract_all("(?<=\\().+?(?=\\))") %>% 
                                 unlist()),
                 xaxis = list(title = "Date"),
                 yaxis = list(title = "Percentage Change of Prices"))
      }
    } else {
      return()
    }
  })
  
  output$headlines <- DT::renderDataTable({
    DT::datatable(news[, "links", drop = FALSE], escape = FALSE, 
                  options = list(dom='t',bPaginate=FALSE, bSort=FALSE), colnames = '')
  })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
