library("DT")
library("scales")

source("init.R")
source("listing.R")
source("summary.R")
source("pie_chart.R")
source("bar_chart.R")
source("summary_bar_chart.R")
source("line_chart.R")

const_ALL <- "-All-"

ds0 <- read_csv("cache/listing.csv")
listingData <- ds0 %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date)
  )
years <- unique(listingData$year) %>% sort(decreasing = TRUE)
categs <- unique(listingData$category) %>% append(const_ALL) %>% sort

filtered_listing <- function(listing, categ, yr) {
  dl0 <- listing
  if(yr != const_ALL) {
    dl0 <- dl0 %>% filter(year==as.numeric(yr))
  }
  dl <- createListing(dl0) %>% arrange(desc(date))
  if (categ != const_ALL) {
    dl <- dl %>% filter(category==categ)
  }
  dl
}

function(input, output, session) {
  
  updateSelectInput(session, "select_category_for_listing",
                    choices = categs)
  updateSelectInput(session, "select_year_for_pie_chart",
                    choices = years)
  updateSelectInput(session, "select_year_for_bar_chart",
                    choices = years)
  updateSelectInput(session, "select_year_for_summary_bar_chart",
                    choices = years)
  updateSelectInput(session, "select_year_for_saldo_line_chart",
                    choices = years)
  updateSelectInput(session, "select_column_for_freq",
                    choices = names(listingData))
  
  observeEvent(c(input$select_category_for_listing),
               {
                 list_years <- years
                 if (input$select_category_for_listing != const_ALL) {
                   list_years <- listingData %>% 
                     filter(category==input$select_category_for_listing) %>% 
                     pull(year) %>% unique %>% sort(decreasing = TRUE)
                 }
                 list_years <- c(const_ALL, list_years)
                 updateSelectInput(session, "select_year_for_listing",
                                   choices = list_years)
               })
  
  output$data_listing <- renderDT({
    filtered_listing(listingData,
                     input$select_category_for_listing,
                     input$select_year_for_listing) %>% 
      datatable()
  })
  
  output$total <- renderText({
    tot <- filtered_listing(listingData,
                     input$select_category_for_listing,
                     input$select_year_for_listing) %>% 
      pull(value) %>% sum
    paste("Total",
          paste0("(category:", input$select_category_for_listing,
                 ", year:", input$select_year_for_listing, "):"),
          scales::dollar(tot, prefix = "€"))
  })
  
  output$pie <- renderPlot({
    create_pie_chart(ds0,input$select_year_for_pie_chart)
  })

  output$bar <- renderPlot({
    create_bar_chart(ds0,input$select_year_for_bar_chart)
  })
  
  output$summary_bar <- renderPlot({
    create_summary_bar_chart(ds0,input$select_year_for_summary_bar_chart)
  })
  
  output$saldo_line <- renderPlot({
    create_saldo_line_chart(ds0,input$select_year_for_saldo_line_chart)
  })
  
  output$selected_freq_columns <- renderDT({
    if(is.null(input$select_column_for_freq)) {
      return(listingData %>% datatable())
    }
    plyr::count(listingData, input$select_column_for_freq) %>% 
      datatable(options = list(
        pageLength = 25
      ))
  })
}