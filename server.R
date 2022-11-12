library("tidyverse")
library("lubridate")
library("formattable")
library("DT")

source("listing.R")
source("summary.R")
source("pie_chart.R")
source("bar_chart.R")

ds0 <- read_csv("cache/listing.csv")
listingData <- ds0 %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date)
  )
years <- unique(listingData$year) %>% sort(decreasing = TRUE)

function(input, output, session) {
  
  updateSelectInput(session, "select_year_for_pie_chart",
                    choices = years)
  updateSelectInput(session, "select_year_for_bar_chart",
                    choices = years)
  
  output$data_listing <- renderDT({
    createListing(listingData) %>% datatable()
    #createSummary(listingData,year(today())) %>% datatable()
  })
  
  output$pie <- renderPlot({
    create_pie_chart(ds0,input$select_year_for_pie_chart)
  })

  output$bar <- renderPlot({
    create_bar_chart(ds0,input$select_year_for_bar_chart)
  })
  
}