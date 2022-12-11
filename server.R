library("tidyverse")
library("lubridate")
library("formattable")
library("DT")

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
listing <- createListing(listingData) %>% arrange(desc(date))
years <- unique(listingData$year) %>% sort(decreasing = TRUE)
categs <- unique(listingData$category) %>% append(const_ALL) %>% sort

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
  
  output$data_listing <- renderDT({
    dl <- listing
    if (input$select_category_for_listing != const_ALL) {
      dl <- dl %>% filter(category==input$select_category_for_listing)
    }
    dl %>% datatable()
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
  
}