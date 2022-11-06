library("tidyverse")
library("lubridate")
library("formattable")
library("DT")

source("listing.R")
source("summary.R")


listingData <- read_csv("cache/listing.csv") %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date)
  )

function(input, output, session) {
  
  output$data_table <- renderDT({
    createListing(listingData) %>% datatable()
    #createSummary(listingData,year(today())) %>% datatable()
  })
  
}