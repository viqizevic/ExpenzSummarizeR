library("tidyverse")
library("DT")

source("listing.R")


listingData <- read_csv("cache/listing.csv") %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date)
  )

function(input, output, session) {
  
  output$data_table <- renderDT({
    createListing(listingData) %>% datatable()
  })
  
}