library("tidyverse")
library("DT")


listing <- read_csv("cache/listing.csv") %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date)
  )

function(input, output, session) {
  
  output$data_table <- renderDT({
    listing %>% datatable()
  })
  
}