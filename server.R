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
categs <- unique(ds0$category) %>% append(const_ALL) %>% sort
years <- unique(ds0$year) %>% sort(decreasing = TRUE)
accounts <- unique(ds0$account) %>% append(const_ALL) %>% sort
listingData <- ds0 %>% 
  arrange(date) %>% 
  mutate(
    date = as.Date(date),
    category = factor(category, levels=categs),
    year = factor(year, levels=years),
    account = factor(account, levels=accounts)
  )

filtered_listing <- function(listing, categ, acc, yr) {
  dl0 <- listing
  if(yr != const_ALL) {
    dl0 <- dl0 %>% filter(year==as.numeric(yr))
  }
  dl <- createListing(dl0) %>% arrange(desc(date))
  if (categ != const_ALL) {
    dl <- dl %>% filter(category==categ)
  }
  if (acc != const_ALL) {
    dl <- dl %>% filter(account==acc)
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
                 list_accounts <- accounts
                 if (input$select_category_for_listing != const_ALL) {
                   dt0 <- listingData %>% 
                     filter(category==input$select_category_for_listing)
                   list_years <- dt0 %>% 
                     pull(year) %>% unique %>% as.character %>% sort(decreasing = TRUE)
                   list_accounts <- dt0 %>% 
                     pull(account) %>% unique %>% as.character %>% sort
                 }
                 list_years <- c(const_ALL, list_years)
                 list_accounts <- c(const_ALL, list_accounts)
                 updateSelectInput(session, "select_year_for_listing",
                                   choices = list_years)
                 updateSelectInput(session, "select_account_for_listing",
                                   choices = list_accounts)
               })
  
  output$data_listing <- renderDT({
    filtered_listing(listingData,
                     input$select_category_for_listing,
                     input$select_account_for_listing,
                     input$select_year_for_listing) %>% 
      datatable(filter = "top")
  })
  
  output$total <- renderText({
    tot <- filtered_listing(listingData,
                     input$select_category_for_listing,
                     input$select_account_for_listing,
                     input$select_year_for_listing) %>% 
      pull(value) %>% sum
    paste("Total",
          paste0("(category:", input$select_category_for_listing,
                 ", year:", input$select_year_for_listing,
                 ", account:", input$select_account_for_listing,
                 "):"),
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
      datatable(
        filter = "top",
        options = list(
          pageLength = 25
        ))
  })
}