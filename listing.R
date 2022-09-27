# Create Listing

createListing <- function(datain, yr="all") {
  ds <- datain
  if (yr != "all") ds <- filter(ds, year == yr)
  ds %>% mutate(
    payee = if_else(is.na(payee), "", payee),
    memo = if_else(is.na(memo), "", memo)
  ) %>% arrange(category, date, payee) %>% 
    select(category, date, payee, account, value, memo)
}

printListing <- function(datain, yr="all") {
  createListing(datain, yr) %>% 
    formattable(list('value' = improvement_formatter))
}

printListing(datain = tb, yr="2022")


# Save cache data for Shiny
createListing(tb) %>% write_csv("cache/listing.csv")
