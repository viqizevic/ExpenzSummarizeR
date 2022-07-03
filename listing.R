# Create Listing

createListing <- function(datain, yr="all") {
  ds <- datain
  if (yr != "all") ds <- filter(ds, year == yr)
  ds %>% mutate(
    payee = if_else(is.na(payee), "", payee),
    memo = if_else(is.na(memo), "", memo)
  ) %>% arrange(category, date, payee) %>% 
    select(category, date, payee, account, value, memo) %>% 
    formattable(list('value' = improvement_formatter))
}

createListing(datain = tb, yr="2021")
