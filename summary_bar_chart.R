
getIncomeExpenseSummary <- function(datain, yr) {
  tb0 <- datain %>% filter(year == yr) %>% 
    filter(!grepl("Transfer .*(Konto|Kreditkarte)",category))
  
  # Update category
  tbcat0 <- tb0 %>% 
    mutate(
      category = if_else(value>0, "Income", "Expense"),
      value = abs(value)
    )
  
  db <- tbcat0 %>% group_by(year_month, month, category) %>% 
    summarise(Sum = sum(value)) %>% ungroup()
  db$Category <- factor(db$category, levels = sort(unique(db$category),decreasing=TRUE))
  db$Month <- factor(db$month, levels = unique(db$month))
  db
}


create_summary_bar_chart <- function(datain, year) {
  db <- getIncomeExpenseSummary(datain, year)
  bar_chart(db, Month, Sum, fill = Category, horizontal = FALSE, sort = FALSE)
}


create_summary_bar_chart(tb,"2021")

