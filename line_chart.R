
getSaldoSummary <- function(datain, yr) {
  tb0 <- datain %>% filter(year == yr)
  # Update category
  tbcat0 <- tb0 %>% mutate(category = "Saldo")
  
  # Get the summary and transpose by months
  tballsum <- tbcat0 %>% group_by(year_month, month, category) %>% 
    summarise(sum = sum(value)) %>% ungroup()
  
  ds <- get_monthly_saldo(tballsum, datain, yr) %>% rename(Saldo=sum) %>% select(-category)
  ds$Month <- as.numeric(gsub(paste0(yr,"-"),"",ds$year_month,fixed=TRUE))
  ds
}


create_saldo_line_chart <- function(datain, year) {
  ds <- getSaldoSummary(datain, year)
  ggplot(ds, aes(Month, Saldo)) + geom_line()
}


create_saldo_line_chart(tb,"2022")

