
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


get_flatten_data <- function(ds, mode="MONTHLY") {
  df <- rbind(ds, ds %>% mutate(Month=Month+0.95))
  df %>% arrange(Month)
}


create_saldo_line_chart <- function(datain, year) {
  ds <- getSaldoSummary(datain, year)
  df <- get_flatten_data(ds)
  ggplot(df, aes(Month, Saldo)) + geom_line() +
    scale_x_continuous(name = "Month", breaks = 1:12, limits = c(1,13))
}


create_saldo_line_chart(tb,"2022")

