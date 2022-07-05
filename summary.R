# Create Summary

# Sub-function to get balance at the end of the year
get_end_balance <- function(datain, yr, mnth=12) {
  stopifnot(all(grepl("\\d{4}", datain$year)))
  stopifnot(!is.na(datain$value))
  stopifnot(1 <= mnth & mnth <= 12)
  firstday <- as_date(paste(yr, mnth, 1, sep = "-"))
  lastday <- update(firstday, mday=days_in_month(firstday))
  datain %>% filter(date <= lastday) %>% .$value %>% sum
}

# Main function to create the summary
createSummary <- function(datain, yr) {
  stopifnot(grepl("\\d{4}", yr))
  
  tb0 <- datain %>% filter(year == yr)
  # Get number of months (for average calculation)
  countmonths <- tb0 %>% count(month)
  nmonths <- length(countmonths$month)

  # Create total category
  tbcatsum1 <- tb0 %>% mutate(category = "Total")
  tbsaldo0 <- tbcatsum1 %>% mutate(category = "Saldo")
  tbcatsum2 <- rbind(tb0, tbcatsum1, tbsaldo0)
  
  # Get frequency counts
  frq <- tbcatsum2 %>% count(category, month) %>%
    group_by(category) %>% summarise(n = n())
  
  # Create total for the whole year
  tbyrtot <- tbcatsum2 %>% mutate(year_month = paste0(year,"-99"), month = "Total")
  tball1 <- rbind(tbcatsum2, tbyrtot)
  
  # Get the summary and transpose by months
  tball1sum <- tball1 %>% group_by(year_month, month, category) %>% 
    summarise(sum = sum(value)) %>% ungroup()
  
  # Get saldo from total
  tbsaldo <- tball1sum %>% filter(category == "Saldo")
  # Get monthly saldo
  saldos <- tbsaldo %>% .$sum %>% cumsum
  # Add the end of year before saldo
  tbsaldo$sum <- saldos + get_end_balance(datain, as.numeric(yr)-1)
  tball1saldo <- rbind(tball1sum %>% filter(category != "Saldo"), tbsaldo) %>% 
    mutate(sumc = if_else(sum != 0, sprintf("%.2f",sum), ""))
  
  tball2 <- tball1saldo %>% select(-year_month, -sum) %>% 
    pivot_wider(names_from = month, values_from = sumc, values_fill = "") %>% 
    mutate(
      Total = if_else(Total == "", "0.00", Total),
      Average = round(as.numeric(Total)/nmonths, digits = 2)
    ) %>% 
    arrange(category)
  # Order by Freq and amount
  tball2$Freq <- frq$n
  maxavg <- max(abs(tball2$Average))
  tball3 <- tball2 %>%
    mutate(
      order = ifelse(Average < 0, -maxavg-(Freq*Average), Average),
      order = ifelse(Average < 0 & 9 <= Freq, (-maxavg-Average)/10, order),
      order = ifelse(category %in% c("Total","Saldo"), -100*maxavg-Average, order),
      Total = ifelse(category == "Saldo", "", Total),
      Average = ifelse(category == "Saldo", "", sprintf("%.2f",Average))
    ) %>% 
    arrange(-order) %>% 
    rename_with(~ gsub("category", yr, .x, fixed = TRUE)) %>% 
    select(-order)
  # Display formatted table
  alignments <- c("l", rep(c("r"),times=nmonths+2))
  formattable(tball3, align=alignments, list(
    'Total' = improvement_formatter,
    'Average' = improvement_formatter
  ))
}

createSummary(datain = tb, yr = "2022")
