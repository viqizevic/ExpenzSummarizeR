# Create Summary

createSummary <- function(datain, yr) {
  tb0 <- datain %>% filter(year == yr)
  # Get number of months (for average calculation)
  countmonths <- tb0 %>% count(month)
  nmonths <- length(countmonths$month)
  # Create total category
  tbcatsum1 <- tb0 %>% mutate(category = "Total")
  tbcatsum2 <- rbind(tb0, tbcatsum1)
  # Get frequency counts
  frq <- tbcatsum2 %>% count(category, month) %>%
    group_by(category) %>% summarise(n = n())
  # Create total for the whole year
  tbyrtot <- tbcatsum2 %>% mutate(year_month = paste0(year,"-99"), month = "Total")
  tball1 <- rbind(tbcatsum2, tbyrtot)
  # Get the summary and transpose by months
  tball2 <- tball1 %>% group_by(year_month, month, category) %>% 
    summarise(sum = sum(value)) %>%
    ungroup() %>% 
    select(-year_month) %>% 
    pivot_wider(names_from = month, values_from = sum, values_fill = 0) %>% 
    mutate(Average = round(Total/nmonths, digits = 2)) %>% 
    arrange(category)
  # Order by freq and amount
  tball2$freq <- frq$n
  maxavg <- max(tball2$Average)
  tball3 <- tball2 %>%
    mutate(order = ifelse(category != "Total", sign(freq-nmonths)*2*maxavg + Average, -100*maxavg)) %>% 
    arrange(-order) %>% 
    rename_with(~ gsub("category", yr, .x, fixed = TRUE)) %>% 
    select(-freq, -order)
  # Display formatted table
  alignments <- c("l", rep(c("r"),times=nmonths+2))
  formattable(tball3, align=alignments, list(
    'Total' = improvement_formatter,
    'Average' = improvement_formatter
  ))
}

createSummary(datain = tb, yr = "2020")
