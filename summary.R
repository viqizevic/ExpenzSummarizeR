# Create Summary

# Sub-function to get balance at the end of the year
get_end_balance <- function(datain, yr, mnth = 12, acc = "") {
  if (acc != "") {
    datain <- datain %>% filter(account == acc)
  }
  stopifnot(all(grepl("\\d{4}", datain$year)))
  stopifnot(!is.na(datain$value))
  stopifnot(1 <= mnth & mnth <= 12)
  firstday <- as_date(paste(yr, mnth, 1, sep = "-"))
  lastday <- update(firstday, mday=days_in_month(firstday))
  datain %>% filter(date <= lastday) %>% .$value %>% sum
}

# Sub-function to get saldo at each month
get_monthly_saldo <- function(datasum, datain, yr) {
  dssaldo <- datasum %>%
    filter(str_starts(category,"Saldo"), month!="Total")
  accs <- dssaldo %>% count(category) %>% select(-n)
  ym <- dssaldo %>% count(year_month, month) %>% select(-n)
  # cross join to display all months and accounts
  ymaccs <- ym %>% cross_join(accs) %>%
    left_join(dssaldo, by = c("year_month", "month", "category")) %>%
    mutate(sum=if_else(is.na(sum),0,sum))
  for (acc in accs$category) {
    account <- gsub("Saldo\\s*","",acc)
    dssaldo1 <- ymaccs %>% filter(category==acc)
    saldos <- dssaldo1 %>% .$sum %>% cumsum
    dssaldo1$csum <- saldos + get_end_balance(datain, as.numeric(yr)-1, 12, account)
    ymaccs <- ymaccs %>%
      left_join(dssaldo1, by = c("year_month", "month", "category", "sum")) %>%
      mutate(sum=if_else(!is.na(csum),csum,sum)) %>% select(-csum)
  }
  ymaccs
}

# Main function to create the summary
create_summary <- function(datain, yr) {
  stopifnot(grepl("\\d{4}", yr))

  # Check for blank category, filter out blank one
  blankcat <- datain %>% filter(is.na(category))
  if(nrow(blankcat)>0) warning("Found obs with blank category, will be ignored.", immediate. = TRUE)
  datain <- datain %>% filter(!is.na(category))

  # Get fix categories
  fix_categs <- fix_categories %>% pull(category)

  # Filter by year given
  tb0 <- datain %>% filter(year == yr)
  # Get number of months (for average calculation)
  countmonths <- tb0 %>% count(month)
  nmonths <- length(countmonths$month)

  # Create total category
  tbcat_total <- tb0 %>% mutate(category = "Total")
  # Create total income and outcome category
  tbcatsum1 <- tb0 %>% filter(!grepl("Transfer ", category)) %>%
    mutate(category = ifelse(value>0, "Total Income", "Total Outcome"))
  # Create total income and outcome fix category
  tbcatsum1f <- tb0 %>% filter(category %in% fix_categs) %>%
    mutate(category = paste("Total", ifelse(value>0,"Income","Outcome"), "Fix (F)"))
  # Create saldo category
  tbsaldo0 <- tbcat_total %>% mutate(category = "Saldo")
  # Create saldo per account category
  tbsaldo1 <- tbsaldo0 %>% mutate(category = paste(category,account))
  # Append all total and saldo
  tbcatsum2 <- rbind(tb0, tbcat_total, tbcatsum1, tbcatsum1f, tbsaldo0, tbsaldo1)

  # Get frequency counts
  frq <- tbcatsum2 %>% count(category, month) %>%
    group_by(category) %>% summarise(n = n())

  # Create total for the whole year
  tbyrtot <- tbcatsum2 %>% mutate(year_month = paste0(year,"-99"), month = "Total")
  # Append total in year
  tball1 <- rbind(tbcatsum2, tbyrtot)

  # Get the summary and transpose by months
  tball1sum <- tball1 %>% group_by(year_month, month, category) %>%
    summarise(sum = sum(value)) %>% ungroup()

  # Get monthly saldo
  tbsaldo <- get_monthly_saldo(tball1sum, datain, yr)

  tball1saldo <- rbind(tball1sum %>% filter(!str_starts(category,"Saldo")), tbsaldo) %>%
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
      Freq = ifelse(category %in% fix_categs, 12, Freq),
      order = ifelse(Average < 0, -maxavg-Average, Average),
      order = ifelse(Average < 0 & 10 <= Freq, (-maxavg-Average)/10, order),
      order = ifelse(grepl("Total",category), -100*maxavg+Average, order),
      order = ifelse(grepl("Total ",category), -150*maxavg+Average, order),
      order = ifelse(grepl("Saldo",category), -200*maxavg+abs(Average), order),
      Total = ifelse(grepl("Saldo",category), "", Total),
      Average = ifelse(grepl("Saldo",category), "", sprintf("%.2f",Average)),
      category = gsub("^(Total|Saldo) ",">> \\1 ", category),
      category = ifelse(category %in% fix_categs, paste(category, "(F)"), category)
    ) %>%
    arrange(-order) %>%
    rename(Category = category) %>%
    select(-order)
}

print_summary <- function(datain, yr) {
  tball <- create_summary(datain, yr) %>%
    rename_with(~ gsub("Category", yr, .x, fixed = TRUE))
  # Display formatted table
  alignments <- c("l", rep(c("r"),times=ncol(tball)-2))
  formattable(tball, align=alignments, list(
    'Total' = improvement_formatter,
    'Average' = improvement_formatter
  ))
}

.tryout <- function() {
  print_summary(datain = tb, yr = "2023")
}
