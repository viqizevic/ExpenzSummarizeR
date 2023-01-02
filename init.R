rm(list = ls())

library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(janitor)
library(formattable)
library(lubridate)

# Set a few color variables to get more visually appealing
customGreen = "#71CA97"
customRed = "#ff7f7f"

# Format improvement
improvement_formatter <- formatter("span", style = x ~ style(
  color = ifelse(x > 0, customGreen, 
                 ifelse(x < 0, customRed, "black")))
)

categories <- read_csv("categories.csv", show_col_types = FALSE)

# Check function for valid income and expense values
checkIncomeAndExpense <- function(datain) {
  ds <- datain %>% mutate(
    navalue = (is.na(income) | is.na(expense)),  
    inexpvalue = (income != 0 & expense != 0),
    wrongincome = (income < 0),
    wrongexpense = (expense > 0)
  )
  if(any(ds$navalue)) warning("Unexpected missing income or expense.", immediate. = TRUE)
  if(any(ds$inexpvalue)) warning("Unexpected both income and expense value given.", immediate. = TRUE)
  if(any(ds$wrongincome)) warning("Unexpected income < 0.", immediate. = TRUE)
  if(any(ds$wrongexpense)) warning("Unexpected expense > 0.", immediate. = TRUE)
  ds %>% filter(navalue | inexpvalue | wrongincome | wrongexpense)
}

# Reader function
readTransactionsFile <- function(folder="data", filename, acc,
                                 dateformat="%d.%m.%Y") {
  file <- paste(folder, filename, sep="/") 
  exl <- read_excel(file) %>% as_tibble %>% clean_names %>% 
    left_join(categories, by = "category") %>% 
    mutate(
      category = ifelse(is.na(preferred), category, preferred),
      dtmn = strptime(date,format=dateformat),
      date = dtmn
    )
  # Add IBAN if RINP transfer of Deutsche Bank
  if(acc=="Deutsche Bank") {
    exl <- exl %>% mutate(
      payee = ifelse(is.na(payee), umsatzart, payee),
      memo = ifelse(grepl("Tanzil",payee,ignore.case=TRUE) & (grepl("RINP|Tabungan",memo) | memo=="-"),
                    paste(memo,iban), memo)
    )
  }
  if(acc=="ING DiBa") {
    exl <- exl %>% mutate(
      payee = ifelse(is.na(payee), buchungstext, payee)
    )
  }
  if(any(is.na(exl$date))) warning("Failed to get dates. Please check format.", immediate. = TRUE)
  checkIncomeAndExpense(exl)
  exl %>% select(date, payee, memo, income, expense, category) %>% 
    mutate(
      account = acc,
      value = income+expense,
      year_month = format(date, '%Y-%m'),
      year = format(date, '%Y'),
      month = format(date, '%B'),
    )
}

# Read data
tb_barclay <- readTransactionsFile(filename="Umsaetze_BC.xlsx", acc="Barclays")
tb_n26     <- readTransactionsFile(filename="N26-transactions.xlsx", acc="N26", dateformat="%Y-%m-%d")
tb_lbbamzn <- readTransactionsFile(filename="KKB-Umsaetze.xlsx", acc="LBB Amazon")
tb_ingdiba <- readTransactionsFile(filename="Umsatzanzeige_ING.xlsx", acc="ING DiBa")
tb_commrzb <- readTransactionsFile(filename="Umsaetze_CMZB.xlsx", acc="Commerzbank")
tb_trfwise <- readTransactionsFile(filename="Statement_Wise.xlsx", acc="Wise", dateformat="%d-%m-%Y")
tb_deutscb <- readTransactionsFile(filename="Kontoumsaetze_DB.xlsx", acc="Deutsche Bank")
tb_wstnrot <- readTransactionsFile(filename="Umsaetze_WSTR.xlsx", acc="Wüstenrot")
tb_mintos  <- readTransactionsFile(filename="Mintos-transactions.xlsx", acc="Mintos", dateformat="%Y-%m-%d")

# Combine as one tibble
tb0 <- rbind(tb_barclay, tb_n26, tb_lbbamzn, tb_ingdiba, tb_commrzb, 
            tb_trfwise, tb_deutscb, tb_wstnrot, tb_mintos)

sc_file <- paste("data", "set_categories.xlsx", sep="/") 
sc_exl <- read_excel(sc_file) %>% as_tibble %>% clean_names

# Get suggested categories
suggcats0 <- tb0 %>% full_join(sc_exl, by=character()) %>% 
  select(-"category", -"year", -"year_month", -"month", -"income", -"expense") %>% 
  mutate(
    detect_payee = str_detect(payee, payee_pattern),
    no_payee_pattern = is.na(payee_pattern),
    detect_memo = str_detect(memo, memo_pattern),
    no_memo_pattern = is.na(memo_pattern),
    detect_exp_flag = (exp_flag=="Y" & value<0) | (exp_flag=="N" & value>0),
    no_expense_flag = is.na(exp_flag),
    detect_account = str_detect(account, account_pattern),
    no_account_pattern = is.na(account_pattern)
  ) %>% filter(
    detect_payee | no_payee_pattern,
    detect_memo | no_memo_pattern,
    detect_exp_flag | no_expense_flag,
    detect_account | no_account_pattern
  ) %>% 
  left_join(categories, by=c("suggested_category"="category")) %>% 
  mutate(
    suggested_category = ifelse(is.na(preferred), suggested_category, preferred),
  )
suggcats <- suggcats0 %>% select(date, payee, memo, account, value, suggested_category)
tb1 <- tb0 %>% left_join(suggcats)
tb <- tb1 %>% mutate(
  category=ifelse(!is.na(category),category, suggested_category)
)

# Check if any duplicates created
duplsgcats <- suggcats0 %>% dplyr::count(date, payee, memo, account, value) %>% filter(n!=1)
if(nrow(duplsgcats) > 0) {
  warning("Duplicates found. Please check.", immediate. = TRUE)
  duplsgcats %>% formattable()
}


misscats <- tb %>% filter(is.na(category),is.na(suggested_category))
if(nrow(misscats) > 0) {
  warning("Found blank category. Please check.", immediate. = TRUE) 
}
misscats %>% select(payee, memo, account, value, category, suggested_category) %>% formattable()


# Save cache data for Shiny
tb %>% write_csv("cache/listing.csv")


.check <- function() {
  
  ctg <- tb %>% count(category)
  ctg %>% arrange(n) %>% formattable
  
  tb %>% count(year, account) %>% formattable
  
  # Display where suggested category not same as available one
  tb %>% filter(!is.na(suggested_category),category!=suggested_category) %>% 
    count(payee, memo, account, value, category, suggested_category) %>% formattable()
  
  tb %>% count(payee, memo, account, category, suggested_category) %>% 
    filter(is.na(suggested_category), n>1) %>% formattable()
  
  tb %>% filter(grepl("Ihre",payee)) %>% formattable()
  tb %>% filter(grepl("Sonstiges",memo),account=="LBB Amazon") %>% formattable()
  tb %>% filter(grepl("Reise",category)) %>% formattable()
  tb %>% filter(grepl("VIYU",memo)) %>% formattable()
  
  # Print used suggested categories
  tb1 %>% filter(is.na(category)) %>% count(payee, memo, suggested_category) %>% formattable()
}

