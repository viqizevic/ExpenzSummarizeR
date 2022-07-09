rm(list = ls())
setwd("~/Documents/rstudio/ExpenzSummarizeR")

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
readTransactionsFile <- function(folder="files", filename, acc,
                                 dateformat="%d.%m.%Y") {
  file <- paste(folder, filename, sep="/") 
  exl <- read_excel(file) %>% as_tibble %>% clean_names %>% 
    left_join(categories, by = "category") %>% 
    mutate(
      category = ifelse(is.na(preferred), category, preferred),
      dtmn = strptime(date,format=dateformat),
      date = dtmn
    )
  if(any(is.na(exl$date))) warning("Failed to get dates. Please check format.", immediate. = TRUE)
  if(any(is.na(exl$category)|exl$category %in% c("","0"))) warning("Found blank category. Please check.", immediate. = TRUE)
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
tb <- rbind(tb_barclay, tb_n26, tb_lbbamzn, tb_ingdiba, tb_commrzb, 
            tb_trfwise, tb_deutscb, tb_wstnrot, tb_mintos)

ctg <- tb %>% count(category)
# ctg %>% arrange(n) %>% formattable
# tb %>% count(year, account) %>% formattable
