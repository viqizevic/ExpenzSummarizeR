library(tidyverse)
library(readxl)
library(janitor)
library(formattable)

# Set a few color variables to get more visually appealing
customGreen = "#71CA97"
customRed = "#ff7f7f"

# Format improvement
improvement_formatter <- formatter("span", style = x ~ style(
  color = ifelse(x > 0, customGreen, 
                 ifelse(x < 0, customRed, "black")))
)

categories <- read_csv("categories.csv")

# Reader function
readTransactionsFile <- function(folder="files", filename, acc,
                                 dateformat="%d.%m.%Y") {
  file <- paste(folder, filename, sep="/") 
  read_excel(file) %>% as_tibble %>% clean_names %>%
    left_join(categories, by = "category") %>% 
    mutate(
      category = ifelse(is.na(preferred), category, preferred),
      dtmn = strptime(date,format=dateformat),
      value = income+expense,
      date = dtmn
    ) %>% 
    select(date, payee, memo, value, category) %>% 
    mutate(
      account = acc,
      year_month = format(date, '%Y-%m'),
      year = format(date, '%Y'),
      month = format(date, '%B'),
    )
}

# Read data
tb_barclay <- readTransactionsFile(filename="Umsaetze_BC.xlsx", acc="Barclays")
tb_n26 <- readTransactionsFile(filename="N26-transactions.xlsx", acc="N26", dateformat="%Y-%m-%d")
tb_lbbamzn <- readTransactionsFile(filename="KKB-Umsaetze.xlsx", acc="LBB Amazon")
tb_ingdiba <- readTransactionsFile(filename="Umsatzanzeige_ING.xlsx", acc="ING DiBa")
tb_commrzb <- readTransactionsFile(filename="Umsaetze_CMZB.xlsx", acc="Commerzbank")
tb_trfwise <- readTransactionsFile(filename="Statement_Wise.xlsx", acc="Wise", dateformat="%d-%m-%Y")
tb_deutscb <- readTransactionsFile(filename="Kontoumsaetze_DB.xlsx", acc="Deutsche Bank")

# Combine as one tibble
tb <- rbind(tb_barclay, tb_n26, tb_lbbamzn, tb_ingdiba, tb_commrzb, tb_trfwise, tb_deutscb)

ctg <- tb %>% count(category)
# tb %>% count(year, account)
