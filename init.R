library(tidyverse)
library(readxl)
library(janitor)
library(formattable)

# Set a few color variables to get more visually appealing
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

# Format improvement
improvement_formatter <- formatter("span", style = x ~ style(
  color = ifelse(x > 0, customGreen, 
                 ifelse(x < 0, customRed, "black")))
)

# Reader function
readTransactionsFile <- function(folder="files", filename, acc,
                                 dateformat="%Y-%m-%d") {
  file <- paste(folder, filename, sep="/") 
  read_excel(file) %>% as_tibble %>% clean_names %>%
    mutate(
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
datef_de <- "%d.%m.%Y"
tb_barclay <- readTransactionsFile(filename="Umsaetze_BC.xlsx", acc="Barclays", dateformat=datef_de)
tb_n26 <- readTransactionsFile(filename="N26-transactions.xlsx", acc="N26")
tb_lbbamzn <- readTransactionsFile(filename="KKB-Umsaetze.xlsx", acc="LBB Amazon", dateformat=datef_de)

# Combine as one tibble
tb <- rbind(tb_barclay, tb_n26, tb_lbbamzn)
