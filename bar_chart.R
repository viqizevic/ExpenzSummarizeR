library(ggplot2)
library(ggcharts)


create_bar_chart <- function(datain, year) {
  df <- get_data_for_chart(datain, year, threshold = 0.1)
  df %>% bar_chart(x = Category, y = AVAL, top_n = 12) +
    labs(y = "Expense (€)")
}

create_bar_chart(tb,"2022")
