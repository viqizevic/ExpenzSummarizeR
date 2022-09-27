library("DT")

fluidPage(
  h1("Transactions"),
  #wellPanel("Overall data"),
  DTOutput("data_table")
)