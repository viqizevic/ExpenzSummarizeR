library("DT")

navbarPage(
  "Transactions",
  tabPanel(
    "Listing",
    fluidPage(
      DTOutput("data_listing")
    )
  ),
  tabPanel(
    "Pie Chart",
    fluidPage(
      plotOutput("pie")
    )
  ),
  tabPanel(
    "Bar Chart",
    fluidPage(
      plotOutput("bar")
    )
  ),
  collapsible = TRUE
)