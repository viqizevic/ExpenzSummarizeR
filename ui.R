library("DT")

navbarPage(
  "Transactions",
  tabPanel(
    "Listing",
    fluidPage(
      selectInput("select_category_for_listing",
                  label = "Choose category:",
                  choices = NULL),
      DTOutput("data_listing")
    )
  ),
  tabPanel(
    "Pie Chart",
    fluidPage(
      selectInput("select_year_for_pie_chart",
                  label = "Choose year:",
                  choices = NULL),
      plotOutput("pie")
    )
  ),
  tabPanel(
    "Bar Chart",
    fluidPage(
      selectInput("select_year_for_bar_chart",
                  label = "Choose year:",
                  choices = NULL),
      plotOutput("bar")
    )
  ),
  collapsible = TRUE
)