library("DT")

navbarPage(
  "Transactions",
  tabPanel(
    "Listing",
    fluidPage(
      fluidRow(
        column(
          6,
          selectInput("select_category_for_listing",
                      label = "Choose category:",
                      choices = NULL)
        ),
        column(
          6,
          selectInput("select_year_for_listing",
                      label = "Choose year:",
                      choices = NULL)
        )
      ),
      DTOutput("data_listing")
    )
  ),
  tabPanel(
    "Pie Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput("select_year_for_pie_chart",
                    label = "Choose year:",
                    choices = NULL)
      ),
      mainPanel(
        plotOutput("pie")
      )
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
  tabPanel(
    "Summary Bar Chart",
    fluidPage(
      selectInput("select_year_for_summary_bar_chart",
                  label = "Choose year:",
                  choices = NULL),
      plotOutput("summary_bar")
    )
  ),
  tabPanel(
    "Saldo Line Chart",
    fluidPage(
      selectInput("select_year_for_saldo_line_chart",
                  label = "Choose year:",
                  choices = NULL),
      plotOutput("saldo_line")
    )
  ),
  collapsible = TRUE
)