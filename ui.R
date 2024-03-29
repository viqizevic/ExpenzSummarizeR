library("DT")

navbarPage(
  "Transactions",
  tabPanel(
    "Listing",
    fluidPage(
      fluidRow(
        column(
          4,
          selectInput("select_category_for_listing",
                      label = "Choose category:",
                      choices = NULL)
        ),
        column(
          2,
          selectInput("select_year_for_listing",
                      label = "Choose year:",
                      choices = NULL)
        ),
        column(
          3,
          selectInput("select_account_for_listing",
                      label = "Choose account:",
                      choices = NULL)
        ),
        column(
          3,
          h5(textOutput("total", container = span))
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
  tabPanel(
    "Freq",
    fluidPage(
      selectInput("select_column_for_freq",
                  label = "Choose columns:",
                  choices = NULL,
                  multiple = TRUE),
      DTOutput("selected_freq_columns")
    )
  ),
  collapsible = TRUE
)