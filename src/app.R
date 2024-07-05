# Exercise Sheet 5
# Author: github.com/fdf-uni

# For this GitHub version I removed some comments here which aren't too
# important for the below code and rather just answers to exercises
# from the exercise sheet. You can ignore numbers followed by a closing
# bracket in comments below as they also are only there to indicate which
# exercise the corresponding code belongs to.

library(shiny)
library(ggplot2)
# 3) More themes
library(bslib)
# 3) Also update ggplot themes
library(thematic)
# 3) Better table output capabilities
library(DT)

# Generate correlated data given correlation and number of observations
generate_correlated_data <- function(correlation, n) {
  alpha <- correlation
  beta <- sqrt(1 - correlation^2)

  x1 <- runif(n)
  x2 <- runif(n)

  x_correlated <- alpha * x1 + beta * x2

  # 3) Also order data frame to make it look nicer in the table output
  return(data.frame(x1, x2 = x_correlated)[order(x1), ])
}

# 3) Custom theme
custom_theme <- bs_theme(
  version = 5,
  preset = "sketchy"
)

# 3) Make plot(s) respect custom theme
thematic_shiny()

# 1a) Adjust title to include my name
title_str <- "Correlation Game by fdf"

# Define UI
ui <- fluidPage(
  title = title_str,
  theme = custom_theme,
  # 3) Add theme selector and switch for dark mode
  fluidRow(
    # 1a) Adjust title to include name
    column(8, titlePanel(title_str)),
    column(
      3,
      shinyWidgets::pickerInput(
        "themeSelection",
        "Theme: ",
        choices = c(builtin_themes(), bootswatch_themes()),
        selected = "sketchy",
        width = "fit"
      )
    ),
    column(
      1,
      align = "right",
      input_dark_mode(id = "dark_mode", mode = "light")
    ),
  ),
  # 1b) Add input field and display for the player's name
  fluidRow(
    column(4, textInput("nameInput", "What's your name?")),
    column(
      6, textOutput("nameOutput"),
      tags$head(tags$style("#nameOutput{font-size: 20px; font-style: bold;}"))
    ),
    # 3) Button to generate new data
    column(
      2,
      align = "right",
      actionButton("newDataButton", "New Data", icon = icon("dice-five"))
    )
  ),
  # Create Panel with different tabsets
  tabsetPanel(
    # Tabset for scatter plot
    tabPanel(
      "Scatter Plot",
      plotOutput("scatterPlot"),
      # 1c) Input for player's correlation guess
      sliderInput(
        "guess", "Try to guess the correlation!",
        min = -1, max = 1, value = 0, step = 0.01,
        width = "100%"
      )
    ),

    # Tabset for solution
    tabPanel(
      "Solution",
      sidebarLayout(
        mainPanel(
          h5("Correlation"),
          verbatimTextOutput("correlationOutput"),
          h6("You were this far off: "),
          verbatimTextOutput("guessOutputValue"),
          fluidRow(
            column(
              6,
              align = "left",
              textOutput("guessOutputCongrats"),
              tags$head(tags$style(
                "#guessOutputCongrats{font-size: 20px; font-style: bold;}"
              ))
            ),
            column(6, align = "right", uiOutput("twitterShareButton"))
          )
        ),
        sidebarPanel(
          h5("Interpretation"),
          htmlOutput("interpretation")
        ),
      ),
    ),

    # 2) Tabset for Sandbox
    tabPanel(
      "Sandbox",
      sliderInput(
        "correlationRange", "Range of correlation",
        min = -1, max = 1, value = 0, step = 0.01,
        width = "100%"
      ),
      numericInput(
        "numberOfObservations", "Number of observations", 100,
        min = 1, step = 1
      )
    ),

    # 3) Tabset for data
    tabPanel(
      "Data",
      sidebarLayout(
        mainPanel(
          h5("Currently plotted data"),
          p("Double click entries to edit them."),
          dataTableOutput("dataOutput"),
        ),
        sidebarPanel(
          h5("Enter your own data"),
          textAreaInput(
            "dataInput",
            "Please provide your data as a comma-separated table with each row
            on a new line and no column names.
            Only two columns are supported.",
            rows = 10
          ),
          fluidRow(
            column(
              6,
              align = "left",
              actionButton("dataPasteButton", "Insert current data")
            ),
            column(
              6,
              align = "right",
              actionButton("dataInputButton", "Submit")
            ),
          ),
          htmlOutput("dataInputResult"),
          htmlOutput("dataInputError")
        ),
      )
    ),
    # 3) Tabset for app information
    tabPanel(
      "About",
      p("This app was created as a solution to the fifth graded exercise sheet
        in the Advanced Statistical Programming course at LMU Munich."),
      p("All credit for the general idea as well as some parts of the code goes
        to the corresponding lecture team."),
      p(
        "If you are interested in how this app works, you can check out it's
        source code on",
        a(
          "GitHub",
          href = "https://github.com/fdf-uni/graded5",
          target = "_blank",
          rel = "noopener noreferrer"
        ),
        "."
      ),
    )
  )
)

server <- function(input, output, session) {
  # 2) Store df as reactive value such that outputs update when it changes
  values <- reactiveValues(
    # Initialize it as a data frame with 100 rows and correlation which is
    # chosen randomly according to uniform distribution on (-1, 1)
    df = generate_correlated_data(correlation = runif(1, -1, 1), n = 100)
  )

  # 1b) Store and display player's name
  name <- reactive({
    input$nameInput
  })
  output$nameOutput <- renderText({
    paste0("Currently playing: ", name())
  })

  # Create scatter plot
  output$scatterPlot <- renderPlot({
    ggplot(values$df, aes(x = x1, y = x2)) +
      geom_point() +
      labs(x = "x1", y = "x2")
  })

  # Reactive helper variables
  correlation <- reactive(cor(values$df$x1, values$df$x2))
  difference <- reactive(abs(correlation() - input$guess))

  # Print the current correlation in the "Solution" tabset.
  output$correlationOutput <- renderPrint(correlation())

  # 1d) Add output for distance of guess to actual correlation
  output$guessOutputValue <- renderPrint(difference())
  # Conditionally add congratulation text
  output$guessOutputCongrats <- renderText({
    if (difference() < 0.1) "Great Guess!"
  })

  # 1e) Provide interpretation of correlation coefficient
  output$interpretation <- renderText({
    paste0(
      "<b>Direction:</b> ",
      if (correlation() >= 0) "positive" else "negative",
      "<br/><b>Magnitude:</b> ",
      if (abs(correlation()) <= 0.3) {
        "weak"
      } else if (abs(correlation()) <= 0.7) {
        "moderate"
      } else {
        "strong"
      }
    )
  })

  # 3) Function to output currently plotted data (I need to use it multiple
  # times since the column headers get a bit messed up when using replaceData
  # with completely new dataframes).
  update_data_table <- function() {
    output$dataOutput <- renderDataTable({
      datatable(
        # Isolate df here so that the table doesn't have to be regenerated
        # with every edit in the table output. Also add id column.
        isolate(cbind(id = seq_len(nrow(values$df)), values$df)),
        editable = list(
          target = "cell", disable = list(columns = 0)
        ),
        rownames = FALSE,
        options = list(
          scrollY = "400px",
          order = list(list(0, "asc")),
          pageLength = 100,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
          )
        )
      )
    })
  }
  update_data_table()

  # Update data when user edits cells
  observeEvent(input$dataOutput_cell_edit, {
    info <- input$dataOutput_cell_edit
    values$df[info$row, info$col] <- info$value
    replaceData(
      dataTableProxy("dataOutput"),
      isolate(cbind(id = seq_len(nrow(values$df)), values$df)),
      rownames = FALSE
    )
  })
  # React to submission button with some (basic) error handling
  observeEvent(
    input$dataInputButton,
    {
      tryCatch(
        {
          table <- read.table(
            text = input$dataInput, sep = ",",
            # Ensure correct naming and type of columns
            col.names = c("x1", "x2"), colClasses = rep("numeric", 2)
          )
          values$df <- as.data.frame(table)[order(table$x1), ]
          update_data_table()
          # Show success message
          output$dataInputResult <- renderText("Successfully imported data!")
          output$dataInputError <- renderText("")
        },
        error = function(e) {
          output$dataInputResult <- renderText({
            paste(
              "Please ensure that your data are formatted correctly,
            i.e. as described above.<br/>",
              "R error:<br/>"
            )
          })
          output$dataInputError <- renderText({
            paste("<font color=\"#FF0000\"><b>", e, "</b></font>")
          })
        }
      )
    }
  )
  # Paste current data to data input field when pressing corresponding button
  observeEvent(input$dataPasteButton, {
    updateTextAreaInput(
      session, "dataInput",
      value = paste(
        do.call(paste, c(values$df[order(values$df$x1), ], sep = ",")),
        collapse = "\n"
      )
    )
  })

  # 2) Update scatter plot and solution fields on changes to variables in
  # sandbox tabset. Note that since df was turned into a reactive variable this
  # can be done in a very short manner as only df needs to be updated.
  observeEvent(input$correlationRange | input$numberOfObservations,
    # Ignore default values when first starting the app
    ignoreInit = TRUE,
    {
      # Check that number of observations is reasonable
      if (input$numberOfObservations >= 1) {
        values$df <- generate_correlated_data(
          correlation = input$correlationRange,
          n = input$numberOfObservations
        )
      }
      # 3) Also update the data view
      update_data_table()
    }
  )

  # 3) Update theme on selection
  observeEvent(input$themeSelection, {
    session$setCurrentTheme(
      bs_theme_update(custom_theme, preset = input$themeSelection)
    )
  })

  # 3) Share results on twitter when pressing the corresponding button
  # Do all this dynamically such that a new guess creates a new link
  output$twitterShareButton <- renderUI({
    shiny::a(
      h4(icon("x-twitter"),
        "Share your result",
        class = "btn btn-default action-button",
        style = "fontweight:600"
      ),
      target = "_blank",
      rel = "noopener noreferrer",
      href = paste0(
        "https://x.com/intent/tweet?",
        "url=https%3A%2F%2Ffdf-uni.github.io%2Fgraded5%2F",
        "&text=I+just+was+only+off+by+approximately+",
        round(difference(), digits = 3),
        "+when+guessing+a+correlation+of+approximately+",
        round(correlation(), digits = 3),
        "+in+a+scatter+plot%21",
        "+How+good+are+you%3F%0A"
      )
    )
  })

  # 3) Generate new random data whenever the corresponding button is pressed
  observeEvent(input$newDataButton, {
    values$df <- generate_correlated_data(
      correlation = runif(1, -1, 1), n = 100
    )
    update_data_table()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
