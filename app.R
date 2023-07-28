# Required Libraries
library(shiny)
library(tidyverse)
library(DT)

# Define UI 
ui <- fluidPage(
  titlePanel("Calculate Daily Wage"),
  sidebarLayout(
    sidebarPanel(
      dateInput('date', 'Select Date', format = "yyyy-mm-dd"),
      numericInput('wage', 'Enter your hourly wage', value = 0),
      textInput('start_time', 'Enter Start Time (hh:mm)', value = "08:30"),
      selectInput('start_time_ampm', 'Select AM/PM', choices = c('AM', 'PM')),
      textInput('end_time', 'Enter End Time (hh:mm)', value = "05:00"),
      selectInput('end_time_ampm', 'Select AM/PM', choices = c('AM', 'PM')),
      actionButton("add", "Add to list"),
      downloadButton("downloadData", "Download")
    ),
    mainPanel(
      DT::dataTableOutput("table")
    )
  )
)

# Define Server 
server <- function(input, output, session) {
  df <- reactiveVal(data.frame(Date = as.Date(character()),
                               Wage = numeric(),
                               Hours = numeric(),
                               stringsAsFactors = FALSE))
  
  observeEvent(input$add, {
    start_time <- strptime(paste(input$start_time, input$start_time_ampm), "%I:%M %p")
    end_time <- strptime(paste(input$end_time, input$end_time_ampm), "%I:%M %p")
    hours_worked <- as.numeric(difftime(end_time, start_time, units = "hours"))
    df(rbind(df(), data.frame(Date = as.Date(input$date), 
                              Wage = input$wage, 
                              Hours = round(hours_worked, digits=2))))
  })
  
  
  output$table <- DT::renderDataTable({
    data <- df()
    data %>%
      mutate(Pay = round((Wage * Hours), digits=2))
  }, 
  rownames = TRUE, 
  selection = 'single')
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
