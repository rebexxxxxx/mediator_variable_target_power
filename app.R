# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Mediator Variable Target Power Analysis"),
  fluidRow(
    column(4, "",
           selectInput("modelInput", "Model Type",
                       choices = list("2 Parallel Mediators"="two_parallel_mediators",
                                      "3 Parallel Mediators"="three_parallel_mediators",
                                      "4 Parallel Mediators"="four_parallel_mediators")),
           numericInput("targetpowerInput", "Target Power", 0.80, min = 0.1, max = 0.99),
           numericInput("NlowInput", "Smallest Sample", 50, min = 1, max = 10000),
           numericInput("NhighInput", "Largest Sample", 100, min = 100, max = 100000),
           numericInput("NstepsInput", "Step Width Between Low and High Sample Size", 5, min = 5, max = 10000),
           numericInput("replicationInput", "Number of Replications", 10, min = 10, max = 100000),
           numericInput("mcdrawInput", "Monte Carlo Draws per Replication", 10, min = 10, max = 100000),
           numericInput("seedInput", "Seed", 123, min = 1, max = 100000),
           numericInput("ciInput", "Confidence Level, %", 95, min = 1, max = 100)),
    fluidRow(
      column(8, "Coefficient OR Correlation Input",
             selectInput("input_method", "Input Method", 
                         choices = list("Correlations"="corr",
                                        "Standardized Coefficients"="sc")),
             uiOutput("input_options")
      )),
    
    fluidRow(
      column(4, "Power Analysis Results",
             actionButton(inputId = "action", label = "Calculate Power",
                          width = "100%", class = "btn-success"),
             tableOutput("power"))
    )
  )
)



server <- function(input, output) {
  
  # Execute model-specific power analysis code
  calc_power <- eventReactive(input$action, {
    out <- tryCatch(
      {source(paste0("./", input$modelInput, ".R"), local = TRUE)$value},
      error = function(e) {return(e$message)}
    )
  })
  
  # Display output or display error messages
  observeEvent(calc_power(),
               if (class(calc_power()) != "data.frame") {
                 output$power <- renderText({
                   paste0("<div class=\"alert alert-dismissible alert-danger\">",
                          calc_power(), "</div>")})
               } else {
                 output$power <- renderTable({ 
                   calc_power()
                 }, include.rownames=FALSE)
               }
  )
  
  
  
  # Render Input Options UI    
  observeEvent(input$input_method, {
    if (input$input_method == "corr") {
      output$input_options <- renderUI({
        source(paste0("./", input$modelInput, "_correlations_ui.R"), local = TRUE)$value  
      })
    }
    if (input$input_method == "sc") {
      output$input_options <- renderUI({
        source(paste0("./", input$modelInput, "_sc_ui.R"), local = TRUE)$value  
      })
    }
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
