source("./auxiliary_files/auxiliary.R")

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("Killer sudoku Helper"),
    navlistPanel(
        tabPanel("What is killer Sudoku"),
        tabPanel("Unique combinations"),
        tabPanel("1 Group helper",
            fluidRow(
                    box(width = 4 ,
                        numericInput(inputId = "totValue",
                                     label = NULL, value = 1)), 
                    box(width = 4, 
                        numericInput(inputId = "cells",
                                     label = NULL, value = 1)),
                    box(width = 4,
                        reactableOutput("combinations"))
                )
            ),
        tabPanel("2 Groups Helper"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$combinations_summary <- renderReactable({
       reactable(combinations_summary %>%
            select("Number of Cells" = cells,
                   "Value of Sum" = totValue,
                   "Number of Combinations" = combinations))
        })

    output$combinations <- renderReactable({
        aux <- combi(input$totValue, input$cells)
        reactable(aux)}
        )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
