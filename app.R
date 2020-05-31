source("./auxiliary_files/auxiliary.R")

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("Killer sudoku Helper"),
    navlistPanel(
        tabPanel("What is killer Sudoku"),
        tabPanel("Unique combinations"),
        tabPanel("1 Group helper",
            fluidRow(
                    box(width = 6,
                        numericInput(inputId = "totValue", width = "50%",
                                     label = "Value of the Sum", value = 1)), 
                    box(width = 6, 
                        numericInput(inputId = "cells", width = "50%",
                                     label = "number of cells", value = 1))),
            fluidRow(
                    box(width = 2,
                        selectInput(inputId = "except",
                                     label = "Exclude combinations with:",
                                     choices = c(1:9), selected = NA,
                                    multiple = TRUE)),
                    box(width = 10,
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
        reactable(aux,
                  rowStyle = function(index){
                      if(!any(aux[index,] %in% input$except)){
                          list(background = "#2FAD28"
                                   # "rgba(0, 10, 0, 0)"
                               )
                      } else {
                          list(background = "#C90C0C")
                      }
                  },
                  defaultColDef = 
                      colDef(name = NULL,
                             align = "center"),
                  borderless = TRUE,
                  outlined = TRUE,
                  compact = TRUE,
                  fullWidth = FALSE,
                  showPagination = FALSE,
                  filterable = F)}
        )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
