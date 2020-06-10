source("./auxiliary_files/auxiliary.R")

ui <- fluidPage(
    theme = shinytheme("sandstone"),
    titlePanel("Killer sudoku Helper"),
    navlistPanel(
        tabPanel("What is killer Sudoku"),
        tabPanel("Unique combinations",
                 fluidRow(
                     bscols(widths = c(3,9),
                     box(
                         
                         filter_checkbox(id ="ncombinations",
                                       label = "number of combinations",
                                       sharedData = data,
                                       ~combinations)
                         # selectInput(inputId = "ncombinations",
                         #             label = "Number of combinations",
                         #             choices = c(1,2,3),
                         #             selected = 1, multiple = F)
                         ),
                     box(reactableOutput("ucombinations"))
                 ))),
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
    
    output$ucombinations <- renderReactable({
        aux <- data
        # %>% filter(combinations == input$ncombinations) %>%
            # select(totValue, '1':'9')
        reactable(aux,
                  defaultColDef = 
                      colDef(name = NULL,
                             align = "center"),
                  borderless = TRUE,
                  outlined = TRUE,
                  compact = TRUE,
                  fullWidth = FALSE,
                  showPagination = FALSE,
                  filterable = F,
                  groupBy = "totValue")
        })
    
    output$combinations_summary <- renderReactable({
       reactable(combinations_summary %>%
            select("Number of Cells" = cells,
                   "Value of Sum" = totValue,
                   "Number of Combinations" = combinations))
        })

    output$combinations <- renderReactable({
        aux <- combinations[paste(input$totValue, input$cells, sep = "_")][[1]]
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
