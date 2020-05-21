#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


ups <- expand.grid(c(3:45), c(2:9))
out <- list()
for(ii in 1:nrow(ups)){
    out[[ii]] <- combi(ups[ii,1], ups[ii,2])
}
out



combi <- function(value, cells){
    x    <- combn(c(1:9), cells)
    x2 <- colSums(x)
    possible_combinations <- t(x[,which(x2 == value)])
    return(possible_combinations)
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("totalsum",
                        "Number to decompose:",
                      value  =1),
            numericInput("partitions",
                      "Number of cells",
                      value =1)
            ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("combinations")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$combinations <- renderTable({
        combi(input$totalsum, input$partitions)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
