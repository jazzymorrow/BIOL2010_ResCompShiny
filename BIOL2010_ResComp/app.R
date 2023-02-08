#
# Resource Competition Shiny App for BIOL2010
#
# Find out more about building applications with Shiny here:
#
#    
#
#devtools::install_github("andrewletten/rescomp")
library(shiny)
library(rescomp)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Modelling Resource Competition"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("mu",
                        "Maximum growth rate:",
                        min = 0.01,
                        max = 0.3,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(title = "One Consumer, One Resource",
            plotOutput("rescomp")
            ),
            tabPanel(title = "Two Consumers, One Resource",
           plotOutput("distPlot")
        )
    )
)))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 20 + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    

    output$rescomp <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 1, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(input$mu),
        resspeed = 0.03,
        resconc = 2,
        totaltime = 300
      )
      m1 <- sim_rescomp(pars)
      plot_rescomp(m1)
    })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
