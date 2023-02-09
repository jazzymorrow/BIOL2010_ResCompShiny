#
# Resource Competition Shiny App for BIOL2010
# Created: 8/02/2023
# Created by: Jasmine Fowler-Morrow
#
#    
#
#devtools::install_github("andrewletten/rescomp")
library(shiny)
library(rescomp)

#--------------------------------------------------
          # Define UI for application 
#--------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Modelling Resource Competition"),
  
  tabsetPanel(
    
    # PANEL 1: 1 CONS 1 RES
    tabPanel(title = "One Consumer, One Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mu",
                             "Maximum growth rate:",
                             min = 0.01,
                             max = 2,
                             value = 0.2),
                 sliderInput("resconc",
                             "Resource concentration:",
                             min = 0.2,
                             max = 2,
                             value = 0.5)),
               # Specify rescomp graph to use 
               mainPanel(
                 plotOutput("OneConOneRes"))
             )),
    # PANEL 2: 2 CONS 1 RES
    tabPanel(title = "Two Consumers, One Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mu1",
                             "Sp1 Maximum growth rate:",
                             min = 0.01,
                             max = 2,
                             value = 0.05),
                 sliderInput("mu2",
                             "Sp2 Maximum growth rate:",
                             min = 0.01,
                             max = 2,
                             value = 0.2),
                 sliderInput("resconc",
                             "Resource concentration:",
                             min = 0.2,
                             max = 2,
                             value = 0.2)),
             mainPanel(
               plotOutput("TwoConOneRes"))
    ))
  )
)

#---------------------------------------------------
                # Define server logic 
#---------------------------------------------------
server <- function(input, output) {
  
    ## One consumer, one resource 
    output$OneConOneRes <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 1, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(input$mu),
        resspeed = 0.03,
        resconc = input$resconc,
        totaltime = 300)
      m1 <- sim_rescomp(pars)
      plot_rescomp(m1)
    })
    
    ## Two consumers, one resource 
    output$TwoConOneRes <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu1,input$mu2), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),  
        resspeed = 0.03, 
        resconc = input$resconc,
        totaltime = 500)
      m1 <- sim_rescomp(pars)
      plot_rescomp(m1)
    })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
