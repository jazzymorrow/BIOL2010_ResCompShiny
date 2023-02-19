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
library(ggplot2)

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
               sidebarPanel(width = 3,
                 sliderInput("mu",
                             "Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("resconc",
                             "Resource concentration:",
                             min = 0.1,
                             max = 1,
                             value = 0.5)),
               # Specify rescomp graph to use 
               mainPanel(
                 plotOutput("OneConOneRes", width = "90%"))
             )),
    # PANEL 2: 2 CONS 1 RES
    tabPanel(title = "Two Consumers, One Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu21", #mu for panel 2, species 1
                             "Sp1 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.05),
                 sliderInput("mu22",
                             "Sp2 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("resconc2",
                             "Resource concentration:",
                             min = 0.1,
                             max = 1,
                             value = 0.5)),
             mainPanel(
               plotOutput("TwoConOneRes"))
        )),
    # PANEL 3: 2 CONS 1 PULSED RES
    tabPanel(title = "Two Consumers, One Pulsed Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu31", #mu for panel 3, species 1
                             "Sp1 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("mu32",
                             "Sp2 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.1),
                 sliderInput("pulsefreq",
                             "Pulse frequency:",
                             min = 2,
                             max = 100,
                             value = 50)),
               # Specify rescomp graph to use 
               mainPanel(
                 plotOutput("TwoConOnePulRes"))
        )),
    # PANEL 4: 2 CONS 2 RES
    tabPanel(title = "Two Consumers, Two Resources", 
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu41A",
                             "Sp1 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.05),
                 sliderInput("mu41B",
                             "Sp1 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.05),
                 sliderInput("mu42A",
                             "Sp2 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("mu42B",
                             "Sp2 Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.05),
                 sliderInput("resconc4",
                             "Resource concentrations:",
                             min = 0.2,
                             max = 2,
                             value = 0.2),
                 # sliderInput("resconcB",
                 #             "Resource B concentration:",
                 #             min = 0.2,
                 #             max = 2,
                 #             value = 0.2)
                 ),
             mainPanel(h4("Functional response"),
               fluidRow(column(12,align = "center",
                        plotOutput("FuncResp", 
                                      width = "80%", height = "300px"))),
               h4("Plotting through time"),
               fluidRow(align = "center",
                 column(12,plotOutput("TwoConTwoRes")))
             ))
  )))

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
      plot_rescomp(m1) + theme(text = element_text(size=22),
                               axis.title = element_text(size = 18),
                               axis.text = element_text(size = 15),
                               legend.text = element_text(size = 18),
                               legend.position = "bottom")
    })
    
    ## Two consumers, one resource 
    output$TwoConOneRes <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu21,input$mu22), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),  
        resspeed = 0.03, 
        resconc = input$resconc2,
        totaltime = 500)
      
      m1 <- sim_rescomp(pars)
      
      plot_rescomp(m1) + theme(text = element_text(size=22),
                               axis.title = element_text(size = 18),
                               axis.text = element_text(size = 15),
                               legend.text = element_text(size = 18),
                               legend.position = "bottom")
    })
    
    ## Two consumers, one pulsed resource 
    output$TwoConOnePulRes <- renderPlot({
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu31,input$mu32), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),
        kmatrix = matrix(c(2, 0.015), 
                         nrow = 2, 
                         ncol = 1, 
                         byrow = TRUE),  
        resspeed = 0, # set to zero for no additional resource supply 
        resconc = 0.2,
        respulse = 0.3,
        pulsefreq = input$pulsefreq # resource pulse size
      )
      m1 <- sim_rescomp(pars)
      plot_rescomp(m1) + theme(text = element_text(size=22),
                               axis.title = element_text(size = 18),
                               axis.text = element_text(size = 15),
                               legend.text = element_text(size = 18),
                               legend.position = "bottom")
    })
    
    ## Two consumers, two resources
    output$FuncResp <- renderPlot({
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 2,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu41A,input$mu41B,
                            input$mu42A, input$mu42B), 
                          nrow = 2, 
                          ncol = 2,
                          byrow = TRUE),
        resspeed = 0.03,
        resconc = input$resconc4, #both resources have same concentration??
        mort = 0.03,
        essential = FALSE,
        totaltime = 500)
      plot_funcresp(pars, maxx = 0.3) + 
        theme(text = element_text(size = 22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })
    
    output$TwoConTwoRes <- renderPlot({
    pars <- spec_rescomp(
      spnum = 2, 
      resnum = 2,
      funcresp = "type2",
      mumatrix = matrix(c(input$mu41A,input$mu41B,
                          input$mu42A, input$mu42B), 
                        nrow = 2, 
                        ncol = 2,
                        byrow = TRUE),
      resspeed = 0.03,
      resconc = input$resconc4, #both resources have same concentration??
      mort = 0.03,
      essential = FALSE,
      totaltime = 500)
    
    m1 <- sim_rescomp(pars)
    
    plot_rescomp(m1) + 
      theme(text = element_text(size = 22),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 18),
            legend.position = "bottom")
    })
     
}

# Run the application 
shinyApp(ui = ui, server = server)
