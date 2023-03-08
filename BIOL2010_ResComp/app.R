# Initial ----

# Resource Competition Shiny App for BIOL2010
# Created: 8/02/2023
# Created by: Jasmine Fowler-Morrow

# Use the navigation bar to navigate this script easily :)

#devtools::install_github("andrewletten/rescomp")
library(shiny)
library(rescomp)
library(ggplot2)


# UI for application ----

ui <- navbarPage("Modelling Resource Competition",
    
    ## PANEL 1: 1 CONS 1 RES ----
    tabPanel(title = "One Consumer, One Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu",
                             "Maximum growth rate:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("halfsat", 
                             "Half saturation:",
                             min = 0.01,
                             max = 1,
                             value = 0.2),
                 sliderInput("resconc",
                             "Resource concentration:",
                             min = 0.1,
                             max = 1,
                             value = 0.5)),
               
               # figures to include on this panel 
               mainPanel(h3("Functional response plots"),
                 fluidRow(column(12,align = "center",
                                         plotOutput(
                                           "FuncResp1",
                                           width = "70%",
                                           height = "300px"))),
                 h3("Population and resources through time"),
                 fluidRow(column(12,align = "center",
                                 plotOutput("OneConOneRes", 
                                            width = "90%", 
                                            height = "300px"))))
             )),
    
    
    ## PANEL 2: 2 CONS 1 RES ----
    # navbar menu to give drop-down options for cts or pulsed resourse 
    navbarMenu("Two Consumers, One Resource",
    ### PANEL 2A: 2 CONS 1 RES CTS ----
    tabPanel(title = "Continuously Supplied Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu21", #mu for panel 2, sp 1
                             "Sp1 maximum growth rate:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.3),
                 sliderInput("halfsat21", #half sat for panel 2, sp 1
                             "Sp1 half saturation:",
                             min = 0.01,
                             max = 1,
                             value = 0.45),
                 sliderInput("mu22", #mu for panel 2, sp 2
                             "Sp2 maximum growth rate:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.1),
                 sliderInput("halfsat22", #half sat for panel 2, sp 2
                             "Sp2 half saturation:",
                             min = 0.01,
                             max = 1,
                             value = 0.06),
                 sliderInput("resconc2",
                             "Resource concentration:",
                             min = 0.1,
                             max = 1,
                             value = 0.5)),
               
               # figures to include on this panel
             mainPanel(h3("Functional response plots"),
                       fluidRow(
                         column(12,align = "center",
                                plotOutput("FuncResp2", 
                                          width = "70%", 
                                          height = "300px"))),
               h3("Population and resources through time"),
               fluidRow(
                 column(12,align = "center",
                        plotOutput("TwoConOneRes",
                                   width = "90%", 
                                   height = "300px"))))
        )),
    
    ### PANEL 2B: 2 CONS 1 PULSED RES ----
    tabPanel(title = "Pulsed Resource", fluid = TRUE,
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu31", #mu for panel 3, sp 1
                             "Sp1 maximum growth rate:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.3),
                 sliderInput("halfsat31", #half sat for panel 3, sp 1
                             "Sp1 half saturation:",
                             min = 0.01,
                             max = 1,
                             value = 0.45),
                 sliderInput("mu32",
                             "Sp2 maximum growth rate:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.1),
                 sliderInput("halfsat32", #half sat for panel 3, sp 2
                             "Sp2 half saturation:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.06),
                 sliderInput("pulsefreq",
                             "Pulse interval size:",
                             min = 50,
                             max = 150,
                             value = 50)),
               
               # figures to include on this panel 
               mainPanel(h3("Functional response plot"),
                         fluidRow(
                           column(12,align = "center",
                                  plotOutput("FuncResp3", 
                                            width = "70%",
                                            height = "300px"))),
                         h3("Population and resources through time"),
                         fluidRow(
                           column(12,align = "center",
                                  plotOutput("TwoConOnePulRes",
                                             width = "90%",
                                             height = "300px"))))
        ))),
    
    
    ## PANEL 3: 2 CONS 2 RES ----
    tabPanel(title = "Two Consumers, Two Resources", 
             # Sidebar with a slider inputs 
             sidebarLayout(
               sidebarPanel(width = 3,
                 sliderInput("mu41A",# mu for panel 4, sp 1, res A
                             "Maximum growth rate for 
                             species 1 on resource A:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.12),
                 sliderInput("mu41B",# mu for panel 4, sp 1, res B
                             "Maximum growth rate for 
                             species 1 on resource B:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.12),
                 sliderInput("mu42A",# mu for panel 4, sp 2, res A
                             "Maximum growth rate for 
                             species 2 on resource A:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.2),
                 sliderInput("mu42B",# mu for panel 4, sp 2, res B
                             "Maximum growth rate for 
                             species 2 on resource B:",
                             min = 0.01,
                             max = 0.5,
                             value = 0.2),
                 sliderInput("resconc4",
                             "Resource concentrations:",
                             min = 0.2,
                             max = 2,
                             value = 0.2),
                 ),
               
               # figures to include on this panel
             mainPanel(h3("Functional response plots"),
                       h5("This plot is used to identify R*. 
                          Use this plot to determine the outcome: 
                          will species coexist,or will one 
                          species exclude the other?"),
               fluidRow(column(12,align = "center",
                        plotOutput("FuncResp4", 
                                      width = "80%", 
                                   height = "300px"))),
               h3("Population and resources through time"),
               fluidRow(column(12,align = "center",
                               plotOutput("TwoConTwoRes")))
      )))
)





# SERVER LOGIC ----

server <- function(input, output) {
  
  ## 1 CONS 1 RES ----
  
  # FUNCTIONAL RESPONSE PLOT
  output$FuncResp1 <- renderPlot({
    pars <- spec_rescomp(
      spnum = 1, 
      resnum = 1,
      funcresp = "type2",
      mumatrix = matrix(input$mu),
      kmatrix = matrix(input$halfsat),
      resspeed = 0.03,
      resconc = input$resconc,
      totaltime = 300)
    
    plot_funcresp(pars, maxx = 1) + 
      theme(text = element_text(size = 22),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 15),
            legend.text = element_text(size = 18),
            legend.position = "bottom")
  })
  
    # PLOT THROUGH TIME
    output$OneConOneRes <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 1, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(input$mu),
        kmatrix = matrix(input$halfsat),
        resspeed = 0.03,
        resconc = input$resconc,
        totaltime = 300)
      
      m1 <- sim_rescomp(pars)
      
      # plot results through time 
      plot_rescomp(m1) + theme(text = element_text(size=22),
                               axis.title = element_text(size = 18),
                               axis.text = element_text(size = 15),
                               legend.text = element_text(size = 18),
                               legend.position = "bottom")
    })
    

    ## 2 CONS 1 CTS RES ----
    # FUNCTIONAL RESPONSE PLOT
    output$FuncResp2 <- renderPlot({
      # specify rescomp parameters 
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu21,input$mu22), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE), 
        kmatrix = matrix(c(input$halfsat21, input$halfsat22),
                         nrow = 2,
                         ncol = 1,
                         byrow = TRUE),
        resspeed = 0.03, 
        resconc = input$resconc2,
        totaltime = 500)
      
      # functional response plot 
      plot_funcresp(pars, maxx = 1) + 
        theme(text = element_text(size = 22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })
    
    #PLOT THROUGH TIME
    output$TwoConOneRes <- renderPlot({
      #simulate based on input params from UI
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu21,
                            input$mu22), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),  
        kmatrix = matrix(c(input$halfsat21, 
                           input$halfsat22), 
                         nrow = 2, 
                         ncol = 1, 
                         byrow = TRUE), 
        resspeed = 0.03, 
        resconc = input$resconc2,
        totaltime = 500)
      
      m1 <- sim_rescomp(pars)
      
      # plot results through time 
      plot_rescomp(m1) + 
        theme(text = element_text(size=22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })
    
    
   
    ## 2 CONS 1 PULSED RES ----
    # FUNCTIONAL RESPONSE PLOT
    output$FuncResp3 <- renderPlot({
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu31,input$mu32), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),
        kmatrix = matrix(c(input$halfsat31, input$halfsat32), 
                         nrow = 2, 
                         ncol = 1, 
                         byrow = TRUE),  
        resspeed = 0, # set to zero for no additional resource supply 
        resconc = 0.2,
        respulse = 0.3,
        pulsefreq = input$pulsefreq # resource pulse size
        )
      # plot functional responses 
      plot_funcresp(pars, maxx = 1) + 
        theme(text = element_text(size = 22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })

    #PLOT THROUGH TIME
    output$TwoConOnePulRes <- renderPlot({
      pars <- spec_rescomp(
        spnum = 2, 
        resnum = 1,
        funcresp = "type2",
        mumatrix = matrix(c(input$mu31,input$mu32), 
                          nrow = 2, 
                          ncol = 1,
                          byrow = TRUE),
        kmatrix = matrix(c(input$halfsat31, input$halfsat32), 
                         nrow = 2, 
                         ncol = 1, 
                         byrow = TRUE),  
        resspeed = 0, # set to zero for no additional resource supply 
        resconc = 0.2,
        respulse = 0.3,
        pulsefreq = input$pulsefreq # resource pulse size
      )
      
      m1 <- sim_rescomp(pars)
      
      # plot outcome through time 
      plot_rescomp(m1) + 
        theme(text = element_text(size=22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })

       
    ## 2 CONS 2 RES ----
    #FUNCTIONAL RESPONSE PLOT
    output$FuncResp4 <- renderPlot({
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
        resconc = input$resconc4, 
        #both resources have same concentration??
        mort = 0.03,
        essential = FALSE,
        totaltime = 500)
      
      # functional response plot 
      plot_funcresp(pars, maxx = 1) + 
        theme(text = element_text(size = 22),
              axis.title = element_text(size = 18),
              axis.text = element_text(size = 15),
              legend.text = element_text(size = 18),
              legend.position = "bottom")
    })
    
    #PLOT THROUGH TIME
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
      resconc = input$resconc4, 
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

# Run the application ----
shinyApp(ui = ui, server = server)
