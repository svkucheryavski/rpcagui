library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "PCA analysis"
)

body <- dashboardBody(
  fluidRow(
    column(width = 2,
           box(width = NULL, status = "warning",
               p(style = 'font-size:8pt;', uiOutput('modelInfo'))
           ),
           box(width = NULL, status = "warning",
               p(style = 'font-size:8pt;', uiOutput('dataInfo')),
               checkboxGroupInput("prep", "Preprocessing",
                                      choices = c(
                                        Centering = 1,
                                        Standardization = 2
                                      ),
                                      selected = c(1, 2)
              ),
               actionButton("btnApply", "Apply"),
               actionButton("btnReload", "Reload")
           )
    ),
    column(width = 8,
           fluidRow(
             column(width = 6,
                    box(width = NULL, 
                        plotOutput("imagePlot",
                                   height = 600,
                                   brush = brushOpts(
                                     id = "plot_brush"
                                   )
                        )
                    )
             ),
             column(width = 6,
                    box(width = NULL, 
                        plotOutput(
                          "scoresPlot",
                          height = 600,
                          brush = brushOpts(
                            id = "plot_brush",
                            delayType = 'debounce',
                            delay = 500,
                            resetOnNew = TRUE
                          )
                        )
                    )
             )
           )
    ),
    column(width = 2,
           box(width = NULL, status = "warning",
               selectInput("method", "PCA decomposition",
                           choices = c(
                             "SVD" = 'svd',
                             "NIPALS" = 'nipals',
                             "Eigcov" = 'eigcov'
                           ),
                           selected = "svd"
               ),
               selectInput("algorithm", "Algorithm",
                           choices = c(
                             "Conventional" = 'conv',
                             "Random" = 'rand'
                           ),
                           selected = "rand"
               ),
               sliderInput('ncomp', 'Number of components', 
                           min = 2, 
                           max = 10, 
                           value = 4
               )        
           ),
           box(width = NULL, status = "warning",
               sliderInput('p', 'Oversampling', 
                           min = 0, 
                           max = 20, 
                           value = 5
               ),
               sliderInput('q', 'Number of iterations', 
                           min = 0, 
                           max = 5, 
                           value = 0
               )
           ),
           box(width = NULL, status = "warning",
               actionButton("btnCompute", "Compute model")
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)