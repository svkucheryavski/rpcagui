library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  title = "PCA analysis"
)

body <- dashboardBody(
  tags$head(tags$script(HTML('
      Shiny.addCustomMessageHandler("jsCode",
                             function(message) {
                             console.log(message)
                             eval(message.code);
                             }
  );
  '))),
  fluidRow(
    column(width = 2,
           box(width = NULL, status = "warning",
               p(style = 'font-size:8pt;', uiOutput('modelInfo')),
               uiOutput("expvarUI")
           ),
           box(width = NULL, status = "warning",
               p(style = 'font-size:8pt;', uiOutput('dataInfo')),
               checkboxGroupInput("prep", "Preprocessing",
                                  choices = c(
                                    Centering = 1,
                                    Standardization = 2,
                                    SNV = 3
                                  ),
                                  selected = c(1)
               ),
               actionButton("btnPreprocess", "Apply"),
               actionButton("btnReload", "Reload")
           ),
           uiOutput("plotSettings")
    ),
    column(width = 8,
           fluidRow(
             column(width = 6,
                    box(width = NULL, 
                        plotOutput("imagePlot",
                                   height = 400,
                                   click = "image_click"
                        )
                    )
             ),
             column(width = 6,
                    box(width = NULL, 
                        plotOutput(
                          "scoresPlot",
                          height = 400,
                          brush = brushOpts(
                            id = "plot_brush",
                            delayType = 'debounce',
                            delay = 300,
                            resetOnNew = TRUE
                          )
                        )
                    )
             )
           ),
           fluidRow(
             column(width = 12,
                    box(width = NULL,
                        plotOutput(
                          "variablesPlot",
                          height = 300
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