library(shiny)
library(shinydashboard)
library(hexbin)

function(input, output, session) {
  source('methods.R')
  source('misc.R')
  
  project = reactiveValues(data = NULL, model = NULL, selected = F, preprocess = F, recalculate = F)
  
  observe({
    if (is.null(input$plot_brush)) {
      project$selected = NULL
    } else {
      project$selected = isolate({which(
        project$model$T[, 1] >= input$plot_brush$xmin &
          project$model$T[, 1] <= input$plot_brush$xmax &
          project$model$T[, 2] >= input$plot_brush$ymin &
          project$model$T[, 2] <= input$plot_brush$ymax 
      )})
    }
  })
  
  observe({
    dlg = modalDialog(
      title = "Loading data",
      'Trying to locate and load dataset...',
      easyClose = TRUE,
      footer = NULL
    )
    
    showModal(dlg)
    if (is.null(project$data)) {
      project$data = readRDS('image.rds')
      colnames(project$data) = paste(attr(project$data, 'xaxis.values'), 'nm')
    } else {
      removeModal()
    }
  })
  
  output$modelInfo = renderUI({
    out = '<b>Model</b><br>'
    if (is.null(project$model)) {
      out = paste(out, 'PCA model is not available')
    } else {
      out = paste(out, sprintf('Number of components: %d <br>', ncol(project$model$P)))
      out = paste(out, sprintf('Computational time: %.1f s.<br>', project$model$time[3]))
    }
    out = HTML(out)
    out
  })
  
  observeEvent(input$btnCompute, {
    dlg = modalDialog(
      title = "Computing PCA model",
      'Preprocessing data and computing PCA model...',
      easyClose = FALSE,
      footer = NULL
    )
    
    showModal(dlg)
    ptm <- proc.time()
    P = getP(project, input)
    project$model = getModel(project, input, P)
    project$model$time = proc.time() - ptm
    removeModal()
  })
  
  output$imagePlot = renderPlot({
    if (is.null(project$data)) {
      return()
    }  else {
      par(mar = c(1, 1, 1, 1))
      imshow(project$data, selected = project$selected)
    }
  })
  
  output$scoresPlot = renderPlot({
    if (is.null(project$model)) {
      return()
    }  else {
      plotScores(project$model)
    }
  })
  
  output$dataInfo = renderUI({
    if (is.null(project$data)) {
      HTML('Dataset is not available.')
    } else {
      d = dim(project$data)
      w = attr(project$data, 'width')
      h = attr(project$data, 'height')
      e = attr(project$data, 'excluded.rows')
      if (is.null(e))
        e = 0
      HTML(
        sprintf(
          '<b>Dataset</b><br>Pixels: %d (%dx%d)<br>Variables: %d<br>Exlcuded pixels: %d', 
          d[1], w, h, d[2], e
        )
      )
    }
    
  })
  
}