library(shiny)
library(shinydashboard)
library(hexbin)

disableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste("$('#",id,"').prop('disabled',true)"
                                             ,sep="")))
}

enableActionButton <- function(id,session) {
  session$sendCustomMessage(type="jsCode",
                            list(code= paste(
                              "$('#",id,"').prop('disabled',false);", sep="")))
}

function(input, output, session) {
  source('methods.R')
  source('misc.R')
  source('prep.R')
  
  project = reactiveValues(data = NULL, model = NULL, selected = F, selectedVar = 1, selectedObj = NULL, varstats = NULL)
  
  observe({
    input$prep
    enableActionButton("btnPreprocess", session)
  })
  
  observe({
    if (is.null(input$plot_brush)) {
      project$selected = NULL
      updateActionButton(session, "btnCompute", label = 'Compute model')
    } else {
      project$selected = isolate({which(
        project$model$T[, 1] >= input$plot_brush$xmin &
          project$model$T[, 1] <= input$plot_brush$xmax &
          project$model$T[, 2] >= input$plot_brush$ymin &
          project$model$T[, 2] <= input$plot_brush$ymax 
      )})
      updateActionButton(session, "btnCompute", label = 'Exclude pixels and recalculate')
    }
  })
  
  observe({
    if (is.null(project$data)) {
      dlg = modalDialog(
        title = "Loading data",
        'Trying to locate and load dataset...',
        easyClose = TRUE,
        footer = NULL
      )
      showModal(dlg)
      
      project$data = readRDS('image.rds')
      
      if (is.null(attr(project$data, 'xaxis.values')))
        attr(project$data, 'xaxis.values') = 1:ncol(project$data)
      
      if (is.null(attr(project$data, 'xaxis.name')))
        attr(project$data, 'xaxis.name') = 'Variables'
      
      colnames(project$data) = paste(attr(project$data, 'xaxis.values'), 'nm')
      
      m = apply(project$data, 2, mean)
      s = apply(project$data, 2, sd)
      
      project$varstats = rbind(m, m - s, m + s)
      attr(project$varstats, 'xaxis.values') = attr(project$data, 'xaxis.values')
      attr(project$varstats, 'xaxis.name') = attr(project$data, 'xaxis.name')

      project$model = NULL
      project$selected = NULL
      project$selectedObj = NULL
      project$selectedVar = 1
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
  
  observeEvent(input$btnReload, {
    enableActionButton("btnPreprocess", session)
    project$data = NULL
  })
  
  observeEvent(input$btnPreprocess, {
    dlg = modalDialog(
      title = "Preprocessing data",
      'Appying selected preprocessing to the data...',
      easyClose = TRUE,
      footer = NULL
    )
    
    showModal(dlg)
    isolate({
     if (any(input$prep == 1))
        project$data = prep.autoscale(project$data, center = T, scale = F)
     if (any(input$prep == 2))
        project$data = prep.autoscale(project$data, center = F, scale = T)
     if (any(input$prep == 3))
       project$data = prep.snv(project$data)
     
     m = apply(project$data, 2, mean)
     s = apply(project$data, 2, sd)
     
     project$varstats = rbind(m, m - s, m + s)
     attr(project$varstats, 'xaxis.values') = attr(project$data, 'xaxis.values')
     attr(project$varstats, 'xaxis.name') = attr(project$data, 'xaxis.name')
    })
    
    disableActionButton("btnPreprocess", session)
    removeModal()
  })
  
  observeEvent(input$btnCompute, {
    
    dlg = modalDialog(
      title = "Computing PCA model",
      "Creating a PCA model ...",
      easyClose = FALSE,
      footer = NULL
    )
    showModal(dlg)
    isolate({
      if (!is.null(project$selected))
        project$data = mda.setimbg(project$data, project$selected)
    })
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
  
  output$variablesPlot = renderPlot({
    if (is.null(project$data)) {
      return()
    } else {
      attrs = mda.getattr(project$data)
      w = attrs$width
      h = attrs$height
      i = round(input$image_click$x * w * h + (1 - input$image_click$y) * h)
      idx = 1:(w*h)
      idx = idx[-attrs$bgpixels]
      idx = which.min(abs(i - idx)) 
      
      if (!is.null(idx) && length(idx) == 1){
        lty = c(1, 2, 2, 1)
        col = c(rep('darkgray', 3), 'red')
        plotdata = t(rbind(project$varstats, project$data[idx, ]))
      } else {
        lty = c(1, 2, 2)
        col = 'darkgray'
        plotdata = t(project$varstats)
      }
      
      matplot(attrs$xaxis.values, plotdata, type = 'l', lty = lty, xlab = attrs$xaxis.name, ylab = '', col = col)
      abline(v = attrs$xaxis.values[project$selectedVar], col = 'darkred', lty = 3)
      grid()
    } 
  })
 
  output$expvarUI = renderUI({
    if (is.null(project$model)) {
      return()
    }  else {
      plotOutput('expvarPlot', height = 100)
    }
    
  }) 
  
  output$expvarPlot = renderPlot({
    if (is.null(project$model)) {
      return()
    }  else {
      par(mar = c(1, 1, 1, 1))
      barplot(project$model$expvar, col = 'orange', border = NA, xaxt = 'n', yaxt = 'n')
    }
  })
  
  output$dataInfo = renderUI({
    if (is.null(project$data)) {
      HTML('Dataset is not available.')
    } else {
      d = dim(project$data)
      w = attr(project$data, 'width')
      h = attr(project$data, 'height')
      e = length(attr(project$data, 'bgpixels'))
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