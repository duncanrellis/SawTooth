library(dplyr)
library(shiny)
library(shinydashboard)
library(SawTooth)

shinyServer(function(input, output, session) {

  textSize <- 18

  slowstart <- reactiveValues(starting = TRUE)
  session$onFlushed(function () slowstart$starting <- FALSE)

  values <- reactiveValues(
    plotList = list()
  )

  ## PPV Mode or standard
  observe({
    if (input$enterPPV) {
      shinyjs::show(id = "PPV")
      shinyjs::hide(id = "standard")
      shinyjs::hide(id = "displayPlotNumber")
    } else {
      shinyjs::hide(id = "PPV")
      shinyjs::show(id = "standard")
      shinyjs::show(id = "displayPlotNumber")
    }
  })

  ## Control the UI "switching"
  observe({
      if (input$enterPlotNumber == 1) {
        shinyjs::hide(id = "plot2Box")
        shinyjs::hide(id = "plot3Box")
        shinyjs::show(id = "LTHide")
        values$plotList$plotData2 <- NULL
        values$plotList$plotData3 <- NULL
      } else if (input$enterPlotNumber == 2) {
        shinyjs::show(id = "plot2Box")
        shinyjs::hide(id = "plot3Box")
        shinyjs::hide(id = "LTHide")
        values$plotList$plotData3 <- NULL
        isolate({
          values$plotList$plotData2 <- data.frame(ROP = input$enterROP2,
                                                  EOQ = input$enterEOQ2,
                                                  Dmd = input$enterDmd2,
                                                  LT =  input$enterLT2,
                                                  Title = "Plot 2")
        })
      } else {
        shinyjs::show(id = "plot2Box")
        shinyjs::show(id = "plot3Box")
        shinyjs::hide(id = "LTHide")
        isolate({
          if (!is.null(input$enterROP2)) {
            values$plotList$plotData2 <- data.frame(ROP = input$enterROP2,
                                                    EOQ = input$enterEOQ2,
                                                    Dmd = input$enterDmd2,
                                                    LT =  input$enterLT2,
                                                    Title = "Plot 2")
          }
        })
        isolate({
          if (!is.null(input$enterROP3)) {
            values$plotList$plotData3 <- data.frame(ROP = input$enterROP3,
                                                    EOQ = input$enterEOQ3,
                                                    Dmd = input$enterDmd3,
                                                    LT =  input$enterLT3,
                                                    Title = "Plot 3")
          }
        })
      }
  })

  ## Observers for reactive values, update plotList
  observeEvent({
    input$enterROP1
    input$enterEOQ1
    input$enterDmd1
    input$enterLT1
  },
  isolate({
    values$plotList$plotData1 <- data.frame(ROP = input$enterROP1,
                                  EOQ = input$enterEOQ1,
                                  Dmd = input$enterDmd1,
                                  LT =  input$enterLT1,
                                  Title = "Plot 1")
  })
  )
  observeEvent({
    input$enterROP2
    input$enterEOQ2
    input$enterDmd2
    input$enterLT2
  },
  isolate({
    values$plotList$plotData2 <- data.frame(ROP = input$enterROP2,
                                  EOQ = input$enterEOQ2,
                                  Dmd = input$enterDmd2,
                                  LT =  input$enterLT2,
                                  Title = "Plot 2")
  })
  )
  observeEvent({
    input$enterROP3
    input$enterEOQ3
    input$enterDmd3
    input$enterLT3
  },
  isolate({
    values$plotList$plotData3 <- data.frame(ROP = input$enterROP3,
                                  EOQ = input$enterEOQ3,
                                  Dmd = input$enterDmd3,
                                  LT =  input$enterLT3,
                                  Title = "Plot 3")
  })
  )

  ## Layout Parameters (Standard)
  output$ROP1 <- renderUI({
    numericInput("enterROP1", label = "Reorder Point",
                 min = 0, max = NA, step = NA, value = 2)
  })
  output$EOQ1 <- renderUI({
        numericInput("enterEOQ1", label = "Order Quantity",
                 min = 0, max = NA, step = NA, value = 5)
  })
  output$Dmd1 <- renderUI({
    numericInput("enterDmd1", label = "Demand (per unit time)",
                 min = 0, max = NA, step = NA, value = 4)
  })
  output$LT1 <- renderUI({
        numericInput("enterLT1", label = "Lead Time",
                 min = 0, max = NA, step = NA, value = 1)
  })
  output$plotLT <- renderUI({
    checkboxInput("plotLT", label = "Show Lead Times", value = TRUE)
  })
  ## Inventory ST 2
  output$ROP2 <- renderUI({
    numericInput("enterROP2", label = "Reorder Point",
                 min = 0, max = NA, step = NA, value = 3)
  })
  output$EOQ2 <- renderUI({
        numericInput("enterEOQ2", label = "Order Quantity",
                 min = 0, max = NA, step = NA, value = 2)
  })
  output$Dmd2 <- renderUI({
    numericInput("enterDmd2", label = "Demand (per unit time)",
                 min = 0, max = NA, step = NA, value = 2.4)
  })
  output$LT2 <- renderUI({
        numericInput("enterLT2", label = "Lead Time",
                 min = 0, max = NA, step = NA, value = 1)
  })
  ## Inventory ST 3
  output$ROP3 <- renderUI({
    numericInput("enterROP3", label = "Reorder Point",
                 min = 0, max = NA, step = NA, value = 4)
  })
  output$EOQ3 <- renderUI({
        numericInput("enterEOQ3", label = "Order Quantity",
                 min = 0, max = NA, step = NA, value = 3)
  })
  output$Dmd3 <- renderUI({
    numericInput("enterDmd3", label = "Demand (per unit time)",
                 min = 0, max = NA, step = NA, value = 2.5)
  })
  output$LT3 <- renderUI({
        numericInput("enterLT3", label = "Lead Time",
                 min = 0, max = NA, step = NA, value = 1)
  })

  ## LayoutParameters PPV
  output$PPV_Demand <- renderUI({
    numericInput("enterPPV_Demand", label = "Demand (per unit time)",
                 min = 0, max = NA, step = NA, value = 4)
  })
  output$PPV_CRR <- renderUI({
    numericInput("enterPPV_CRR", label = "Carcass Return Rate",
                 min = 0, max = 1, step = NA, value = .7)
  })
  output$PPV_SR <- renderUI({
    numericInput("enterPPV_SR", label = "Carcass Survival Rate",
                 min = 0, max = 1, step = NA, value = .8)
  })
  output$PPV_PCLT <- renderUI({
    numericInput("enterPPV_PCLT", label = "Procurement Lead Time",
                 min = 0, max = NA, step = NA, value = 3)
  })
  output$PPV_PRTAT <- renderUI({
    numericInput("enterPPV_PRTAT", label = "Process Repair Turnaround Time",
                 min = 0, max = NA, step = NA, value = 1.5)
  })
  output$PPV_Cov_Dur<- renderUI({
    numericInput("enterPPV_Cov_Dur", label = "Procurement Coverage Duration",
                 min = 0, max = NA, step = NA, value = 2)
  })
  output$PPV_Repair_period <- renderUI({
    numericInput("enterPPV_Repair_period", label = "Repair Period",
                 min = 0, max = NA, step = NA, value = 1)
  })

  ## Put up the inventory plot
  output$plotSawTooth <- renderPlot({
    if (length(values$plotList) > 0) {
      if (!input$enterPPV) {
        pltData <- suppressWarnings(do.call("bind_rows", values$plotList))
        plotST(ROP = pltData,
               plotLTs = input$plotLT,
               axis_text = textSize)
      } else {
        plotST(ppvDecom(
          Demand = input$enterPPV_Demand,
          CRR = input$enterPPV_CRR,
          SR = input$enterPPV_SR,
          PCLT = input$enterPPV_PCLT,
          PRTAT = input$enterPPV_PRTAT,
          Cov_Dur = input$enterPPV_Cov_Dur,
          Repair_period = input$enterPPV_Repair_period),
          axis_text = textSize)
      }
    }
    })
})


