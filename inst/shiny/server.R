library(dplyr)
library(scales)
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

  observe({
    if (length(values$plotList) > 0 && input$enterFR1Sigma > 0){
      paramDb <- values$plotList$plotData1
      paramDb$LTD_sigma <- input$enterFR1Sigma
      fillRate <-
        scales::percent(round(estimateFR(paramDb)$FR, 4))
    } else {
      fillRate <- "Enter a Positive Standard Deviation"
    }
    values$FR1fillRate <- fillRate
  })
  observe({
    if (length(values$plotList) > 0 && input$enterFR2Sigma > 0){
      paramDb <- values$plotList$plotData2
      paramDb$LTD_sigma <- input$enterFR2Sigma
      fillRate <-
        scales::percent(round(estimateFR(paramDb)$FR, 4))
    } else {
      fillRate <- "Enter a Positive Standard Deviation"
    }
    values$FR2fillRate <- fillRate
  })
  observe({
    if (length(values$plotList) > 0 && input$enterFR3Sigma > 0){
      paramDb <- values$plotList$plotData3
      paramDb$LTD_sigma <- input$enterFR3Sigma
      fillRate <-
        scales::percent(round(estimateFR(paramDb)$FR, 4))
    } else {
      fillRate <- "Enter a Positive Standard Deviation"
    }
    values$FR3fillRate <- fillRate
  })
  observe({
      if (length(values$plotList) > 0 && input$enterPPVSigma > 0) {
        paramDb <- ppvDecom(
          SL = input$enterPPV_SL,
          Demand = input$enterPPV_Demand,
          CRR = input$enterPPV_CRR,
          SR = input$enterPPV_SR,
          PCLT = input$enterPPV_PCLT,
          PRTAT = input$enterPPV_PRTAT,
          Cov_Dur = input$enterPPV_Cov_Dur,
          Repair_period = input$enterPPV_Repair_period) %>%
          mutate(LTD_sigma = input$enterPPVSigma) %>%
          filter(Title == "PPV")
        fillRate <-
          scales::percent(round(estimateFR(paramDb)$FR, 4))
      } else {
        fillRate <- "Enter a Positive Standard Deviation"
      }
      values$PPVfillRate <- fillRate
  })

  ## Store the fill-rate values
  output$PPVFR <- renderText({
    paste(values$PPVfillRate)
  })
  output$FR1 <- renderText({
    paste(values$FR1fillRate)
  })
  output$FR2 <- renderText({
    paste(values$FR2fillRate)
  })
  output$FR3 <- renderText({
    paste(values$FR3fillRate)
  })

  ## PPV Mode or standard
  observe({
    if (input$enterPPV) {
      shinyjs::show(id = "PPV")
      shinyjs::show(id = "PPV2")
      shinyjs::hide(id = "standard")
      shinyjs::hide(id = "displayPlotNumber")
      if (input$calcFR) {
        shinyjs::show(id = "PPVFR")
      } else {
        shinyjs::hide(id = "PPVFR")
      }
    } else {
      shinyjs::hide(id = "PPV")
      shinyjs::hide(id = "PPV2")
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
        if (input$calcFR) {
          shinyjs::show(id = "FR1")
          shinyjs::hide(id = "FR2")
          shinyjs::hide(id = "FR3")
        } else {
          shinyjs::hide(id = "FR1")
          shinyjs::hide(id = "FR2")
          shinyjs::hide(id = "FR3")
        }
      } else if (input$enterPlotNumber == 2) {
        shinyjs::show(id = "plot2Box")
        shinyjs::hide(id = "plot3Box")
        shinyjs::hide(id = "LTHide")
        if (input$calcFR) {
          shinyjs::show(id = "FR1")
          shinyjs::show(id = "FR2")
          shinyjs::hide(id = "FR3")
        } else {
          shinyjs::hide(id = "FR1")
          shinyjs::hide(id = "FR2")
          shinyjs::hide(id = "FR3")
        }
        values$plotList$plotData3 <- NULL
        isolate({
          values$plotList$plotData2 <- data.frame(SL = input$enterSL2,
                                                  EOQ = input$enterEOQ2,
                                                  Dmd = input$enterDmd2,
                                                  LT =  input$enterLT2,
                                                  Title = "Plot 2")
        })
      } else {
        shinyjs::show(id = "plot2Box")
        shinyjs::show(id = "plot3Box")
        shinyjs::hide(id = "LTHide")
        if (input$calcFR) {
          shinyjs::show(id = "FR1")
          shinyjs::show(id = "FR2")
          shinyjs::show(id = "FR3")
        } else {
          shinyjs::hide(id = "FR1")
          shinyjs::hide(id = "FR2")
          shinyjs::hide(id = "FR3")
        }
        isolate({
          if (!is.null(input$enterSL2)) {
            values$plotList$plotData2 <- data.frame(SL = input$enterSL2,
                                                    EOQ = input$enterEOQ2,
                                                    Dmd = input$enterDmd2,
                                                    LT =  input$enterLT2,
                                                    Title = "Plot 2")
          }
        })
        isolate({
          if (!is.null(input$enterSL3)) {
            values$plotList$plotData3 <- data.frame(SL = input$enterSL3,
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
    input$enterSL1
    input$enterEOQ1
    input$enterDmd1
    input$enterLT1
  },
  isolate({
    values$plotList$plotData1 <- data.frame(SL = input$enterSL1,
                                  EOQ = input$enterEOQ1,
                                  Dmd = input$enterDmd1,
                                  LT =  input$enterLT1,
                                  Title = "Plot 1")
  })
  )
  observeEvent({
    input$enterSL2
    input$enterEOQ2
    input$enterDmd2
    input$enterLT2
  },
  isolate({
    values$plotList$plotData2 <- data.frame(SL = input$enterSL2,
                                  EOQ = input$enterEOQ2,
                                  Dmd = input$enterDmd2,
                                  LT =  input$enterLT2,
                                  Title = "Plot 2")
  })
  )
  observeEvent({
    input$enterSL3
    input$enterEOQ3
    input$enterDmd3
    input$enterLT3
  },
  isolate({
    values$plotList$plotData3 <- data.frame(SL = input$enterSL3,
                                  EOQ = input$enterEOQ3,
                                  Dmd = input$enterDmd3,
                                  LT =  input$enterLT3,
                                  Title = "Plot 3")
  })
  )

  ## Layout Parameters (Standard)
  output$SL1 <- renderUI({
    numericInput("enterSL1", label = "Safety Level",
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
  output$SL2 <- renderUI({
    numericInput("enterSL2", label = "Safety Level",
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
  output$SL3 <- renderUI({
    numericInput("enterSL3", label = "Safety Level",
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
  output$PPV_SL <- renderUI({
    numericInput("enterPPV_SL", label = "Safety Level",
                 min = 0, max = NA, step = NA, value = 0)
  })
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
        plotST(SL = pltData,
               plotLTs = input$plotLT,
               axis_text = textSize)
      } else {
        plotST(ppvDecom(
          SL = input$enterPPV_SL,
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


