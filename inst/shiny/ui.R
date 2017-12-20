library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Saw Tooth Inventory Plot Generator"),
  dashboardSidebar(
    shinyjs::useShinyjs(),
    width = 400,
    h3("Inputs", align= "center"),
    div(id = "displayPlotNumber",
        sliderInput("enterPlotNumber", label = "Number of Plots",
                    min = 1, max = 3, value = 3, step = 1)
        ),
    checkboxInput("enterPPV", label = "Enable PPV Mode",
                  value = FALSE),
    checkboxInput("calcFR", label = "Estimate Fill-Rate",
                  value = TRUE)
    ),
  dashboardBody(
    fluidRow(
      box(width = 10,
          plotOutput("plotSawTooth")
      ),
      div(id = "PPV",
          div(id = "PPVFR",
            fluidRow(
              box(
                title = "Fill Rate Projection", width = 2,
                numericInput("enterPPVSigma", label = "Lead Time Demand Standard Deviation",
                             min = 0, max = Inf, step = NA, value = 0),
                h3("Fill Rate Estimate"),
                textOutput("PPVFR")
              )
            )
            )
          )
    ),
    div(id = "standard",
        fluidRow(
          box(
            title = "Plot 1 Parameters", width = 4,
            uiOutput("SL1"),
            uiOutput("EOQ1"),
            uiOutput("Dmd1"),
            uiOutput("LT1"),
            div(id = "LTHide", uiOutput("plotLT"))
          ),
          div(id = "plot2Box",
              box(title = "Plot 2 Parameters", width = 4,
                  uiOutput("SL2"),
                  uiOutput("EOQ2"),
                  uiOutput("Dmd2"),
                  uiOutput("LT2")
                  )
             ),
          div(id = "plot3Box",
              box(title = "Plot 3 Parameters", width = 4,
                  uiOutput("SL3"),
                  uiOutput("EOQ3"),
                  uiOutput("Dmd3"),
                  uiOutput("LT3")
                  )
              )
        ),
        fluidRow(
          div(id = "FR1",
              box(title = "Fill Rate Projection", width = 4,
                  numericInput("enterFR1Sigma", label = "Lead Time Demand Standard Deviation",
                               min = 0, max = Inf, step = NA, value = 0),
                  h3("Fill Rate Estimate"),
                  textOutput("FR1")
                  )
              ),
          div(id = "FR2",
              box(title = "Fill Rate Projection", width = 4,
                  numericInput("enterFR2Sigma", label = "Lead Time Demand Standard Deviation",
                               min = 0, max = Inf, step = NA, value = 0),
                  h3("Fill Rate Estimate"),
                  textOutput("FR2")
                  )
              ),
          div(id = "FR3",
              box(title = "Fill Rate Projection", width = 4,
                  numericInput("enterFR3Sigma", label = "Lead Time Demand Standard Deviation",
                               min = 0, max = Inf, step = NA, value = 0),
                  h3("Fill Rate Estimate"),
                  textOutput("FR3")
                  )
              )
          )
        ),
    div(id = "PPV2",
        fluidRow(
          box(
            title = "PPV Mode", width = 12,
            uiOutput("PPV_SL"),
            uiOutput("PPV_Demand"),
            uiOutput("PPV_CRR"),
            uiOutput("PPV_SR"),
            uiOutput("PPV_PCLT"),
            uiOutput("PPV_Cov_Dur"),
            uiOutput("PPV_PRTAT"),
            uiOutput("PPV_Repair_period")
          )
        )
        )
  )
)
