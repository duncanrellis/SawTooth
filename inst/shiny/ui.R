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
                  value = FALSE)
    ),
  dashboardBody(
    fluidRow(
      box(
          plotOutput("plotSawTooth")
      )
    ),
    div(id = "standard",
        fluidRow(
          box(
            title = "Plot 1 Parameters", width = 4,
            uiOutput("ROP1"),
            uiOutput("EOQ1"),
            uiOutput("Dmd1"),
            uiOutput("LT1"),
            div(id = "LTHide", uiOutput("plotLT"))
          ),
          div(id = "plot2Box",
              box(title = "Plot 2 Parameters", width = 4,
                  uiOutput("ROP2"),
                  uiOutput("EOQ2"),
                  uiOutput("Dmd2"),
                  uiOutput("LT2")
                  )
             ),
          div(id = "plot3Box",
              box(title = "Plot 3 Parameters", width = 4,
                  uiOutput("ROP3"),
                  uiOutput("EOQ3"),
                  uiOutput("Dmd3"),
                  uiOutput("LT3")
                  )
              )
        )
        ),
    div(id = "PPV",
        fluidRow(
          box(
            title = "PPV Mode", width = 12,
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
