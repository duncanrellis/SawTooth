if (FALSE) {

  devtools::load_all("~/Projects/SawTooth")
  plotST(SL = 0, EOQ = 0, Dmd = 150, LT = 1, plotLTs = TRUE)
  plotST(SL = 0, EOQ = 100, Dmd = 150, LT = 1, plotLTs = TRUE)
  plotST(SL = c(0,1,0), EOQ = c(1,2,3), Dmd = c(2,2,2), LT = 2, plotLTs = TRUE)
  plotST(SL = 4, EOQ = 1.5, Dmd = 1, LT = 2, xMax = 10, plotLTs = TRUE)
  plotST(SL = 4, EOQ = 4, Dmd = 1, LT = 2, cycleLimits = 2, plotLTs = TRUE)

  paramDb <- data.frame(SL = c(2,3,4),
                        EOQ = c(1,2,3),
                        Dmd = c(4,2.4,2.5),
                        LT =  c(1,1,1),
                        Title = paste("Item", 1:3, sep = " "))
  plotST(paramDb)

  plotST(SL = 0, EOQ = 2, Dmd = 1, LT = 2, plotLTs = TRUE, lSize = 1, axis_text = 12)

  Demand = 4; CRR = 0; SR = .8; PCLT = 3; Cov_Dur = 2; PRTAT = 9; Repair_period = 1;

  plotST(
    ppvDecom(Demand = Demand, CRR = CRR, SR = SR, PCLT = PCLT, PRTAT = PRTAT)
  )

  estimateFR(SL = 15, EOQ = 107, Dmd = 139.277, LT = 1, LT_sigma = 52.65,  Dmd_sigma = 1)

}
##' Using approach in Silver, et all, estimate fill rate of CR system
##' \eqn{1-f =  \frac{1}{Q} \int_{s^{'}}^{\infty} (x - s^{'}) f_{X^{'}}  {dx}}
##
##'
##' @param EOQ numeric, er quantity
##' @param Dmd numeric, quarterly demand forecast
##' @param LT numeric, lead time
##' @param ... numeric, additional variance params for LT and Dmd.  Either LTD_sigma or both LT_sigma and Dmd_sigma must be supplied.
##' @param SL either data.frame containing SL, EOQ, Dmd, LT, and variance measure, or numeric SL minimum
##' @import dplyr
##' @export
estimateFR <- function(SL, EOQ, Dmd, LT, ...) {

  dots <- list(...)

  if (is.data.frame(SL)) {
    paramDb <- SL
    if (!xor(sum(c("LT", "Dmd_sigma") %in% names(SL)) == 2,
        c("LTD_sigma") %in% names(SL)) ||
        !sum(c("SL", "EOQ", "Dmd", "LT") %in% names(paramDb)) == 4) {
      stop("data.frame does not contain correct columns", call. = TRUE)
    }
  } else {
    LTD_sigma <- do.call("sum", dots)
    if (!xor(sum(c("LT_sigma", "Dmd_sigma") %in% names(dots)) == 2,
             c("LTD_sigma") %in% names(dots))) {
      stop("Must supply EITHER LT_sigma and Dmd_sigma OR LTD_sigma", call. = TRUE)
    }
    paramDb <- getParamDb(SL, EOQ, Dmd, LT, catNames = letters[1:length(SL)])
    paramDb$LTD_sigma <- LTD_sigma

  }
  paramDb$LTD <- paramDb$Dmd * paramDb$LT

  ## Calculate the cycle numbers and additional parameters
  paramDb$cycleTime <- pmax(1, paramDb$LTD / paramDb$EOQ)
  paramDb$LTD_Prime <- paramDb$LTD / paramDb$cycleTime
  paramDb$LTD_Var_Prime <- paramDb$LTD_sigma^2 / paramDb$cycleTime^2
  paramDb$ROP <- paramDb$LTD_Prime + paramDb$SL
  paramDb$si_prime <- paramDb$ROP - (paramDb$cycleTime - 1) * paramDb$EOQ

  FRDb <- paramDb %>%
    group_by(Title) %>%
    do({
      .

      # func <- getFunc(.$LTD_Prime, .$LTD_Var_Prime)
      ## Temp code, expand to consider additional distributions

      func <- function(x, mu = 1, var = 1) {
        1 / sqrt(2 * pi * var) * exp(-((x - mu)^2)/(2 * var))
      }
      expfunc <- function(x) {
        (x - .$si_prime) * func(x, mu = .$LTD_Prime, var = .$LTD_Var_Prime)
      }
      fr <- 1 - integrate(expfunc, .$si_prime, Inf)$value / .$EOQ
      tibble(FR = fr)
    })

  return (FRDb)
}

#' Takes as input PPV parameters.  Returns a data.frame suitable for plotting with \code{\link{plotST}}
#' Sets ROP to provide a zero safety level cycle
#' PPV is the procurement problem variable, the average required to meet the demand over the net leadtime
#' of an item with both a repairable pipeline (regenerative) and a procurement pipeline (attrition)
#'
#' @param Demand numeric, quarterly demand forecast
#' @param CRR numeric, proportion of items that are returned for repair
#' @param SR numeric, proportion of items that are successfully repaired
#' @param PCLT numeric, production lead time in quarters
#' @param PRTAT numeric, process repair turn around time in quarters
#'
#' @param Cov_Dur numeric, number of quarters of material to buy
#' @param SL numeric, minimum stock level
#' @param Repair_period number, number of quarters of material to repair
#'
#' @return data.frame
#' @export
ppvDecom <- function(SL = 0, Demand, CRR, SR, PCLT, PRTAT, Cov_Dur = 2, Repair_period = 1) {

  if (!between(CRR, 0, 1) || !between(SR, 0, 1) ||
        !Demand > 0 || !PCLT > 0 || !PRTAT > 0) {
    stop("CRR and SR must be [0,1] and all other parameters >= 0", call. = TRUE)
  }

  regenD <- Demand * CRR * SR
  attrD <- Demand * (1 - CRR * SR)

  paramDb <- tibble(
    Title = c("PPV", "Regen", "Attrition"),
    Dmd = c(Demand, regenD, attrD),
    LT = c(CRR * SR * PRTAT + (1 - CRR * SR) * PCLT, PRTAT, PCLT),
    EOQ = c(regenD * Repair_period + attrD * Cov_Dur, regenD * Repair_period, attrD * Cov_Dur)
    )
  paramDb$SL <- SL

  return (paramDb)
}

#' Creates a data.frame of parameters suitable for inventory analysis
#'
#' @param SL numeric, planned minimum safety level
#' @param EOQ numeric, Item order quantity
#' @param Dmd numeric, Item demand per unit time
#' @param LT numeric, lead time
#'
#' @return data.frame
#' @export
#' @import dplyr
getParamDb <- function(SL = NULL, EOQ = NULL, Dmd = NULL, LT = NULL, catNames) {

  params <- list(SL, EOQ, Dmd, LT)
  maxLength <- max(sapply(params, length))
  paramCheck <- sapply(params, function(p) {
    if (!length(p) %in% c(1, maxLength)) {
      return (FALSE)
    } else {
      return (TRUE)
    }
  })
  if (sum(paramCheck) < length(params)) stop("All params must be multiple of 1, max param length", call. = TRUE)
  if (is.null(catNames)) {
    catNames <- letters[1:maxLength]
  }
  paramDb <- tibble(SL = SL,
                    EOQ = EOQ,
                    Dmd = Dmd,
                    LT = LT,
                    Title = catNames)
  return (paramDb)
}

#' Produces a saw tooth continuous review inventory plot with user supplied parameters
#'
#' @param SL numeric, planned minimum safety level to honor or data.frame with at minimum
#' EOQ, Sl, Dmd, and LT
#' @param EOQ Item Order Quantity
#' @param Dmd numeric, demand per unit time
#' @param LT numeric, lead time
#' @param cycleLimits how many saw tooth cycles to draw
#' @param catNames names for each sawtooth
#' @param xMax if provided provide explicit maximum for x-axis
#' @param plotLTs if TRUE provide LT information
#' @param FR logical, if TRUE, estimate Fill Rate
#' @param ... additional parameters, for plot output
#' @return gg
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom scales comma
#' @examples
#' \dontrun{
#' plotST(ROP = 4, EOQ = 2, Dmd = 1, LT = 2, plotLTs = TRUE)
#' plotST(ROP = c(2,3,4), EOQ = c(1,2,3), Dmd = c(2,2,2), LT = 2, plotLTs = TRUE)
#' plotST(ROP = 4, EOQ = 1.5, Dmd = 1, LT = 2, xMax = 10, plotLTs = FALSE)
#' plotST(ROP = 4, EOQ = 4, Dmd = 1, LT = 2, cycleLimits = 2, plotLTs = TRUE)
#'
#' paramDb <- data.frame(ROP = c(2,3,4),
#'                       EOQ = c(1,2,3),
#'                       Dmd = c(4,2.5,2.2),
#'                       LT =  c(1,1,1),
#'                       Title = paste("Item", 1:3, sep = " "))
#' plotST(paramDb)
#'
#' }
#'
plotST <- function(SL, EOQ = NULL, Dmd = NULL, LT = NULL, cycleLimits = 2, catNames = NULL,
                   xMax = NULL, plotLTs = FALSE, FR = TRUE,  ...) {

  plotParams <- list(
    lSize = 1,
    axis_text = 12
  )
  dots <- list(...)
  if ("lSize" %in% names(dots)) plotParams$lSize <- dots$lSize
  if ("axis_text" %in% names(dots)) plotParams$axis_text <- dots$axis_text

  if (is.data.frame(SL)) {
    paramDb <- SL
    # Check for the right column names
    if (!sum(c("SL", "EOQ", "Dmd", "LT", "Title") %in% names(paramDb)) == 5) {
      stop("Data.frame must contain columns 'SL', 'EOQ', 'Dmd', 'LT', and 'Title'",
           call. = TRUE)
    }
  } else {
  # Some data checks, build the paramDb if not supplied
    paramDb <- getParamDb(SL, EOQ, Dmd, LT, catNames)
  }

  # Calculate some additional plot parameters
  paramDb$LTD <- paramDb$Dmd * paramDb$LT
  paramDb$cycleTime <- pmax(1, paramDb$LTD / paramDb$EOQ)
  paramDb$LTD_prime <- paramDb$LTD / paramDb$cycleTime
  paramDb$maxPoint <- paramDb$SL + paramDb$EOQ # Stock position right after resupply
  paramDb$minPoint <- paramDb$SL # Stock position right before resupply
  paramDb$ROP <- paramDb$SL + paramDb$LTD_prime

  # If missing xMax compute based on number of desired cycles
  if (missing(xMax)) {
    tstep <- paramDb$EOQ/paramDb$Dmd
    xMax <- max(tstep * cycleLimits, na.rm = TRUE)
    xlims <- range(c(0, xMax) * 1.01)
  } else {
    xlims <- c(0, xMax)
  }
  ylims <- range(c(min(min(paramDb$minPoint), 0),
                   (paramDb$ROP - paramDb$LTD_prime + paramDb$EOQ) * 1.1))

  if (!any(paramDb$EOQ > 0) || !any(paramDb$Dmd > 0)) {
    stop("Demand and Order Quantity Must be Positive")
  }

  pltDb <- paramDb %>%
    filter(Dmd > 0) %>%
    group_by(Title) %>%
    do({
      lineSegs <- tibble(x1 = 0, y1 = .$maxPoint, x2 = .$LTD_prime, y2 = .$minPoint)[-1,]
      time <- 0
      while(time < xlims[2] ) {
        if (nrow(lineSegs) == 0) resupply <- FALSE # initial condition
        if (resupply) {
          # resupply segment
          nextSeg <- tibble(x1 = time, y1 = .$minPoint, x2 = time, y2 = .$maxPoint)
          lineSegs <- bind_rows(lineSegs, nextSeg)
          resupply <- FALSE
        } else {
          tstep <- .$EOQ/.$Dmd
          if (time + tstep <= xlims[2]) {
            nextSeg <- tibble(x1 = time, y1 = .$maxPoint, x2 = time + tstep, y2 = .$minPoint)
            lineSegs <- bind_rows(lineSegs, nextSeg)
          }
          time <- time + tstep
          resupply = TRUE
        }
      }
      lineSegs
    })

  plt <- ggplot(pltDb) +
    scale_x_continuous(limits = xlims,
                       labels = scales::comma,
                       name = "Time",
                       expand = c(0, 0)) +
    scale_y_continuous(limits = ylims,
                       labels = scales::comma,
                       name = "Inventory Level")

  paramDb$SL = paramDb$ROP - paramDb$LTD_prime

  if (plotLTs && nrow(paramDb) == 1) {
    # only calcuate for single saw tooth, otherwise too messy
    # work backwords from the resupplies
    orderPeriods <-
      pltDb %>%
      rowwise() %>%
      do({
        if (.$x1[1] == .$x2[1]) {
          # resupply
          ret <- data.frame(
                     xstart = .$x1 - paramDb$LT[1],
                     xend = .$x1)
        } else {
          ret <- data.frame(xstart = NA, xend = NA)
        }
        ret
      }) %>%
      na.omit() %>%
      mutate(Order = as.character(row_number())) %>%
      rowwise() %>%
      mutate(xstart = max(0, xstart, na.rm = TRUE))

    resup_n <- nrow(orderPeriods)
    resupColors <- c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4",
                     "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")[1:min(8, resup_n)]
    names(resupColors) <- orderPeriods$Order
    plt <- plt + geom_rect(data = orderPeriods, alpha = .3,  col = "gray",
      aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Order),
      show.legend  = FALSE) +
      scale_fill_manual(values = resupColors)
  }

  paramDb$MRP_RO <- ifelse(paramDb$cycleTime > 1,
                           paramDb$SL + paramDb$LTD_prime * paramDb$cycleTime %% 1, ## Time to back up in cycle for resupplyn
                           paramDb$MRP_RO <-  paramDb$SL + paramDb$LTD_prime
                           )

  # add the rest of the elements
  lineCols <- c("dark red", "dark blue")
  names(lineCols) <- c("SL", "ROP")
  hlineData <- select(paramDb, Title, ROP=MRP_RO, SL) %>%
    tidyr::gather(., "Type", "y", -Title)

  plt <-
    plt +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(data = hlineData, aes(yintercept = y, color = Type, linetype = Title),
               lwd = plotParams$lSize) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = Title),
                 lwd = plotParams$lSize) +
    theme_light() +
    theme(
      axis.text.x = element_text(size = plotParams$axis_text),
      axis.text.y = element_text(size = plotParams$axis_text),
      axis.title.x = element_text(size = plotParams$axis_text),
      axis.title.y = element_text(size = plotParams$axis_text),
      plot.title = element_text(size = plotParams$axis_text),
      plot.subtitle = element_text(size = round(.8 * plotParams$axis_text, 0)),
      legend.text = element_text(size = round(.8 *plotParams$axis_text, 0)),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = lineCols)

  ## Add a title
  subTitle <-
    paramDb %>%
    rowwise() %>%
    mutate(OverlapYesNo = ifelse(cycleTime > 1, "Yes", "No")) %>%
    do({
      if (nrow(paramDb) > 1) {
        lab <- sprintf("Title: %s, Demand: %s, Lead Time: %s, EOQ: %s, ROP: %s, SL: %s, Overlapping Orders Required: %s",
                       .$Title, round(.$Dmd,1), round(.$LT,1), round(.$EOQ,1),
                       round(.$MRP_RO,1), round(.$SL,1), .$OverlapYesNo)
      } else {
        lab <- sprintf("Demand: %s, Lead Time: %s, EOQ: %s, ROP: %s, SL: %s, Overlapping Orders Required: %s",
                       round(.$Dmd,1), round(.$LT,1), round(.$EOQ,1), round(.$MRP_RO,1),
                       round(.$SL,1), .$OverlapYesNo)
      }
      tibble(lab = lab)
    }) %>%
    unlist() %>%
    paste(., collapse = "\n")

  ## Create an appeasing title
  if (plotLTs && nrow(paramDb) == 1) {
    subTitle <- paste0(subTitle, "\nShaded areas denote periods when orders are outstanding")
  }
  plt <- plt +
    labs(title = "Inventory Saw Tooth",
         subtitle = subTitle)

  # Return in attributes some information about parameters
  attr(plt, "plot_data") <- pltDb
  attr(plt, "param_data") <- paramDb

  return (plt)
}
