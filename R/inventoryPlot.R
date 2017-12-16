if (FALSE) {

  plotST(ROP = 4, EOQ = 2, Dmd = 1, LT = 2, plotLTs = TRUE)
  plotST(ROP = c(2,3,4), EOQ = c(1,2,3), Dmd = c(2,2,2), LT = 2, plotLTs = TRUE)
  plotST(ROP = 4, EOQ = 1.5, Dmd = 1, LT = 2, xMax = 10, plotLTs = TRUE)
  plotST(ROP = 4, EOQ = 4, Dmd = 1, LT = 2, cycleLimits = 2, plotLTs = TRUE)

  paramDb <- data.frame(ROP = c(2,3,4),
                        EOQ = c(1,2,3),
                        Dmd = c(4,2.4,2.5),
                        LT =  c(1,1,1),
                        Title = paste("Item", 1:3, sep = " "))
  plotST(paramDb)

  plotST(ROP = 4, EOQ = 2, Dmd = 1, LT = 2, plotLTs = TRUE, lSize = 1, axis_text = 12)

  Demand = 4; CRR = .7; SR = .8; PCLT = 3; PRTAT = 1.5
  plotST(ppvDecom(Demand = Demand, CRR = CRR, SR = SR, PCLT = PCLT, PRTAT = PRTAT))
}

##' Using approach in Silver, et. al, estimate fill rate of CR system
##'
##' @param paramDb data.frame containing Demand, LT, EOQ, ROP
##' @export
estimateFR <- function(paramDb) {

  return (NULL)

}

#' Takes as input PPV parameters.  Returns a data.frame suitable for plotting with \link{\code{plotST}}
#' Sets ROP to provide a zero safety level cycle
#' PPV is the procurement problem variable, the average required to meet the demand over the net leadtime
#' of an item with both a repairable pipeline (regenerative) and a procurement pipeline (attrition)
#' @param Demand numeric, quarterly demand forecast
#' @param CRR numeric, proportion of items that are returned for repair
#' @param SR numeric, proportion of items that are successfully repaired
#' @param PCLT numeric, production lead time in quarters
#' @param PRTAT numeric, process repair turn around time in quarters
#'
#' @param Cov_Dur numeric, number of quarters of material to buy
#' @param Repair_period number, number of quarters of material to repair
#' @param CR numeric, proportion of items that are returned for repair
#' @return data.frame
#' @export
#'
ppvDecom <- function(Demand, CRR, SR, PCLT, PRTAT, Cov_Dur = 2, Repair_period = 1) {

  if (!between(CRR, 0, 1) || !between(SR, 0, 1) ||
        !Demand > 0 || !PCLT > 0 || !PRTAT > 0) {
    error("CRR and SR must be [0,1] and all other parameters >= 0")
  }

  regenD <- Demand * CRR * SR
  attrD <- Demand * (1 - CRR * SR)

  paramDb <- tibble(
    Title = c("PPV", "Regen", "Attrition"),
    Dmd = c(Demand, regenD, attrD),
    LT = c(CRR * SR * PRTAT + (1 - CRR * SR) * PCLT, PRTAT, PCLT),
    EOQ = c(regenD * Repair_period + attrD * Cov_Dur, regenD * Repair_period, attrD * Cov_Dur)
    )
  LTD_prime <- (paramDb$Dmd * paramDb$LT) / (pmax(1, (paramDb$Dmd * paramDb$LT)/paramDb$EOQ))
  paramDb$ROP <- LTD_prime

  return (paramDb)
}

#' Produces a saw tooth continuous review inventory plot with user supplied parameters
#'
#' @param ROP Item Reorder Point or data.frame containing (ROP, EOQ, Dmd, LT, and catNames)
#' @param EOQ Item Order Quantity
#' @param cycleLimits how many saw tooth cycles to draw
#' @param xMax if provided provide explicit maximum for x-axis
#' @param plotLTs if TRUE provide LT information
#' @param ... additional parameters, for plot output
#' @param catNames names for each sawtooth
#'
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
plotST <- function(ROP, EOQ = NULL, Dmd = NULL, LT = NULL, cycleLimits = 2, catNames = NULL,
                   xMax = NULL, plotLTs = FALSE, ...) {

  plotParams <- list(
    lSize = 1,
    axis_text = 12
  )
  dots <- list(...)
  if ("lSize" %in% names(dots)) plotParams$lSize <- dots$lSize
  if ("axis_text" %in% names(dots)) plotParams$axis_text <- dots$axis_text

  if (is.data.frame(ROP)) {
    paramDb <- ROP
  } else {
  # Some data checks, build the paramsDb if not supplied
    params <- list(ROP, EOQ, Dmd, LT)
    maxLength <- max(sapply(params, length))
    paramCheck <- sapply(params, function(p) {
      if (!length(p) %in% c(1, maxLength)) {
        return (FALSE)
      } else {
        return (TRUE)
      }
    })
    if (sum(paramCheck) < length(params)) error ("All params must be multiple of 1, max param length")
    if (missing(catNames)) {
      catNames <- letters[1:maxLength]
    }
    paramDb <- tibble(ROP = ROP,
                      EOQ = EOQ,
                      Dmd = Dmd,
                      LT = LT,
                      Title = catNames)
  }
  # Calculate some additional plot parameters
  paramDb$LTD <- paramDb$Dmd * paramDb$LT
  cycleTime <- pmax(1, paramDb$LTD / paramDb$EOQ)
  paramDb$LTD_prime <- paramDb$LTD / cycleTime
  paramDb$maxPoint <- paramDb$ROP - paramDb$LTD_prime + paramDb$EOQ # Stock position right after resupply
  paramDb$minPoint <- paramDb$ROP - paramDb$LTD_prime # Stock position right before resupply

  # If missing xMax compute based on number of desired cycles
  if (missing(xMax)) {
    tstep <- -(paramDb$minPoint - paramDb$maxPoint)/paramDb$Dmd
    xMax <- max(tstep * cycleLimits)
    xlims <- range(c(0, xMax) * 1.01)
  } else {
    xlims <- c(0, xMax)
  }
  ylims <- range(c(0, paramDb$ROP - paramDb$LTD_prime + paramDb$EOQ) * 1.1)

  pltDb <- paramDb %>%
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
          tstep <- -(.$minPoint - .$maxPoint)/.$Dmd
          # Determine next point of resupply, where slope = -dmd
          if (time + tstep < xlims[2]) {
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
                     xstart = .$x1 - paramDb$LTD[1],
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

  # add the rest of the elements
  lineCols <- c("dark red", "dark blue")
  names(lineCols) <- c("SL", "ROP")
  hlineData <- select(paramDb, Title, ROP, SL) %>%
    tidyr::gather(., "Type", "y", -Title)

  plt <-
    plt +
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
      legend.title = element_blank(),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = lineCols)

  ## Add a title
  subTitle <-
    paramDb %>%
    rowwise() %>%
    do({
      if (nrow(paramDb) > 1) {
        lab <- sprintf("Title: %s, Demand: %s, Lead Time: %s, EOQ: %s, ROP: %s, SL: %s",
                       .$Title, round(.$Dmd,1), round(.$LT,1), round(.$EOQ,1), round(.$ROP,1), round(.$SL,1))
      } else {
        lab <- sprintf("Demand: %s, Lead Time: %s, EOQ: %s, ROP: %s, SL: %s",
                       round(.$Dmd,1), round(.$LT,1), round(.$EOQ,1), round(.$ROP,1), round(.$SL,1))
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
