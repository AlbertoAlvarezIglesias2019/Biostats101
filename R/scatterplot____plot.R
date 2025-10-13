#' @title Generate a Scatter Plot with Multiple Customization Options
#'
#' @description This function generates a customizable scatter plot based on the
#'   logic from a Jamovi module. It handles various plot options such as color
#'   mapping, faceting, transformations, best-fit lines, and plot aesthetics.
#'
#' @param data A data frame containing the variables to plot.
#' @param xxx A character string specifying the name of the variable to be plotted on the x-axis.
#' @param yyy A character string specifying the name of the variable to be plotted on the y-axis.
#' @param ccc An optional character string specifying the name of the variable to use for point color.
#' @param ppp An optional character string specifying the name of the variable to use for faceting the plot.
#' @param imis A logical value. If TRUE, missing values are labeled; otherwise, they are omitted.
#' @param mist A character string specifying the label for missing values.
#' @param yax An optional character string for the y-axis label. If NULL, `yyy` is used.
#' @param xax An optional character string for the x-axis label. If NULL, `xxx` is used.
#' @param logt A logical value. If TRUE, the y-axis is log-transformed.
#' @param fosi A character string controlling the font size. Must be one of "fs" (small), "fm" (medium), or "fl" (large).
#' @param posi A character string controlling the point size. Must be one of "ps" (small), "pm" (medium), or "pl" (large).
#' @param lisi A character string controlling the line size. Must be one of "ls" (small), "lm" (medium), or "ll" (large).
#' @param fli A logical value. If TRUE, the plot axes are flipped.
#' @param saxy A logical value. If TRUE, the x and y axes are set to the same scale.
#' @param lieq A logical value. If TRUE, a line of equality (y=x) is added to the plot.
#' @param befi A character string for the best-fit line. Must be "none", "reg" (linear model), or "smo" (smooth loess).
#' @param addi A logical value. If TRUE, adds a confidence interval to the best-fit line.
#' @param mti An optional character string for the main plot title.
#' @param msti An optional character string for the plot subtitle.
#' @param lti An optional character string for the legend title.
#' @param lpo A character string for the legend position. Accepts standard ggplot2 legend positions.
#'
#' @return A `ggplot` object, which can be printed or saved.
#'
#' @details This function is a standalone version of a Jamovi module and
#'   reproduces its plot functionality. It uses `ggplot2` for all plotting.
#'   The function includes several conditional statements to handle the various
#'   plot options provided as arguments.
#'
#' @examples
#' # Use the built-in mtcars dataset
#' data(mtcars)
#'
#' # A basic scatter plot of mpg vs. wt
#' scatterplot____plot(
#'   data = mtcars,
#'   xxx = "wt",
#'   yyy = "mpg"
#' )
#'
#' # A more complex plot with color, linear regression, and custom labels
#' scatterplot____plot(
#'   data = mtcars,
#'   xxx = "wt",
#'   yyy = "mpg",
#'   ccc = "cyl",
#'   befi = "reg",
#'   addi = TRUE,
#'   mti = "MPG vs. Weight",
#'   lti = "Cylinders"
#' )
#'
#' # A plot with a log-transformed y-axis and a smooth best-fit line
#' scatterplot____plot(
#'   data = mtcars,
#'   xxx = "hp",
#'   yyy = "mpg",
#'   logt = TRUE,
#'   befi = "smo",
#'   fosi = "fl"
#' )
scatterplot____plot <- function(data, xxx, yyy, ccc = NULL, ppp = NULL, imis = FALSE, mist = "NA",
                             yax = NULL, xax = NULL, logt = FALSE, fosi = "fm", posi = "pm",
                             lisi = "lm", fli = FALSE, saxy = FALSE, lieq = FALSE,
                             befi = "none", addi = FALSE, mti = NULL, msti = NULL,
                             lti = NULL, lpo = "right") {
  

  # Input validation
  if (is.null(yyy) || is.null(xxx)) {
    message("Cannot create plot: 'xxx' and 'yyy' must be specified.")
    return(NULL)
  }
  
  # Prepare data
  plotData <- data
  if (!imis) plotData <- plotData %>% na.omit()
  if (imis && !is.null(ccc)) plotData[[ccc]] <- mis_label_function(plotData[[ccc]], mist)
  if (imis && !is.null(ppp)) plotData[[ppp]] <- mis_label_function(plotData[[ppp]], mist)
  
  # Map aesthetic options
  fontsi <- dplyr::case_when(fosi == "fs" ~ 15, fosi == "fm" ~ 20, fosi == "fl" ~ 25)
  pointsi <- dplyr::case_when(posi == "ps" ~ 1, posi == "pm" ~ 4, posi == "pl" ~ 7)
  linesi <- dplyr::case_when(lisi == "ls" ~ 0.5, lisi == "lm" ~ 2, lisi == "ll" ~ 4)
  
  # Build the base ggplot object
  p <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[xxx]], y = .data[[yyy]])) +
    ggplot2::geom_point(size = pointsi)
  
  # Add color if specified
  if (!is.null(ccc)) {
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[[xxx]], y = .data[[yyy]], colour = .data[[ccc]])) +
      ggplot2::geom_point(size = pointsi)
  }
  
  fp <- p
  
  # Add log transformation to y-axis
  if (logt) {
    fp <- fp + ggplot2::scale_y_continuous(trans = "log")
    if (is.null(yax)) ylabe <- paste("log (", yyy, ")", sep = "") else ylabe <- paste("log (", yax, ")", sep = "")
  } else {
    if (is.null(yax)) ylabe <- yyy else ylabe <- yax
  }
  
  # Add other plot elements
  if (fli) fp <- fp + ggplot2::coord_flip()
  
  if (saxy) {
    mini <- min(plotData[[yyy]], plotData[[xxx]], na.rm = TRUE)
    maxi <- max(plotData[[yyy]], plotData[[xxx]], na.rm = TRUE)
    fp <- fp + ggplot2::expand_limits(x = c(mini, maxi), y = c(mini, maxi))
  }
  
  if (lieq) {
    fp <- fp +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", linewidth = linesi) +
      ggplot2::annotate("label", x = Inf, y = Inf, label = "Line of equality",
                        hjust = 1.1, vjust = 1.2, size = 5, color = "black",
                        fill = "white", alpha = 0.8, label.size = 0)
  }
  
  #+++++++++++++++++++++++
  #++ Best fit regression
  #+++++++++++++++++++++++
  if (befi=="reg") {
    addintervals <- if_else(addi,TRUE,FALSE)
    
    
    if (!is.null(ccc)) {
      fp <- fp + 
        geom_smooth(method = "lm", se = addintervals,linewidth = linesi)
    }
    
    
    if (is.null(ccc)) {
      fp <- fp + 
        geom_smooth(method = "lm", se = addintervals, color = "blue",linewidth = linesi) +
        annotate("label", 
                 x = mean(plotData[[xxx]], na.rm = TRUE), 
                 y = mean(plotData[[yyy]], na.rm = TRUE), 
                 label = "Regression line", 
                 fontface = "bold", 
                 size = 5, color = "white",fill = "blue") 
    }
    
  }
  
  #+++++++++++++++++++++++
  #++ Best fit Smooth
  #+++++++++++++++++++++++
  if (befi=="smo") {
    addintervals <- if_else(addi,TRUE,FALSE)
    
    if (!is.null(ccc)) {
      fp <- fp + 
        geom_smooth(method = "loess", se = addintervals,linewidth = linesi)
    }
    
    if (is.null(ccc)) {
      fp <- fp + 
        geom_smooth(method = "loess", se = addintervals, color = "red",linewidth = linesi,inherit.aes = TRUE) +
        annotate("label", 
                 x = mean(plotData[[xxx]], na.rm = TRUE), 
                 y = mean(plotData[[yyy]], na.rm = TRUE), 
                 label = "Smother", 
                 fontface = "bold",
                 size = 5, color = "white",fill = "red") 
    }
    
    
  }
  
  # Set labels and themes
  if (!is.null(mti)) fp <- fp + ggplot2::ggtitle(mti, subtitle = msti) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5))
  
  if (!is.null(xax)) fp <- fp + ggplot2::xlab(xax)
  fp <- fp + ggplot2::ylab(ylabe)
  
  if (!is.null(lti)) fp <- fp + ggplot2::labs(color = lti)
  
  fp <- fp + ggplot2::theme(text = ggplot2::element_text(size = fontsi), legend.position = lpo)
  
  # Add facets
  if (!is.null(yyy) && !is.null(ppp)) {
    fp <- fp + ggplot2::facet_wrap(ppp, labeller = ggplot2::label_both)
  }
  
  return(fp)
}