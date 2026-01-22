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
#' @param befilab A logical value. If FALSE, the label for smother and best fit dissapear.
#' @param addi A logical value. If TRUE, adds a confidence interval to the best-fit line.
#' @param mti An optional character string for the main plot title.
#' @param msti An optional character string for the plot subtitle.
#' @param lti An optional character string for the legend title.
#' @param lpo A character string for the legend position. Accepts standard ggplot2 legend positions.
#' @param ol_yn A logical value. If TRUE, adds additional custom mathematical lines.
#' @param ol_fun The transformation for custom lines: "Identity", "Logarithm", "Exponential", or "Logistic".
#' @param ol_eq A character vector of linear equations possibly separated by ;.
#' @param ol_s A numeric vector of slopes (or rates) for the custom lines.
#' @param ol_l A character vector of labels for the custom lines (appears in the legend).
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
#' 
#' scatterplot____plot(
#'   data = mtcars,
#'   xxx = "hp",
#'   yyy = "mpg",
#'   logt = TRUE,
#'   befi = "reg",
#'   befilab = FALSE,
#'   fosi = "fl"
#' )
#'
#' scatterplot____plot(
#'   data = mtcars,
#'   xxx = "wt",
#'   yyy = "mpg",
#'   logt = FALSE,
#'   befi = "none",
#'   befilab = FALSE,
#'   fosi = "fl",
#'   ol_yn = TRUE,
#'   ol_fun = "Identity",
#'   ol_eq = "30 - 5.3x^2; 45-5.3x; 50-5.3x",
#'   ol_l = "Models",
#'   posi = "ps",
#'   lisi = "ls"
#' )
#' 
scatterplot____plot <- function(data, xxx, yyy, ccc = NULL, ppp = NULL, imis = FALSE, mist = "NA",
                             yax = NULL, xax = NULL, logt = FALSE, fosi = "fm", posi = "pm",
                             lisi = "lm", fli = FALSE, saxy = FALSE, lieq = FALSE,
                             befi = "none",befilab=TRUE, addi = FALSE, mti = NULL, msti = NULL,
                             lti = NULL, lpo = "right",ol_yn = FALSE,ol_fun = "Identity",ol_eq=NULL,ol_l=NULL) {
  

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
        geom_smooth(method = "lm", se = addintervals, color = "blue",linewidth = linesi)
      if (befilab) {
        fp <- fp +
          annotate("label", 
                   x = mean(plotData[[xxx]], na.rm = TRUE), 
                   y = mean(plotData[[yyy]], na.rm = TRUE), 
                   label = "Regression line", 
                   fontface = "bold", 
                   size = 5, color = "white",fill = "blue") 
      }

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
        geom_smooth(method = "loess", se = addintervals, color = "red",linewidth = linesi,inherit.aes = TRUE)
      
      if (befilab) {
        fp <- fp +
          annotate("label", 
                   x = mean(plotData[[xxx]], na.rm = TRUE), 
                   y = mean(plotData[[yyy]], na.rm = TRUE), 
                   label = "Smother", 
                   fontface = "bold",
                   size = 5, color = "white",fill = "red") 
      }

    }
    
    
  }
  
  
  #+++++++++++++++++++
  #++ Add other lines
  #+++++++++++++++++++
  if (ol_yn & !ol_eq=="") {
    # 1. Split the string by the semicolon
    raw_expressions <- strsplit(ol_eq, ";")[[1]]
    
    # 2. Clean and convert to R-readable syntax
    # This regex finds a number followed by 'x' and inserts a '*'
    cleaned_expressions <- gsub("([0-9])x", "\\1*x", raw_expressions)
    cleaned_expressions <- stringr::str_trim(cleaned_expressions)
    
    # Function to check if an expression is valid
    is_valid_expr <- function(expr_text) {
      tryCatch({
        # 1. Parse the text to check for syntax errors (e.g., "5 + * 2")
        parsed <- parse(text = expr_text)
        
        # 2. Evaluate with a dummy x to check for logic errors (e.g., undefined variables)
        result <- eval(parsed, list(x = 1))
        
        # 3. Check if the result is actually a number
        return(is.numeric(result))
        
      }, error = function(e) {
        # If any error occurs during parse or eval, return FALSE
        return(FALSE)
      })
    }
    
    # Apply the check to your vector
    validity_results <- sapply(cleaned_expressions, is_valid_expr)
    
    if (all(validity_results)) {
      # 3. Create a list of functions
      function_list <- lapply(cleaned_expressions, function(expr_text) {
        if (ol_fun=="Identity") eval(parse(text = paste("out <- function(x) {",expr_text,"}",sep="")))
        if (ol_fun=="Logarithm") eval(parse(text = paste("out <- function(x) {log(",expr_text,")}",sep="")))
        if (ol_fun=="Exponential") eval(parse(text = paste("out <- function(x) {exp(",expr_text,")}",sep="")))
        if (ol_fun=="Logistic") eval(parse(text = paste("out <- function(x) {exp(",expr_text,") / (1 + exp(",expr_text,"))}",sep="")))
        out
      })
      
      temp <- strsplit(ol_l, ";")[[1]]
      if (length(temp)==0) temp=""
      nombre <- stringr::str_trim(temp)
      ddff <- data.frame(id = 1:length(function_list),
                         nombre = nombre)
      name <- ddff$nombre
      
      mix <- min(plotData[[xxx]], na.rm = TRUE)
      maax <- max(plotData[[xxx]], na.rm = TRUE)
      miy <- min(plotData[[yyy]], na.rm = TRUE)
      maay <- max(plotData[[yyy]], na.rm = TRUE)
      
      # Define the x position for the labels (at the right edge of the plot)
      label_x <- maax 
      
      # 1. Define a pool of available linetypes
      possible_types <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
      possible_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF")
      
      # 2. Pre-generate a random selection for the number of lines you have
      # We use replace = TRUE in case you have more than 6 lines
      set.seed(42) # Optional: keeps the 'random' choice the same every time you run it
      num_funcs <- length(function_list)
      random_linetypes <- sample(possible_types, num_funcs, replace = TRUE)
      random_colors    <- sample(possible_colors, num_funcs, replace = TRUE)
      
      fp <- fp + lapply(seq_along(function_list), function(i) {
        
        # 1. Calculate the Y position for the label based on the function
        # We evaluate the function at the far right x-axis point
        label_y <- function_list[[i]](label_x)
        current_col <- random_colors[i] # Pick the color for this iteration
        
        # 2. Return both the line and the annotation
        list(
          stat_function(
            fun = function_list[[i]],
            size = 1,
            # Assign the pre-generated random linetype here
            linetype = random_linetypes[i],
            linewidth = linesi,
            # Remove linetype from aes() so it doesn't create a legend
            color = current_col 
          ),
          annotate(
            "label",
            x = label_x,
            y = label_y,
            label = name[i],
            hjust = 1,      # Aligns label to the left of the x-coordinate
            vjust = 0,      # Adjusts vertical position slightly
            size = linesi+3,
            fill = "white", # Makes label readable over grid lines
            alpha = 0.8
          )
        )
      }) + 
        # Remove the legend title since we aren't using the linetype aesthetic anymore
        labs(linetype = NULL) +
        coord_cartesian(xlim = c(mix, maax))
    } else {cat("\n\nInvalid math expressions found. Skipping custom lines.\n\n")}
    

    
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