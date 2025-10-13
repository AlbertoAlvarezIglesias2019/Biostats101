#' @title Calculate and Format a Normal Tolerance Interval
#' @description This function calculates a tolerance interval for a single variable
#'   assumed to be from a normal distribution. It returns the result in a styled
#'   HTML table and provides the necessary parameters to generate a corresponding
#'   plot.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the numeric variable
#'   in the data frame.
#' @param conf_tolerance A numeric value between 0 and 1, representing the confidence
#'   level of the interval.
#' @param poco_tolerance A numeric value between 0 and 1, representing the
#'   proportion of the population to be covered by the interval.
#' @param side_tolerance A numeric value, either 1 for a one-sided interval or
#'   2 for a two-sided interval.
#' @param plotype_tolerance A character string specifying the type of plot to
#'   generate (e.g., "qq", "pp", "histogram", "boxplot").
#' @param nd The number of decimal places to round the interval to.
#' @param font_size The font size for the output HTML table.
#' @return A list containing two elements:
#'   \item{table}{A \code{kableExtra} HTML table object showing the tolerance interval.}
#'   \item{plot}{A list of parameters for a plot, typically for a Jamovi backend to render.}
#' @details The function relies on the \code{tolerance} package to calculate the interval
#'   based on a normally distributed sample. It handles missing values by removing them
#'   before the calculation.
#' @note This function is designed to be part of a larger analysis, likely a
#'   Jamovi module, and does not produce a standalone plot. Instead, it passes
#'   plot parameters for external rendering.
#' @seealso \code{\link[tolerance]{normtol.int}}, \code{\link[tolerance]{plottol}},
#'   \code{\link[kableExtra]{kableExtra}}
#' @importFrom tolerance normtol.int
#' @importFrom dplyr if_else
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' # Create a sample dataset from a normal distribution
#' set.seed(456)
#' sample_data <- tibble::tibble(
#'   my_var = rnorm(100, mean = 25, sd = 5)
#' )
#'
#' # Example 1: Basic two-sided tolerance interval
#' onemean____tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.95,
#'   poco_tolerance = 0.99,
#'   side_tolerance = 2,
#'   plotype_tolerance = "both",
#'   nd = 2,
#'   font_size = 12
#' )$table
#'
#' # Example 2: One-sided interval with different confidence and proportion
#' onemean____tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.99,
#'   poco_tolerance = 0.95,
#'   side_tolerance = 1,
#'   plotype_tolerance = "hist",
#'   nd = 3,
#'   font_size = 14
#' )$table
#' 
#' # Example 3: One-sided interval with different confidence and proportion (plot)
#' tp <- onemean____tolerance(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_tolerance = 0.99,
#'   poco_tolerance = 0.95,
#'   side_tolerance = 1,
#'   plotype_tolerance = "both",
#'   nd = 3,
#'   font_size = 14
#' )$plot
#' 
#' tolerance::plottol(tol.out=tp$tol.out,x=tp$x,plot.type=tp$plot.type,y.lab=tp$y.lab)
#'           


onemean____tolerance <- function(data, variable, conf_tolerance, poco_tolerance, side_tolerance, plotype_tolerance, nd, font_size) {
  
  # --- 1. PREPARE DATA AND CALCULATE TOLERANCE INTERVAL ---
  
  # Extract the variable, ensuring a clean vector without NAs.
  var_data <- data[[variable]]
  var_data <- var_data[!is.na(var_data)]
  
  # Calculate the tolerance interval using the 'tolerance' package.
  # Note: `alpha` is 1 - confidence level.
  fit <- tolerance::normtol.int(
    x = var_data,
    alpha = 1 - conf_tolerance,
    P = poco_tolerance,
    side = as.numeric(side_tolerance)
  )
  
  # Extract the interval bounds and format them into a single string.
  ttt1 <- round(fit[, 4], nd)
  ttt2 <- round(fit[, 5], nd)
  ti <- paste("(", ttt1, ",", ttt2, ")", sep = "")
  
  # --- 2. BUILD THE TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(Ci = ti)
  row.names(dframe) <- NULL
  
  # Create the table header string with HTML formatting.
  col_headers_html <- paste(conf_tolerance * 100, "% Tolerance Interval<sup>1</sup>", sep = "")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Tolerance Interval</p>",
    sep = ""
  )
  
  # Create the HTML strings for footnotes based on the parameters.
  fn1 <- paste("Proportion Covered = ", poco_tolerance, sep = "")
  fn2 <- dplyr::if_else(side_tolerance == 2, "2-sided", "1-sided")
  fn <- paste(fn1, fn2, sep = "; ")
  fn <- paste("<i>", fn, "<i>", sep = "")
  footnotes_html <- fn
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  table_out <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) %>%
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) %>%
    # Add borders and spacing to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  # --- 4. RETURN TABLE AND PLOT PARAMETERS ---
  
  # The plot is returned as a list of parameters for a Jamovi backend to render.
  list(table = table_out, plot = list(tol.out = fit, x = var_data, plot.type = plotype_tolerance, y.lab = variable))
}