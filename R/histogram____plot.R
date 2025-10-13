#' @title Histogram Plotting Function Mimicking Jamovi Inputs
#'
#' @description Creates one or more histograms (or faceted/grouped histograms) 
#'   using ggplot2, based on the structure of inputs found in jamovi modules.
#'   It includes options for grouping, faceting, log-transformation, and
#'   handling missing data.
#'
#' @param data A data frame containing the variables specified in the options.
#' @param yyy A character string specifying the name of the continuous variable 
#'   to be plotted on the x-axis (e.g., 'outcome'). This is the main variable.
#' @param xxx A character string specifying the name of the optional categorical 
#'   variable used to group the histograms (e.g., 'group'). If provided, separate
#'   histograms are overlaid or presented side-by-side (using 'fill').
#' @param ppp A character string specifying the name of the optional categorical 
#'   variable used for faceting (paneling) the plot (e.g., 'panel').
#' @param imis Logical. If TRUE, missing values in 'xxx' and 'ppp' are treated 
#'   as a separate category (labeled by 'mist'). If FALSE, rows with missing 
#'   values in 'yyy', 'xxx', or 'ppp' are removed (default: FALSE).
#' @param mist A character string specifying the label to use for missing values 
#'   if 'imis' is TRUE (default: 'MISSING').
#' @param logt Logical. If TRUE, the 'yyy' variable is log-transformed, and 
#'   non-positive values are filtered out before plotting (default: FALSE).
#' @param fosi A character string specifying the font size: 'fs' (small, 15), 
#'   'fm' (medium, 20), or 'fl' (large, 25) (default: 'fm').
#' @param lti A character string for the legend title (default: NULL, uses 'xxx' name).
#' @param xax A character string for the X-axis label (default: NULL, uses 'yyy' name).
#' @param mti A character string for the main plot title (default: NULL).
#' @param msti A character string for the plot subtitle (default: NULL).
#' @param lpo A character string for the legend position (e.g., 'right', 'none', 'bottom') 
#'   (default: 'right').
#' @param alpha_hist A numeric value specifying the transparency of the histogram 
#'   bars (0 to 1) (default: 0.7).
#'
#' @return A ggplot object that displays the histogram(s).
#' @export
#'
#' @import ggplot2 dplyr
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' set.seed(42)
#' data_sim <- data.frame(
#'   Value = rnorm(200, mean = 10, sd = 2),
#'   Group = factor(sample(c("A", "B", "C"), 200, replace = TRUE)),
#'   Panel = factor(sample(c("X", "Y"), 200, replace = TRUE))
#' )
#' 
#' # Simple Histogram
#' histogram____plot(data = data_sim, yyy = "Value")
#' 
#' # Grouped Histograms
#' histogram____plot(data = data_sim, yyy = "Value", xxx = "Group", xax = "Measured Value")
#' 
#' # Faceted Histograms
#' histogram____plot(data = data_sim, yyy = "Value", xxx = "Group", ppp = "Panel", fosi = "fl")
#' 
#' # Log-transformed
#' data_sim$Value_Pos <- abs(data_sim$Value) # ensure positive values
#' histogram____plot(data = data_sim, yyy = "Value_Pos", logt = TRUE)
#' }
histogram____plot <- function(data, yyy, xxx = NULL, ppp = NULL, imis = FALSE, 
                           mist = "(Unknown)", logt = FALSE, fosi = "fm", 
                           lti = NULL, xax = NULL, mti = NULL, 
                           msti = NULL, lpo = "right", 
                           alpha_hist = 0.7) {
  

  # 1. Prepare Data
  vars_to_select <- c(yyy, xxx, ppp)
  vars_to_select <- vars_to_select[!is.null(vars_to_select)]
  tmpDat <- data %>% dplyr::select(dplyr::all_of(vars_to_select))
  
  # Handle missing data based on imis option
  if (!imis) {
    tmpDat <- tmpDat %>% na.omit()
  } else {
    if (!is.null(xxx)) tmpDat[[xxx]] <- mis_label_function(tmpDat[[xxx]], mist)
    if (!is.null(ppp)) tmpDat[[ppp]] <- mis_label_function(tmpDat[[ppp]], mist)
  }
  
  # Handle Log Transformation (logt)
  xlabe <- yyy
  if (!is.null(xax)) xlabe <- xax
  
  if (logt) {
    xlabe <- paste("log (", xlabe, ")", sep = "")
    tmpDat <- tmpDat %>% dplyr::filter(.data[[yyy]] > 0)
    # Note: In jamovi, a message would appear. We skip this for a standalone function.
  }
  
  # 2. Set Plot Aesthetics and Labels
  
  # Font size mapping
  fontsi <- dplyr::case_when(fosi == "fs" ~ 15,
                             fosi == "fm" ~ 20,
                             fosi == "fl" ~ 25,
                             TRUE ~ 20) # Default to medium if not specified
  
  # Set legend title
  if (is.null(lti) && !is.null(xxx)) lti <- xxx
  
  # Set Y-axis label
  #ylabe <- "Count" # Default for histogram
  #if (!is.null(yax)) ylabe <- yax
  
  # 3. Create Base Plot
  
  # Default X and Y mappings
  aes_mapping <- ggplot2::aes(x = .data[[yyy]])
  
  # If a grouping variable (xxx) is provided
  if (!is.null(xxx)) {
    # 'fill' is used to group the histograms
    aes_mapping <- ggplot2::aes(x = .data[[yyy]], fill = .data[[xxx]])
  }
  
  pt <- ggplot2::ggplot(tmpDat, aes_mapping)
  
  # Add geom_histogram
  pt <- pt + ggplot2::geom_histogram(
    aes(y = after_stat(count / sum(count))),
    alpha = alpha_hist,
    position = "identity", # Use 'identity' or 'dodge' for grouped plots
    bins = 16, color = "white"
  )+
    scale_y_continuous(labels = scales::percent)
  
  # 4. Apply Customizations
  
  # Add titles and labels
  pt <- pt + 
    ggplot2::labs(x = xlabe, y = NULL)
  
  # Handle grouping legend title
  if (!is.null(xxx) && !is.null(lti)) {
    pt <- pt + ggplot2::scale_fill_discrete(name = lti)
  }
  
  # Add Faceting (Paneling)
  if (!is.null(ppp)) {
    pt <- pt + ggplot2::facet_wrap(~.data[[ppp]],ncol = 1)
  }
  
  # Log transformation on x-axis scale
  if (logt) {
    pt <- pt + ggplot2::scale_x_continuous(trans = "log10")
  }
  
  # Theme and Titles
  fp <- pt + 
    ggplot2::theme_classic(base_size = fontsi) +
    ggplot2::theme(
      text = ggplot2::element_text(size = fontsi),
      legend.position = lpo,
      # Ensure legend key uses the specified alpha
      legend.key = ggplot2::element_rect(fill = NA, colour = NA) 
    )
  
  if (!is.null(mti)) {
    fp <- fp + 
      ggplot2::ggtitle(mti, subtitle = msti) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5)
      )
  }
  
  return(fp)
}