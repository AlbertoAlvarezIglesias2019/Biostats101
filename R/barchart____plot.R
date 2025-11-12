#' @title Create a Bar Chart for Categorical Data (Counts or Proportions)
#'
#' @description Generates a standard or grouped bar chart using \code{ggplot2}. This function
#'   handles plotting counts or percentages, stacking or dodging bars, incorporating
#'   grouping variables, and managing missing values and labels.
#'
#' @param data A data frame containing the data to be plotted. It should be in the
#'   'expanded' or 'uncounted' format, where each row represents a single observation.
#' @param yyy The name of the categorical variable whose counts/proportions form the
#'   \strong{fill} aesthetic (the variable being counted/proportioned). Must be a string.
#' @param xxx The name of the categorical variable for the \strong{X-axis} (the primary
#'   grouping variable). Use \code{NULL} for a simple bar chart of \code{yyy}'s distribution. Must be a string.
#' @param ppp The name of the categorical variable used to \strong{panel} (facet) the bar charts.
#'   Defaults to \code{NULL} (no faceting). Must be a string.
#' @param stac Logical. If \code{TRUE}, bars are \strong{stacked}. If \code{FALSE}, bars are \strong{dodged}.
#' @param imis Logical. If \code{TRUE}, \code{NA} values in the plotting variables are treated as
#'   a separate category (named by \code{mist}). If \code{FALSE}, rows with \code{NA} values are omitted.
#' @param ilab Logical. If \code{TRUE}, displays count or percentage \strong{labels} on the bars.
#' @param fli Logical. If \code{TRUE}, flips the coordinate system to create a \strong{horizontal} bar chart.
#' @param rota Logical. If \code{TRUE}, rotates the X-axis text labels by 80 degrees for better readability.
#' @param lpo Character string specifying the \strong{legend position}. Common values include \code{"right"}, \code{"left"}, \code{"bottom"}, \code{"top"}, or \code{"none"}.
#' @param tasi Character string specifying the font size for the \strong{bar labels} (\code{ilab = TRUE}). Must be one of \code{"ts"} (Small), \code{"tm"} (Medium), or \code{"tl"} (Large).
#' @param fosi Character string specifying the overall \strong{plot text size} (theme base size). Must be one of \code{"fs"} (Small - 15), \code{"fm"} (Medium - 20), or \code{"fl"} (Large - 25).
#' @param suty Character string specifying the \strong{scale} for the Y-axis. Must be one of:
#'   \itemize{
#'     \item \code{"c"}: \strong{Counts} (Frequency)
#'     \item \code{"p"}: \strong{Proportions} (Percentage)
#'   }
#' @param mti Character string for the \strong{main title} of the plot. Defaults to \code{NULL}.
#' @param msti Character string for the \strong{subtitle} of the plot. Defaults to \code{NULL}.
#' @param lti Character string for the \strong{legend title}. Defaults to \code{NULL} (uses \code{yyy}).
#' @param mist Character string used to label the missing values category if \code{imis = TRUE}.
#'   Defaults to \code{NULL} (will be set to "Unknown" internally by \code{mis_label_function}).
#'
#' @return A \code{ggplot} object representing the bar chart.
#'
#' @details
#' The function relies on \code{dplyr} and \code{ggplot2} and assumes the existence of a custom utility function named \code{mis_label_function} (used to replace NA values with a custom label).
#'
#' \itemize{
#'   \item \strong{Simple Chart} (\code{xxx = NULL}): Plots the distribution of \code{yyy}. The bars are **not** stacked/dodged because there is no grouping variable.
#'   \item \strong{Grouped Chart} (\code{xxx != NULL}): Plots \code{yyy} as the fill within categories defined by \code{xxx} on the X-axis.
#'   \item \strong{Labels (ilab = TRUE)}: For stacked bars, labels are placed within the stack (\code{position_stack(vjust = 1)} is used but then overridden by a manual \code{vjust = -0.2} which seems to be an attempt to place them slightly above the segment tops or the total top, which is generally incorrect for stacking but reflects the current code implementation). For dodged bars, labels are placed above the bar tops.
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming 'dplyr', 'ggplot2', and a custom 'mis_label_function' are loaded.
#' library(ggplot2)
#' library(dplyr)
#'
#' # Define a placeholder for the assumed helper function
#' mis_label_function <- function(x, label) {
#'   x[is.na(x)] <- label
#'   return(x)
#' }
#'
#' # Example Data
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$vs <- factor(mtcars$vs, labels = c("V-Shaped", "Straight"))
#' mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
#'
#' # 1. Simple Bar Chart (Counts)
#' # Plots distribution of 'cyl' (yyy) only
#' barchart____plot(
#'   data = mtcars, yyy = "cyl", xxx = NULL, ppp = NULL,
#'   stac = TRUE, imis = FALSE, ilab = TRUE, fli = FALSE, rota = FALSE,
#'   lpo = "none", tasi = "tm", fosi = "fm", suty = "c",
#'   mti = "Simple Cylinder Count"
#' )
#'
#' # 2. Grouped Bar Chart (Stacked, Percentages)
#' # X-axis: 'am', Fill: 'vs', Stacked, Percentages
#' barchart____plot(
#'   data = mtcars, yyy = "vs", xxx = "am", ppp = NULL,
#'   stac = TRUE, imis = FALSE, ilab = FALSE, fli = FALSE, rota = FALSE,
#'   lpo = "right", tasi = "tm", fosi = "fl", suty = "p",
#'   mti = "Stacked Percentages by Transmission"
#' )
#'
#' # 3. Grouped Bar Chart (Dodged, Counts, Rotated)
#' # X-axis: 'cyl', Fill: 'vs', Dodged, Counts, Rotated X-labels
#' barchart____plot(
#'   data = mtcars, yyy = "vs", xxx = "cyl", ppp = NULL,
#'   stac = FALSE, imis = FALSE, ilab = TRUE, fli = FALSE, rota = TRUE,
#'   lpo = "bottom", tasi = "ts", fosi = "fs", suty = "c",
#'   mti = "Dodged Counts by Cylinder"
#' )
#' }

#' summ_data <- c("Ulcer","NonUlcer")
#' dat <- data.frame(
#'   Bloodtype = c("O", "A", "B", "AB"),
#'   Ulcer = c(698, 472, 102, 29),
#'   NonUlcer = c(345, 23, 432, 65)
#' )
#' dat <- dat %>% tidyr::pivot_longer(all_of(summ_data), names_to = "Group", values_to = "N")
#' expanded_dat <- dat %>% tidyr::uncount(N)
#' # Factor levels are set to ensure proper ordering in the plot
#' expanded_dat <- expanded_dat %>% mutate(
#'   Bloodtype = factor(Bloodtype, levels = c("O", "A", "B", "AB")),
#'   Group = factor(Group, levels = c("Ulcer", "nonUlcer"))
#' )
#' barchart____plot(
#'   data = expanded_dat,
#'   yyy = "Bloodtype",
#'   xxx = "Group",
#'   ppp = NULL,
#'   stac = TRUE,
#'   imis = TRUE,
#'   ilab = TRUE,
#'   fli = FALSE,
#'   rota = TRUE,
#'   lpo = "right",
#'   tasi = "tm",
#'   fosi = "fm",
#'   suty = "c",
#'   mti = "Overall Blood Type Distribution",
#'   msti = "Ulcer"
#' )
#' 
#' 
#' 

barchart____plot <- function(data, yyy, xxx, ppp, stac, imis, ilab, fli, rota,
                             lpo, tasi, fosi, suty, mti = NULL, msti = NULL, lti = NULL, mist= "(Unknown)",nd=1) {
  
  varName <- yyy
  byName <- xxx
  panName <- ppp
  
  df <- data.frame(outcome = factor(data[[varName]]) )
  if (!is.null(byName)) df$group <- factor(data[[byName]])
  if (!is.null(panName)) df$panel <- factor(data[[panName]])
  
  
  #+++++++++++++++
  #++ Font size
  #+++++++++++++++
  fontsi <- dplyr::case_when(
    fosi == "fs" ~ 15,
    fosi == "fm" ~ 20,
    fosi == "fl" ~ 25
  )
  
  
  # Note: mis_label_function is assumed to be an existing function, 
  # but since its package is unknown, it remains without a prefix.
  if (imis) {
    df$outcome <- mis_label_function(df$outcome, mist)
    if (!is.null(byName)) df$group <- mis_label_function(df$group, mist)
    if (!is.null(panName)) df$panel <- mis_label_function(df$panel, mist)
  }
  
  if (!imis) {
    df <- df %>% dplyr::filter(!is.na(outcome))
    if (!is.null(byName)) df <- df %>% dplyr::filter(!is.na(group))
    if (!is.null(panName)) df <- df %>% dplyr::filter(!is.na(panel))
  }
  
  
  tasi <- dplyr::case_when(
    tasi == "ts" ~ 4,
    tasi == "tm" ~ 7,
    tasi == "tl" ~ 10
  )
  
  
  pt <- NULL
  
  ################
  ## SCENARIO 1 (Simple Chart, Counts)
  ################
  if (is.null(byName) & suty == "c") {
    
    if (stac) {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = outcome, y = ggplot2::after_stat(count))) +
        ggplot2::geom_bar(position = "stack", stat = "count",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Counts") +
        ggplot2::xlab(varName)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = ggplot2::after_stat(count)),
            position = ggplot2::position_stack(vjust = 1),
            stat = "count",
            vjust = -.2,
            size = tasi
          )
      }
    } else {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = outcome, y = ggplot2::after_stat(count))) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "count",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Counts") +
        ggplot2::xlab(varName)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = ggplot2::after_stat(count)),
            position = ggplot2::position_dodge(.9),
            stat = "count",
            vjust = -.2,
            size = tasi
          )
      }
    }
  }
  
  
  ################
  ## SCENARIO 2 (Simple Chart, Percentages)
  ################
  if (is.null(byName) & suty == "p") {
    if (stac) {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = outcome, y = ggplot2::after_stat(prop))) +
        ggplot2::geom_bar(position = "stack", stat = "prop",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(varName) +
        ggplot2::scale_y_continuous(labels = scales::percent)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = paste(ndformat(100 * ggplot2::after_stat(prop),nd), "%", sep = "")),
            position = ggplot2::position_stack(vjust = 1),
            stat = "prop",
            vjust = -.2,
            size = tasi
          )
      }
      
    } else {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = outcome, y = ggplot2::after_stat(prop))) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "prop",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(varName) +
        ggplot2::scale_y_continuous(labels = scales::percent)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = paste(ndformat(100 * ggplot2::after_stat(prop),nd), "%", sep = "")),
            position = ggplot2::position_dodge(.9),
            stat = "prop",
            vjust = -.2,
            size = tasi
          )
      }
    }
    
  }
  
  
  ################
  ## SCENARIO 3 (Grouped Chart, Counts)
  ################
  if (!is.null(byName) & suty == "c") {
    
    if (stac) {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y = ggplot2::after_stat(count), fill = outcome, by = group)) +
        ggplot2::geom_bar(position = "stack", stat = "count", width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Counts") +
        ggplot2::xlab(byName)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = ggplot2::after_stat(count)),
            position = ggplot2::position_stack(vjust = 1),
            stat = "count",
            vjust = -.2,
            size = tasi
          )
      }
    } else {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y = ggplot2::after_stat(count), fill = outcome, by = group)) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "count",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Counts") +
        ggplot2::xlab(byName)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = ggplot2::after_stat(count)),
            position = ggplot2::position_dodge(.9),
            stat = "count",
            vjust = -.2,
            size = tasi
          )
      }
    }
  }
  
  
  ################
  ## SCENARIO 4 (Grouped Chart, Percentages)
  ################
  if (!is.null(byName) & suty == "p") {
    
    if (stac) {
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y = ggplot2::after_stat(prop), fill = outcome, by = group)) +
        ggplot2::geom_bar(position = "stack", stat = "prop",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(byName) +
        ggplot2::scale_y_continuous(labels = scales::percent)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = paste(ndformat(100 * ggplot2::after_stat(prop),nd), "%", sep = "")),
            position = ggplot2::position_stack(vjust = 1),
            stat = "prop",
            vjust = -.2,
            size = tasi
          )
      }
      
    } else {
      
      pt <- df %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y = ggplot2::after_stat(prop), fill = outcome, by = group)) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "prop",width = 0.5,color = "black",linewidth = 0.2) +
        ggplot2::ylab("Percent") +
        ggplot2::xlab(byName) +
        ggplot2::scale_y_continuous(labels = scales::percent)
      
      if (ilab) {
        pt <- pt +
          ggplot2::geom_text(
            ggplot2::aes(label = paste(ndformat(100 * ggplot2::after_stat(prop),nd), "%", sep = "")),
            position = ggplot2::position_dodge(.9),
            stat = "prop",
            vjust = -.2,
            size = tasi
          )
      }
    }
  }
  
  
  # Flip Coordinates
  if (fli) pt <- pt + ggplot2::coord_flip()
  
  
  # Rotate X-Axis Labels
  if (rota) pt <- pt + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 80, vjust = 0.8, hjust = 0.5))
  
  
  # Faceting (Panel)
  if (!is.null(ppp)) {
    pt <- pt +
      ggplot2::facet_wrap(~panel, ncol = 2)
  }
  
  
  if (!is.null(lti)) legtitle <- lti
  if (is.null(lti)) legtitle <- varName
  
  pt <- pt +
    ggplot2::theme(
      text = ggplot2::element_text(size = fontsi),
      legend.position = lpo
    )
  pt <- pt + ggplot2::labs(fill = legtitle)
  
  if (!is.null(mti)) {
    pt <- pt +
      ggplot2::ggtitle(mti, msti) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.subtitle = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(size = tasi + 4)
      )
  }
  
  return(pt)
}
