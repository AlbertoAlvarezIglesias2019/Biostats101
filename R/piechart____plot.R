#' @title Create a Pie Chart or a Panelled Pie Chart
#'
#' @description This function generates a single pie chart or a set of pie charts (panelled)
#'   from an uncounted data frame, using the \code{ggplot2} package. It calculates
#'   counts/proportions and handles optional missing values.
#'
#' @param data A data frame containing the data to be plotted. It should be in the
#'   'expanded' or 'uncounted' format, where each row represents a single observation.
#' @param yyy The name of the categorical variable to be displayed within the pie chart(s).
#'   (i.e., the slice variable). Must be a string.
#' @param ppp The name of the categorical variable used to panel the pie charts. If
#'   \code{NULL} (the default), a single pie chart for \code{yyy} is created. Must be a string.
#' @param imis Logical. If \code{TRUE}, \code{NA} values in \code{yyy} and \code{ppp}
#'   are treated as a separate category (named by \code{mist}). If \code{FALSE}, rows
#'   with \code{NA} values are omitted.
#' @param lpo Character string specifying the legend position. Passed to \code{ggplot2::theme(legend.position)}.
#'   Common values include \code{"right"}, \code{"left"}, \code{"bottom"}, \code{"top"}, or \code{"none"}.
#' @param tasi Character string specifying the size of the text labels within the slices.
#'   Must be one of \code{"ts"} (Tiny - size 4), \code{"tm"} (Medium - size 7), or
#'   \code{"tl"} (Large - size 10).
#' @param suty Character string specifying the type of label to display within the slices.
#'   Must be one of:
#'   \itemize{
#'     \item \code{"c"}: Count (N)
#'     \item \code{"p"}: Percentage (%) (with one decimal place)
#'     \item \code{"pc"}: Percentage and Count (e.g., "50.0% (100)")
#'     \item \code{"none"}: No labels displayed
#'   }
#' @param mti Character string for the main title of the plot. Defaults to \code{NULL}.
#' @param msti Character string for the subtitle of the plot. Defaults to \code{NULL}.
#' @param lti Character string for the legend title. Defaults to \code{NULL} (uses \code{yyy}).
#' @param mist Character string used to label the missing values category if \code{imis = TRUE}.
#'   Defaults to \code{"Unknown"}.
#'
#' @return A \code{ggplot} object representing the pie chart(s).
#'
#' @details The function internally aggregates the data using \code{dplyr} and plots
#'   it with \code{ggplot2}. The data must be in an 'uncounted' format (i.e., a row
#'   for every single item or observation). Note that \code{ndformat} is assumed
#'   to be a custom function available in the environment for numerical formatting.
#'   When \code{ppp} is used, the slices in each panel are scaled to represent
#'   the proportion *within that group* (i.e., \code{scales = "free"} in \code{facet_wrap}).
#'
#' @examples
#' \dontrun{
#' # Example Data Preparation: Simulating the 'uncounted' data from the commented block
#' summ_data <- c("Ulcer", "nonUlcer")
#' dat <- data.frame(
#'   Bloodtype = c("O", "A", "B", "AB"),
#'   Ulcer = c(698, 472, 102, 29),
#'   nonUlcer = c(2892, 2625, 570, 226)
#' )
#'
#' # Requires 'dplyr' and 'tidyr'
#' library(dplyr)
#' library(tidyr)
#'
#' dat <- dat %>% tidyr::pivot_longer(all_of(summ_data), names_to = "Group", values_to = "N")
#' expanded_dat <- dat %>% tidyr::uncount(N)
#'
#' # Factor levels are set to ensure proper ordering in the plot
#' expanded_dat <- expanded_dat %>% mutate(
#'   Bloodtype = factor(Bloodtype, levels = c("O", "A", "B", "AB")),
#'   Group = factor(Group, levels = c("Ulcer", "nonUlcer"))
#' )
#'
#' # Example 1: Panelled Pie Chart (Ulcer vs. nonUlcer)
#' # Shows the blood type distribution within each group, with percentage labels.
#' piechart____plot(
#'   data = expanded_dat,
#'   yyy = "Bloodtype",
#'   ppp = "Group",
#'   imis = FALSE,
#'   lpo = "left",
#'   tasi = "tl",
#'   suty = "p",
#'   mti = NULL,
#'   msti = NULL,
#'   lti = NULL
#' )
#'
#' # Example 2: Single Pie Chart
#' # Shows the overall distribution of 'Bloodtype' across ALL groups combined, with counts.
#' piechart____plot(
#'   data = expanded_dat,
#'   yyy = "Bloodtype",
#'   ppp = NULL, # Single chart
#'   imis = FALSE,
#'   lpo = "right",
#'   tasi = "tm",
#'   suty = "c",
#'   mti = "Overall Blood Type Distribution",
#'   msti = "Combined Ulcer and nonUlcer Cohort"
#' )
#'
#' # Example 3: Single Pie Chart with Missing Value Handling (Demonstrative)
#' # Add some NA values to demonstrate imis=TRUE functionality
#' expanded_dat_na <- expanded_dat
#' expanded_dat_na$Bloodtype[1:50] <- NA
#'
#' piechart____plot(
#'   data = expanded_dat_na,
#'   yyy = "Bloodtype",
#'   ppp = NULL,
#'   imis = TRUE, # Include NA values
#'   lpo = "right",
#'   tasi = "tm",
#'   suty = "pc",
#'   mti = "Blood Type Including Missing",
#'   mist = "Missing Data" # Custom label for NA
#' )
#' }
#' 
#' summ_data <- c("Ulcer")
#' dat <- data.frame(
#'   Bloodtype = c("O", "A", "B", "AB"),
#'   Ulcer = c(698, 472, 102, 29)
#' )
#' dat <- dat %>% tidyr::pivot_longer(all_of(summ_data), names_to = "Group", values_to = "N")
#' expanded_dat <- dat %>% tidyr::uncount(N)
#' # Factor levels are set to ensure proper ordering in the plot
#' expanded_dat <- expanded_dat %>% mutate(
#'   Bloodtype = factor(Bloodtype, levels = c("O", "A", "B", "AB")),
#'   Group = factor(Group, levels = c("Ulcer", "nonUlcer"))
#' )
#' piechart____plot(
#'   data = expanded_dat,
#'   yyy = "Bloodtype",
#'   ppp = "Group",
#'   imis = FALSE,
#'   lpo = "right",
#'   tasi = "tm",
#'   suty = "c",
#'   mti = "Overall Blood Type Distribution",
#'   msti = "Ulcer"
#' )
 

piechart____plot <- function(data, yyy, ppp, imis, lpo, tasi, suty, mti = NULL, msti = NULL, lti = NULL, mist = NULL,nd =1) {
  
  varName <- yyy
  byName <- ppp
  
  if (is.null(mist)) mist <- "Unknown"
  
  # case_when from dplyr
  tasi <- dplyr::case_when(tasi == "ts" ~ 4,
                           tasi == "tm" ~ 7,
                           tasi == "tl" ~ 10)
  
  #################
  # Basic piechart
  #################
  if (is.null(ppp)) {
    df <- data.frame(outcome = data[[varName]])
    
    # mutate and if_else from dplyr
    if (imis) { df <- df %>% dplyr::mutate(outcome = dplyr::if_else(is.na(outcome), mist, outcome)) }
    # na.omit is a base R function, but adding 'stats::' is safest if you are paranoid, though typically unneeded.
    if (!imis) { df <- df %>% stats::na.omit() }
    
    df <- df %>% 
      dplyr::mutate(N = sum(!is.na(outcome))) %>% 
      dplyr::group_by(outcome) %>% 
      dplyr::summarise(N = N[1],
                       value = n()) %>% 
      dplyr::ungroup()
    
    # mutate and case_when from dplyr
    df <- df %>% 
      dplyr::mutate(label = dplyr::case_when(suty == "c" ~ paste(value, sep=""),
                                             suty == "p" ~ paste(ndformat(value / N * 100, nd), "%", sep=""),
                                             suty == "pc" ~ paste(ndformat(value / N * 100, nd), "% (", value, ")", sep="")))
    
    if (suty == "none") {
      # All plot elements from ggplot2
      pt <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = outcome)) +
        ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void() # remove background, grid, numeric labels
    }
    
    if (!suty == "none") {
      # arrange and mutate from dplyr
      df <- df %>% 
        dplyr::arrange(dplyr::desc(outcome)) %>%
        dplyr::mutate(prop = value) %>% 
        dplyr::mutate(ypos = cumsum(prop) - 0.5 * prop)
      
      # All plot elements from ggplot2
      pt <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = outcome)) +
        ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::geom_text(ggplot2::aes(y = ypos, label = label), color = "white", size = tasi) +
        ggplot2::scale_fill_brewer(palette = "Set1")
    }
    
  }
  
  
  #####################
  # Panelled piechart
  #####################
  if (!is.null(ppp)) {
    df <- data.frame(outcome = data[[varName]],
                     group = data[[byName]])
    
    if (imis) {
      # mutate and if_else from dplyr
      df <- df %>% dplyr::mutate(outcome = dplyr::if_else(is.na(outcome), mist, outcome))
      df <- df %>% dplyr::mutate(group = dplyr::if_else(is.na(group), mist, group))
    }
    # na.omit is a base R function, but adding 'stats::' is safest if you are paranoid, though typically unneeded.
    if (!imis) { df <- df %>% stats::na.omit() }
    
    # All chain functions from dplyr
    df <- df %>% 
      dplyr::group_by(group) %>% 
      dplyr::mutate(N = sum(!is.na(outcome))) %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(group, outcome) %>% 
      dplyr::summarise(N = N[1],
                       value = n()) %>% 
      dplyr::ungroup()
    
    
    # mutate and case_when from dplyr
    df <- df %>% 
      dplyr::mutate(label = dplyr::case_when(suty == "c" ~ paste(value, sep=""),
                                             suty == "p" ~ paste(ndformat(value / N * 100, nd), "%", sep=""),
                                             suty == "pc" ~ paste(ndformat(value / N * 100, nd), "% (", value, ")", sep="")))
    
    # All chain functions from dplyr
    df <- df %>% 
      dplyr::arrange(group, dplyr::desc(outcome)) %>%
      dplyr::group_by(group) %>% 
      dplyr::mutate(prop = value) %>% 
      dplyr::mutate(ypos = cumsum(prop) - 0.5 * prop) %>% 
      dplyr::ungroup()
    
    if (suty == "none") {
      # All plot elements from ggplot2
      pt <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = outcome)) +
        ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::facet_wrap(~group, scales = "free",ncol = 2)
    }
    
    if (!suty == "none") {
      # All plot elements from ggplot2
      pt <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = value, fill = outcome)) +
        ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::geom_text(ggplot2::aes(y = ypos, label = label), color = "white", size = tasi) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::facet_wrap(~group, scales = "free",ncol = 2)
    }
    
    
  }
  
  # All remaining plot elements from ggplot2
  pt <- pt + ggplot2::theme(text = ggplot2::element_text(size = 25), legend.position = lpo)
  pt <- pt + ggplot2::labs(fill = yyy)
  if (!is.null(lti)) pt <- pt + ggplot2::labs(fill = lti)
  if (!is.null(mti)) {
    pt <- pt + ggplot2::ggtitle(mti, msti) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5))
  }
  
  return(pt)
}

