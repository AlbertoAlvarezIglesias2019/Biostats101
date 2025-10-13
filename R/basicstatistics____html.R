#' Generate an HTML Table of Basic Descriptive Statistics
#'
#' This function computes descriptive statistics for a specified variable,
#' optionally stratified by a grouping variable, and formats the results into
#' a highly-styled HTML table suitable for a "Table 1" in a research paper.
#' It relies on external, unprovided functions for core summary calculation
#' and p-value formatting.
#'
#' @param data A data.frame or tibble containing the data.
#' @param variable A character vector specifying the name(s) of the variable(s) to summarize.
#' @param by A character string specifying the name of the grouping variable for stratification.
#'   Set to \code{NULL} (default) for no stratification.
#' @param imis Logical. If \code{TRUE} (default), missing values are included in the table.
#' @param suca A string specifying the statistics to calculate for \strong{categorical} variables.
#'   Passed to the internal \code{my_tbl_summary} function. Defaults to \code{"nNp"}.
#' @param suco A string specifying the statistics to calculate for \strong{continuous} variables.
#'   Passed to the internal \code{my_tbl_summary} function. Defaults to \code{"meansd"}.
#' @param mist An optional character string to label the missing data rows.
#'   Defaults to \code{"(Unknown)"}.
#' @param addp Logical. If \code{TRUE} (default) and \code{by} is not \code{NULL}, a column for p-values is added.
#' @param addt Logical. If \code{TRUE} (default) and \code{by} is not \code{NULL}, an overall column (Total) is added.
#' @param addn Logical. If \code{TRUE} (default), the column for the total number of non-missing observations (\code{N}) is added.
#' @param varl A string for the header of the first column, typically for variable names/labels (e.g., "Demographics"). Defaults to \code{"Demo"}.
#' @param heal A string for the header spanning the columns of the \code{by} variable's levels (e.g., "Treatment"). Defaults to \code{"Treatment"}.
#' @param font_size An integer specifying the font size (in pixels) for the table body. Defaults to \code{16}.
#' @param mti A string for the main title/caption of the table (e.g., "Table 1"). Defaults to \code{"Table 1"}.
#' @param nd An integer specifying the number of decimal places for numeric statistics. Defaults to \code{1}.
#'
#' @return A \code{list} containing the styled HTML table object (\code{kableExtra::kable_styling} output).
#'
#' @seealso \code{\link[knitr]{kable}}, \code{\link[kableExtra]{kable_styling}}
#'
#' @examples
#' # The example below is illustrative and assumes 'my_tbl_summary' and 'pvformat' exist.
#' \dontrun{
#' data(iris)
#' my_data <- iris
#' my_data$trt <- sample(rep(c("A","B"),each = dim(my_data)[1]/2 ))
#'  basicstatistics____html(
#'    data = my_data,
#'    variable = c("Sepal.Length", "Petal.Width","Species"),
#'    by = "trt",
#'    mti = "Iris Demographics by Species"
#'  )
#' 
#' }


basicstatistics____html <- function(data, variable, by = NULL,
                                    imis = TRUE,
                                    suca = "nNp",
                                    suco = "meansd",
                                    mist = NULL,
                                    addp = TRUE,
                                    addt = TRUE,
                                    addn = TRUE,
                                    varl = "Demo",
                                    heal = "Treatment",
                                    font_size = 16,
                                    mti = "Table 1",
                                    nd_num = 1,
                                    nd_cat = 1) {
  
  miss_text <- "(Unknown)"
  if (!is.null(mist)) miss_text <- mist
  
  # Prepare the table
  dframe <- my_tbl_summary(data, variable, by, suca = suca, suco = suco, nd_num=nd_num,nd_cat=nd_cat)
  dframe <- dframe %>% dplyr::mutate(p.value = pvformat(p.value))
  
  # Define the columns to choose
  t1 <- c("row_type", "label", "test_name", "p.value", "variable", "var_type", "n", "stat_label")
  t2 <- stringr::str_detect(names(dframe), "Overall")
  statc <- names(dframe)[!(names(dframe) %in% t1) & !t2]
  if (!is.null(by)) {overallc <- names(dframe)[t2]} else {overallc <- statc}
  
  # Change missing label
  dframe <- dframe %>% dplyr::mutate(label = ifelse(row_type == "missing", miss_text, label))
  
  # If no missing remove the row
  wher <- as.logical(dframe[, overallc] == "0" & dframe$row_type == "missing")
  dframe <- dframe[!wher, ]
  
  # Select or not missing
  if (!imis) dframe <- dframe %>% dplyr::filter(!row_type == "missing")
  
  collist <- c("label")
  
  # Add n
  if (addn) {
    collist <- c(collist, "n")
  }
  
  # Add Totals
  if (addt & !is.null(by)) {
    collist <- c(collist, overallc)
  }
  
  collist <- c(collist, statc)
  collist <- unique(collist)
  
  if (addp & !is.null(by)) {
    collist <- c(collist, "p.value")
  }
  
  # DF <- dframe  ### This variable is defined but not used.
  
  dframe <- dframe %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  wher <- is.na(dframe)
  dframe[wher] <- ""
  
  # Tidy up first column
  dframe <- dframe %>%
    dplyr::mutate(stat_label = ifelse(!stat_label == "",
                                      paste("<span style='font-size: 0.8rem;'>", stat_label, "</span>", sep = ""),
                                      stat_label)) %>%
    dplyr::mutate(label = ifelse(row_type == "label", paste("<b>", label, "</b>", sep = ""), label)) %>%
    dplyr::mutate(label = ifelse(row_type == "label", paste(label, "; ", stat_label, sep = ""), label)) %>%
    dplyr::mutate(label = ifelse(!row_type == "label", paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", label, sep = ""), label)) %>%
    dplyr::select(-row_type, -stat_label)
  
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- collist
  col_headers_html[col_headers_html == "label"] <- varl
  col_headers_html[col_headers_html == "n"] <- "N"
  col_headers_html[col_headers_html == "p.value"] <- "P-value<sup>1</sup>"
  
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>", mti, "</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes
  footnotes_html <- NULL
  # Note: 'byva' is not defined in the function, assuming it should be 'by'.
  # I will use 'by' but keep in mind this is an assumption based on context.
  if (addp & !is.null(by)) {
    fn1 <- paste(unique(dframe$test_name), collapse = "; ")
    fn1 <- paste("<i>", fn1, "</i>", sep = "")
    footnotes_html <- fn1
  }
  
  # Define the HTML header vector
  if (!is.null(heal) & !is.null(by)) {
    if (!heal == "") {
      wher <- collist %in% statc
      tna <- ifelse(wher, heal, " ")
      header_vector <- stats::setNames(rle(wher)$lengths, rle(tna)$values)
      # ttt <- rle(wher)$lengths # This variable is defined but not used.
    }
  }
  
  alit <- c("l", rep("c", length(collist) - 1))
  alit <- paste(alit, collapse = "")
  
  
  # --- BUILD THE KABLEEXTRA TABLE ---
  table_out <- knitr::kable(
    as.data.frame(dframe[, collist]),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) %>%
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    )
  
  if (!is.null(heal) & !is.null(by)) {
    if (!heal == "") {
      table_out <- table_out %>% kableExtra::add_header_above(header_vector)
    }
  }
  
  table_out <- table_out %>%
    # Top border and bottom border of the last row.
    kableExtra::row_spec(
      row = c(0, dim(dframe)[1]),
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Bottom border of the last row.
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Horizontal borders for all data rows.
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out)
}
