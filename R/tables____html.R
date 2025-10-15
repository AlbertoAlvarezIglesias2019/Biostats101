#' @title Generate Formatted HTML Cross-Tabulation Table
#'
#' @description
#' Creates a highly styled HTML cross-tabulation table using \code{knitr::kable} and \code{kableExtra}.
#' It internally relies on a custom function \code{my_tbl_cross} for the initial tabulation
#' and handles options for percentages, marginal totals, and missing values.
#'
#' @param data A data frame or tibble containing the data.
#' @param row A character string specifying the name of the column to use for the table rows.
#' @param col A character string specifying the name of the column to use for the table columns.
#' @param percent A character string specifying the type of percentage to display.
#'   Possible values include:
#'   \itemize{
#'     \item \code{"none"} (default)
#'     \item \code{"row"} (row percentages)
#'     \item \code{"col"} (column percentages)
#'     \item \code{"cell"} (cell percentages)
#'   }
#' @param marg A character string specifying which marginal totals to include.
#'   Possible values are:
#'   \itemize{
#'     \item \code{"mnone"} (default, no marginals)
#'     \item \code{"mcolu"} (column marginals only)
#'     \item \code{"mrow"} (row marginals only)
#'     \item \code{"mcolrow"} (both column and row marginals)
#'   }
#' @param imis Logical. If \code{TRUE} (default), include missing values in the table.
#' @param missing_text An optional character string to replace the default missing value label ("Unknown").
#' @param margin_text An optional character string to replace the default margin total label ("Total").
#' @param addp Logical. If \code{TRUE} (default), include the p-value from the underlying
#'   \code{my_tbl_cross} test, formatted by \code{pvformat}.
#' @param font_size Numeric. The base font size for the table, in pixels (default is 16).
#' @param mti Character string. The main title or caption for the table, displayed with custom HTML styling.
#' @param nd number of decimals for percentages Default is \code{"1"}.
#'
#' @return A list containing the generated HTML table object, suitable for display in R Markdown
#'   or Shiny applications.
#'   \item{table}{An object of class \code{knitr_kable} and \code{html_widget} that can be printed
#'     or further manipulated by \code{kableExtra}.}
#' @export
#'
#' @importFrom dplyr %>% select mutate if_else filter across everything
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec add_header_above footnote
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_tbl_cross' and 'pvformat' functions are defined and available,
#' # and a data frame 'iris' is used.
#'
#' # Sample data (using a built-in R dataset for demonstration)
#' data(iris)
#' iris$Petal.Length_Group <- cut(iris$Petal.Length, breaks = c(0, 3, 5, 7),
#'                                labels = c("Short", "Medium", "Long"), include.lowest = TRUE)
#'
#' # Example 1: Basic table with column percentages and column marginals
#' table_html_1 <- tables____html(
#'   data = iris,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "colu",
#'   marg = "mcolu",
#'   mti = "Species by Petal Length Group (Col Percent)",
#'   font_size = 14
#' )
#' print(table_html_1$table) # To display in R console/viewer
#'
#' # Example 2: Table without p-value, with row percentages and both marginals
#' table_html_2 <- tables____html(
#'   data = iris,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "row",
#'   marg = "mcolrow",
#'   addp = FALSE,
#'   margin_text = "TOTAL",
#'   mti = "Species by Petal Length Group (Row Percent)"
#' )
#' print(table_html_2$table)
#'
#' # Example 3: Basic table without marginals and including missing text
#' # (Must simulate missing data for this example to have effect)
#' iris_miss <- iris
#' iris_miss$Petal.Length_Group[10:15] <- NA
#'
#' table_html_3 <- tables____html(
#'   data = iris_miss,
#'   row = "Species",
#'   col = "Petal.Length_Group",
#'   percent = "none",
#'   marg = "mnone",
#'   imis = TRUE,
#'   missing_text = "NA Value",
#'   addp = TRUE,
#'   mti = "Species by Petal Length Group (Including Missing)"
#' )
#' print(table_html_3$table)
#' }
tables____html <- function(data,
                           row,
                           col,
                           percent = "none",
                           marg = "mcolrow",
                           imis = TRUE,
                           missing_text = NULL,
                           margin_text = NULL,
                           addp = TRUE,
                           font_size = 16,
                           mti = NULL,
                           nd = 1) {
  
  # Define labels for missing and margin totals
  mist <- "Unknown"
  if (!is.null(missing_text)) mist <- missing_text
  # HTML formatting for missing text
  mist_html <- paste("<i>(", mist, ")</i>", sep = "")
  
  mart <- "Total"
  if (!is.null(margin_text)) mart <- margin_text
  
  # +++++++++++++++++++++++
  # +++ Prepare the table
  # +++++++++++++++++++++++
  # NOTE: my_tbl_cross and pvformat are assumed to be user-defined functions
  # and are not explicitly defined here.
  TTT <- my_tbl_cross(data, row = row, col = col, perc = percent,nd=nd)
  
  tbl <- TTT %>%
    dplyr::select(-test_name)
  # RRR seems to be the row label column in the output of my_tbl_cross
  tbl <- tbl %>%
    dplyr::mutate(row_type = dplyr::if_else(.data$RRR == "Sum" | .data$RRR == .data$RRR[1], "label", "nolabel"))
  
  # Include missing
  if (imis) {
    tbl <- tbl %>%
      dplyr::mutate(RRR = dplyr::if_else(.data$RRR == "MMM", mist_html, .data$RRR))
    # Rename the column 'MMM' to the missing text label for display
    names(tbl)[names(tbl) %in% "MMM"] <- mist_html
  } else {
    if (percent == "none") {
      # Adjust total row/column if 'MMM' is being removed without percentages
      tbl$Sum <- tbl$Sum - tbl$MMM
      tbl[tbl$RRR == "Sum", !names(tbl) %in% c("RRR", "p.value","row_type")] <- tbl[tbl$RRR == "Sum", !names(tbl) %in% c("RRR", "p.value","row_type")] -
        tbl[tbl$RRR == "MMM", !names(tbl) %in% c("RRR", "p.value","row_type")]
    } 
    
    tbl <- tbl %>%
      dplyr::filter(!.data$RRR == "MMM") %>%
      dplyr::select(-MMM)
    
  }
  
  # Deal with the margins
  if (marg == "mnone") {
    tbl <- tbl %>%
      dplyr::filter(!.data$RRR == "Sum") %>%
      dplyr::select(-Sum)
  }
  if (marg == "mcolu") {
    tbl <- tbl %>%
      dplyr::select(-Sum) %>%
      dplyr::mutate(RRR = dplyr::if_else(.data$RRR == "Sum", mart, .data$RRR))
  }
  if (marg == "mrow") {
    tbl <- tbl %>%
      dplyr::filter(!.data$RRR == "Sum")
    names(tbl)[names(tbl) %in% "Sum"] <- mart
  }
  if (marg == "mcolrow") {
    tbl <- tbl %>%
      dplyr::mutate(RRR = dplyr::if_else(.data$RRR == "Sum", mart, .data$RRR))
    names(tbl)[names(tbl) %in% "Sum"] <- mart
  }
  
  # p.value
  if (addp) {
    # NOTE: pvformat is assumed to be a user-defined function.
    tbl$p.value[1] <- pvformat(tbl$p.value[1])
    tbl$p.value[-1] <- NA
  } else {
    tbl <- tbl %>%
      dplyr::select(-p.value)
  }
  
  dframe <- tbl
  
  # Convert everything to character and replace NA with empty string
  dframe <- dframe %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  wher <- is.na(dframe)
  dframe[wher] <- ""
  
  # *************************
  # *** tidy up first column
  # *************************
  dframe <- dframe %>%
    dplyr::mutate(RRR = dplyr::if_else(.data$row_type == "label", paste("<b>", .data$RRR, "</b>", sep = ""), .data$RRR))
  dframe <- dframe %>%
    dplyr::mutate(RRR = dplyr::if_else(!.data$row_type == "label", paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", .data$RRR, sep = ""), .data$RRR))
  dframe <- dframe %>%
    dplyr::select(-row_type)
  
  # *************************
  # Define the table column headers with HTML formatting.
  # *************************
  col_headers_html <- names(dframe)
  col_headers_html[col_headers_html == "RRR"] <- " "
  col_headers_html[col_headers_html == "p.value"] <- "<b>P-value<sup>1</sup></b>"
  col_headers_html[col_headers_html == mart] <- paste("<b>", col_headers_html[col_headers_html == mart], "</b>", sep = "")
  
  
  # *************************
  # Create the HTML string for the table caption.
  # *************************
  caption_html <- paste(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>", mti, "</p>",
    sep = ""
  )
  
  # *************************
  # Define the HTML strings for footnotes
  # *************************
  footnotes_html <- NULL
  if (addp) {
    fn1 <- TTT$test_name[1] # Assumes test_name is a single-element vector or only the first is needed
    fn1 <- paste("<i>", fn1, "</i>", sep = "")
    footnotes_html <- fn1
  }
  
  
  # *************************
  # Define the HTML header vector for spanning columns
  # *************************
  ttmm <- names(TTT)[!names(TTT) %in% c("RRR", "MMM", "Sum", "test_name", "p.value")]
  wher <- names(dframe) %in% ttmm
  tna <- dplyr::if_else(wher, col, " ")
  # Create the named vector for add_header_above
  header_vector <- setNames(rle(tna)$lengths, rle(tna)$values)
  
  # Alignment string for kable
  alit <- c("l", rep("c", dim(dframe)[2] - 1))
  alit <- paste(alit, collapse = "")
  
  
  # ---  BUILD THE KABLEEXTRA TABLE ---
  table_out <- knitr::kable(
    as.data.frame(dframe),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    bold = FALSE,
    escape = FALSE
  ) %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      #bold = TRUE, # Using CSS instead of bold argument
      extra_css = "font-weight: normal;"
    ) %>%
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) 
  
  table_out <- table_out %>% kableExtra::add_header_above(header_vector)
  
  table_out <- table_out %>%
    kableExtra::row_spec(
      row = 0,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Data rows spec
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Final row spec (thicker bottom border)
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  return(list(table = table_out))
}