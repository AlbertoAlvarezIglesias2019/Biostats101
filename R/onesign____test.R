#' @title Perform a One-Sample Wilcoxon Signed-Rank Test and Generate an HTML Table
#' @description This function performs a one-sample Wilcoxon Signed-Rank Test on a single
#'   variable from a data frame, calculates a confidence interval for the population
#'   pseudomedian, and formats the results into a styled HTML table using `kableExtra`.
#'
#' @details This test is the non-parametric alternative to a one-sample t-test. It is used
#'   to test if the population median of a single sample is equal to a specified value.
#'   The function is designed for use in a Jamovi module, which is why it returns a
#'   list containing a `kableExtra` table object.
#'
#' @param data A data frame containing the variable to be tested.
#' @param variable A character string specifying the name of the variable to analyze.
#' @param conf_test A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param nh_test A numeric value for the null hypothesis median (mu).
#' @param alt_test A character string specifying the alternative hypothesis direction.
#'   Can be "two.sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param imis A logical value; if `TRUE`, missing values are included in the total 'N' count.
#'   If `FALSE`, they are excluded.
#' @param testyn_test A logical value; if `TRUE`, the P-value column is included in the output table.
#'
#' @return A `list` containing one element: `table`, which is a `kableExtra` HTML table object
#'   that summarizes the results of the Wilcoxon signed-rank test.
#'
#' @examples
#' # Create example data
#' data_wilcoxon <- data.frame(
#'   rating = c(8, 7, 9, 6, 8, 10, 7, 5, 9, 8)
#' )
#'
#' # Example 1: Basic usage with a two-sided test and 95% confidence
#' onesign____test(
#'   data = data_wilcoxon,
#'   variable = "rating",
#'   conf_test = 0.95,
#'   nh_test = 7,
#'   alt_test = "two.sided",
#'   font_size=16,
#'   nd = 3
#' )
#'
#' # Example 2: Exclude the test statistics and p-value
#' onesign____test(
#'   data = data_wilcoxon,
#'   variable = "rating",
#'   conf_test = 0.95,
#'   nh_test = 7,
#'   alt_test = "two.sided",
#'   nd = 3,
#'   testyn_test = FALSE
#' )
#'
#' # Example 3: Different confidence level and one-sided test
#' onesign____test(
#'   data = data_wilcoxon,
#'   variable = "rating",
#'   conf_test = 0.90,
#'   nh_test = 7,
#'   alt_test = "greater",
#'   nd = 3
#' )
#' 
#' 

onesign____test <- function(data, variable, conf_test, nh_test, alt_test, nd, font_size=16, imis = FALSE, testyn_test = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  # Extract the variable data from the data frame.
  df <- data.frame(rrr = data[[variable]])
  
  # Run the one-sample t-test.
  fit <- suppressWarnings(stats::wilcox.test(df$rrr, mu = nh_test, conf.level = conf_test, alternative = alt_test, paired = FALSE, conf.int = TRUE))
  
  # Calculate and format descriptive statistics.
  esti <- ndformat(fit$estimate,nd) 

  # Calculate and format the confidence interval.
  out <- ndformat(fit$conf.int,nd)   
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  st <- fit$statistic
  
  # Format the p-value with boundary conditions.
  pval <- pvformat(fit$p.value)

  # Get counts for N and missing values.
  nn <- length(df$rrr)
  mm <- sum(is.na(df$rrr))
  
  # Correct N if missing values are not to be included in the count.
  if (!imis) nn <- sum(!is.na(df$rrr))
  
  # --- 2. BUILD THE TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = variable,
    N = nn,
    Mis = mm,
    Median = esti,
    Ci = ci,
    St = st,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Variable", "N", "Mis", "(Pseudo)Median", paste(conf_test * 100, "% CI for &nu;<sup>1</sup>", sep = ""),
    "V-statistic","P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-Mis)
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
  }
  if (!testyn_test) {
    dframe <- dframe %>% dplyr::select(-Pval)
    col_headers_html <- col_headers_html[!col_headers_html %in% c("P-value<sup>2</sup>")]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>One-Sample Wilcoxon Signed-Rank Test</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  if (testyn_test) {
    fn1 <- dplyr::if_else(alt_test == "two.sided", "&nu;: population median (Two-sided interval)", "&nu;: population median (One-sided interval)")
    fn2 <- dplyr::case_when(
      alt_test == "greater" ~ paste(fit$method," H<sub>1</sub>: &nu;&gt;", nh_test, sep = ""),
      alt_test == "less" ~ paste(fit$method," H<sub>1</sub>: &nu;&lt;", nh_test, sep = ""),
      alt_test == "two.sided" ~ paste(fit$method," H<sub>1</sub>: &nu;&ne;", nh_test, sep = "")
    )
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- dplyr::if_else(alt_test == "two.sided", "&nu;: population median (Two-sided interval)", "&nu;: population median (One-sided interval)")
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    footnotes_html <- fn1
  }
  
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
    # Add vertical borders and padding to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Make the first column (Variable) bold.
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
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
  
  # Return the table as a list.
  list(table = table_out)
}