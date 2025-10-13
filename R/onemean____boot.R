#' @title Perform One-Sample Bootstrap Inference
#' @description This function conducts a one-sample bootstrap analysis to create a
#'   confidence interval and, optionally, a hypothesis test for a single mean. It
#'   generates a confidence interval based on the percentile method and calculates a
#'   p-value using a simulated null distribution. The results are formatted into
#'   a customizable HTML table and a plot of the null distribution.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the numeric variable
#'   in the data frame.
#' @param conf_boot A numeric value between 0 and 1 specifying the confidence level
#'   for the bootstrap confidence interval.
#' @param nh_boot The numeric value of the null hypothesis mean (\eqn{\mu_0}).
#' @param alt_boot A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two-sided"`.
#' @param nd The number of decimal places to round the confidence interval to.
#' @param font_size The font size for the output HTML table.
#' @param testyn_boot A logical value. If \code{TRUE}, a p-value for the
#'   hypothesis test is included in the output. If \code{FALSE} (the default),
#'   only the confidence interval is shown.
#' @return A list containing two elements:
#'   \item{table}{A \code{kableExtra} HTML table object showing the confidence
#'     interval and p-value (if requested).}
#'   \item{plot_null}{A \code{ggplot} object visualizing the null distribution with
#'     the observed statistic and shaded p-value.}
#'   \item{plot_interval}{A \code{ggplot} object visualizing the confidence interval.}
#' @details This function is designed for use within a Jamovi module. It relies
#'   on the \code{infer} package for statistical computations and \code{kableExtra}
#'   for HTML table formatting. The confidence interval is calculated using the
#'   percentile method from the bootstrap distribution. The p-value is calculated
#'   by generating a simulated null distribution and finding the proportion of
#'   simulated means that are as extreme or more extreme than the observed sample mean.
#'
#' @note For valid results in real analyses, it is highly recommended to use a much
#'   larger number of replicates than the default `reps = 1000` used here (e.g., 5000+).
#'   This function's use of `get_ci()` is a custom or internal alias; the standard
#'   function in the \code{infer} package is `get_confidence_interval()`.
#'
#' @seealso \code{\link[infer]{infer}}, \code{\link[kableExtra]{kableExtra}}
#'
#' @importFrom dplyr select mutate if_else case_when
#' @importFrom infer specify hypothesize generate calculate get_p_value get_confidence_interval visualize shade_p_value
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom rlang .data
#' @importFrom tibble tibble
#'
#' @examples
#' # Create a sample dataset
#' library(kableExtra)
#' library(dplyr)
#' set.seed(123)
#' sample_data <- tibble::tibble(
#'   my_var = rnorm(100, mean = 55, sd = 10)
#' )
#'
#' # Example 1: Default usage with a hypothesis test
#' # Test if the population mean is different from 50
#' onemean____boot(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_boot = 0.95,
#'   nh_boot = 50,
#'   alt_boot = "two-sided",
#'   nd = 2,
#'   font_size = 12,
#'   testyn_boot = TRUE
#' )$table
#'
#' # Example 2: Get only the confidence interval
#' # Do not perform a hypothesis test
#' onemean____boot(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_boot = 0.95,
#'   nh_boot = 50,
#'   alt_boot = "two-sided",
#'   nd = 2,
#'   font_size = 12,
#'   testyn_boot = FALSE
#' )$table
#'
#' # Example 3: Different alternative hypothesis
#' # Test if the population mean is greater than 50
#' onemean____boot(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_boot = 0.95,
#'   nh_boot = 50,
#'   alt_boot = "greater",
#'   nd = 2,
#'   font_size = 12,
#'   testyn_boot = TRUE
#' )$table
#'
#'
#' # Example 4: Visualise the interval
#' # Test if the population mean is greater than 50
#' call_boot <-onemean____boot(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_boot = 0.95,
#'   nh_boot = 50,
#'   alt_boot = "greater",
#'   nd = 2,
#'   font_size = 12,
#'   testyn_boot = TRUE
#' )
#' call_boot$plot_interval
#' 
onemean____boot <- function(data, variable, conf_boot, nh_boot, alt_boot, nd, font_size, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND INFER WORKFLOW
  # =========================================================================
  
  # Create a data frame in the format infer expects, with a single column.
  # Using `data[[variable]]` is a robust way to select a column by string name.
  df <- data.frame(rrr = data[[variable]])
  df <- df %>% na.omit()
  
  # Calculate the observed sample mean from the data.
  x_bar <- df |>
    infer::specify(response = rrr) |>
    infer::calculate(stat = "mean")
  
  # Generate a bootstrap distribution of sample means.
  # This is used to calculate the confidence interval.
  boot_dist <- df %>%
    infer::specify(response = rrr) %>%
    infer::generate(reps = 1000, type = "bootstrap") %>%
    infer::calculate(stat = "mean")
  
  # Generate a null distribution centered at the null hypothesis mean.
  # This is used to calculate the p-value.
  null_dist <- df %>%
    infer::specify(response = rrr) %>%
    infer::hypothesize(null = "point", mu = nh_boot) %>%
    infer::generate(reps = 1000) %>%
    infer::calculate(stat = "mean")
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  
  # Create a ggplot object for the null distribution plot.
  plot_null <- infer::visualize(null_dist) +
    infer::shade_p_value(obs_stat = x_bar, direction = alt_boot)
  

  
  # Get the p-value based on the observed statistic and the null distribution.
  pv <- null_dist %>%
    infer::get_p_value(obs_stat = x_bar, direction = alt_boot)
  
  # Format the p-value for display.
  pvalue <- dplyr::if_else(pv < 0.001, "<0.001", as.character(round(pv, 3)))
  pvalue <- dplyr::if_else(pv > 0.999, ">0.999", pvalue)
  
  # Calculate the percentile confidence interval.
  # NOTE: Your original code had `get_ci()`, which is not a standard `infer` function.
  # This line has been corrected to use `get_confidence_interval()`.
  percentile_ci <- infer::get_confidence_interval(
    x = boot_dist,
    level = conf_boot,
    type = "percentile"
  )
  
  # Create a ggplot object to visualise the intervals.
  plot_interval <- infer::visualize(boot_dist) +
    infer::shade_confidence_interval(percentile_ci)
  
  # Format the confidence interval into a single string.
  percentile_ci <- round(percentile_ci, nd)
  percentile_ci <- paste("(", paste(percentile_ci, collapse = ", "), ")", sep = "")
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  
  # Create the data frame for the table.
  dframe <- data.frame(
    Ps = ndformat(x_bar,nd),
    Ci = percentile_ci,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Bootstrap Mean",
    paste(conf_boot * 100, "% Bootstrap CI for &mu;<sup>1</sup>", sep = ""),
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove the P-value column if not requested.
  if (!testyn_boot) {
    dframe <- dframe %>% dplyr::select(-Pval)
    col_headers_html <- col_headers_html[!col_headers_html %in% "P-value<sup>2</sup>"]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on the test type.
  if (testyn_boot) {
    fn1 <- "Based on percentiles"
    fn2 <- dplyr::case_when(
      alt_boot == "greater" ~ paste("H<sub>1</sub>: &mu;&gt;", nh_boot, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>: &mu;&lt;", nh_boot, sep = ""),
      alt_boot == "two-sided" ~ paste("H<sub>1</sub>: &mu;&ne;", nh_boot, sep = "")
    )
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- "Based on percentiles"
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    footnotes_html <- fn1
  }
  
  # =========================================================================
  # 4. BUILD THE KABLEEXTRA TABLE
  # =========================================================================
  
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
    # Add the footnotes to the bottom of the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  # Return the table and the plot as a list.
  list(table = table_out, plot_null = plot_null,plot_interval=plot_interval)
}