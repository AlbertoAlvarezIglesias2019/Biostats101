#' @title Perform a Paired T-Test using Bootstrap Inference and Generate an HTML Table
#' @description This function performs a paired t-test using bootstrap resampling and
#'   permutation methods from the `infer` package. It calculates a confidence
#'   interval and a p-value, and then formats the results into a styled HTML table.
#'
#' @details This function uses the `infer` package's workflow to perform statistical
#'   inference. It generates a bootstrap distribution of the mean difference to
#'   calculate a percentile-based confidence interval. It also generates a null
#'   distribution via permutation (shuffling) to calculate the p-value for the
#'   specified null hypothesis.
#'
#'   The function returns a list containing the final HTML table and two `ggplot`
#'   objects, which visualize the null and bootstrap distributions.
#'
#' @param var1 A numeric vector representing the first variable.
#' @param var2 A numeric vector representing the second variable.
#' @param conf_boot A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_boot A character string specifying the alternative hypothesis direction.
#'   Can be "two_sided", "greater", or "less". Note the use of an underscore for "two_sided".
#' @param nd An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param testyn_boot A logical value; if `TRUE`, the P-value column is included in the output table.
#'
#' @return A `list` with three elements:
#'   - `table`: a `kableExtra` HTML table object summarizing the results.
#'   - `plot_null`: a `ggplot` object visualizing the null distribution.
#'   - `plot_interval`: a `ggplot` object visualizing the bootstrap confidence interval.
#'
#' @examples
#' # Create example data
#' data_paired <- data.frame(
#'   before = c(10, 12, 15, 14, 18),
#'   after = c(15, 13, 16, 17, 20)
#' )
#'
#' # Example 1: Basic usage with a two-sided test and 95% confidence
#' boot_results <- pairedt____boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.95,
#'   alt_boot = "two-sided",
#'   nd = 3
#' )
#'
#' # Accessing the results
#' boot_results$table
#'
#' # Example 2: Exclude the p-value column
#' pairedt____boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.95,
#'   alt_boot = "two-sided",
#'   nd = 3,
#'   testyn_boot = TRUE
#' )
#'
#' # Example 3: One-sided test with a different null hypothesis
#' pairedt____boot(
#'   var1 = data_paired$before,
#'   var2 = data_paired$after,
#'   conf_boot = 0.90,
#'   alt_boot = "less",
#'   nh_boot = 2,
#'   nd = 3
#' )
#' 
#' 
     
pairedt____boot <- function(var1,var2, conf_boot, alt_boot, nd, font_size=16, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND INFER WORKFLOW
  # =========================================================================
  

  df <- data.frame(var1 = var1,
                   var2 = var2)
  
  df <- df %>%
    dplyr::mutate(diff = var1 - var2) 
    
  
  df <- df %>% na.omit()
  #rrr <- data[[variable]]
  #ggg <- data[[by]]
  
  #stop(paste(df$ggg,collapse="; "))
  
  # Calculate the observed sample mean from the data.
  x_tilde <- df %>% 
    infer::specify(response = diff)  %>% 
    infer::calculate(stat = "mean")
  
  # Generate a bootstrap distribution of sample proportions
  # This is used to calculate the confidence interval.
  boot_dist <- df %>%
    infer::specify(response = diff) %>%
    infer::generate(reps = 1000, type = "bootstrap") %>%
    infer::calculate(stat = "mean")
  
  # Generate a null distribution centered at the null hypothesis mean.
  # This is used to calculate the p-value.
  null_dist <- df %>%
    infer::specify(response = diff) %>%
    infer::hypothesize(null = "paired independence") %>%
    infer::generate(reps = 1000, type = "permute") %>%
    infer::calculate(stat = "mean")
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  
  # Create a ggplot object for the null distribution plot.
  plot_null <- infer::visualize(null_dist) +
    infer::shade_p_value(obs_stat = x_tilde, direction = alt_boot)
  

  
  # Get the p-value based on the observed statistic and the null distribution.
  pv <- null_dist %>%
    infer::get_p_value(obs_stat = x_tilde, direction = alt_boot)
  
  # Format the p-value for display.
  pvalue <- pvformat(pv)

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
  percentile_ci <- ndformat(percentile_ci,nd)
  percentile_ci <- paste("(", paste(percentile_ci, collapse = ", "), ")", sep = "")
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  
  # Create the data frame for the table.
  dframe <- data.frame(
    md = ndformat(x_tilde,nd),
    Ci = percentile_ci,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Mean Diff",
    paste(conf_boot * 100, "% Bootstrap CI for d<sup>1</sup>", sep = ""),
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
      alt_boot == "greater" ~ paste("H<sub>1</sub>:d&gt; ", 0, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>:d&lt; ", 0, sep = ""),
      alt_boot == "two-sided" ~ paste("H<sub>1</sub>:d&ne; ", 0, sep = "")
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