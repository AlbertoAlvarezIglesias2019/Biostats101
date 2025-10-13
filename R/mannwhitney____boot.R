#' @title Bootstrap Inference for Two-Sample Mann-Whitney Test
#' @description This function performs a bootstrap analysis to compute a confidence interval
#'   for the difference in medians and a p-value for a two-sample Mann-Whitney-like
#'   test. It uses the `infer` package to generate both a bootstrap distribution and
#'   a null distribution, which are then used to create a formatted HTML table and
#'   visualization plots.
#'
#' @param data A `data.frame` or `tibble` containing the variables.
#' @param variable A character string specifying the name of the response variable.
#' @param by A character string specifying the name of the grouping variable.
#' @param conf_boot A numeric value for the confidence level of the bootstrap interval,
#'   e.g., `0.95`.
#' @param nh_boot A numeric value for the null hypothesis.
#' @param alt_boot A character string specifying the alternative hypothesis: `"two-sided"`,
#'   `"greater"`, or `"less"`.
#' @param nd An integer for the number of decimal places to display.
#' @param font_size An integer for the font size of the output table.
#' @param testyn_boot A logical value (`TRUE`/`FALSE`) to include the p-value column
#'   in the output.
#'
#' @return A `list` with three elements: `table` (a `kableExtra` HTML table),
#'   `plot_null` (a `ggplot` of the null distribution), and `plot_interval` (a `ggplot`
#'   of the bootstrap confidence interval).
#'
#' @examples
#' # Create some sample data for a bootstrap analysis
#' if (require(infer)) {
#'   data_example <- data.frame(
#'     group = factor(rep(c("A", "B"), each = 50)),
#'     value = c(rnorm(50, 10, 2), rnorm(50, 12, 2.5))
#'   )
#'
#'   # Run the bootstrap test and display the results
#'   mannwhitney____boot(
#'     data = data_example,
#'     variable = "value",
#'     by = "group",
#'     conf_boot = 0.95,
#'     nh_boot = 0,
#'     alt_boot = "two-sided",
#'     nd = 3,
#'     font_size = 14
#'   )
#' }
#'

mannwhitney____boot <- function(data, variable, by, conf_boot, nh_boot, alt_boot, nd, font_size, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND INFER WORKFLOW
  # =========================================================================
  
  
  df <- data.frame(rrr = data[[variable]],
                   ggg = data[[by]])
  
  df <- df %>% na.omit()
  #rrr <- data[[variable]]
  #ggg <- data[[by]]
  
  #stop(paste(df$ggg,collapse="; "))
  
  # Calculate the observed sample mean from the data.
  d_hat <- df %>% 
    infer::specify(response = rrr,
                   explanatory = ggg)  %>% 
    infer::calculate(stat = "diff in medians", order = levels(data[[by]]))
  
  # Generate a bootstrap distribution of sample means.
  # This is used to calculate the confidence interval.
  boot_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg) %>%
    infer::generate(reps = 1000, type = "bootstrap") %>%
    infer::calculate(stat = "diff in medians", order = levels(data[[by]]))
  
  # Generate a null distribution centered at the null hypothesis mean.
  # This is used to calculate the p-value.
  null_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg) %>%
    infer::hypothesize(null = "independence") %>%
    infer::generate(reps = 1000, type = "permute") %>%
    infer::calculate(stat = "diff in medians", order = levels(data[[by]]))
  
  # =========================================================================
  # 2. P-VALUE AND CONFIDENCE INTERVAL CALCULATION
  # =========================================================================
  
  # Create a ggplot object for the null distribution plot.
  plot_null <- infer::visualize(null_dist) +
    infer::shade_p_value(obs_stat = d_hat, direction = alt_boot)
  
  
  
  # Get the p-value based on the observed statistic and the null distribution.
  pv <- null_dist %>%
    infer::get_p_value(obs_stat = d_hat, direction = alt_boot)
  
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
  percentile_ci <- ndformat(percentile_ci, nd)
  percentile_ci <- paste("(", paste(percentile_ci, collapse = ", "), ")", sep = "")
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  
  # Create the data frame for the table.
  dframe <- data.frame(
    Pe = ndformat(d_hat,nd),
    Ci = percentile_ci,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Bootstrap<br> Diff in Medians",
    paste(conf_boot * 100, "% Bootstrap CI for &nu;<sub>1</sub> - &nu;<sub>2</sub><sup>1</sup>", sep = ""),
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
  
  
  fn1 <- "&nu;<sub>1</sub> - &nu;<sub>2</sub>: population difference in medians"
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  footnotes_html <- fn1
  # Define the HTML strings for footnotes based on the test type.
  if (testyn_boot) {
    fn2 <- dplyr::case_when(
      alt_boot == "greater" ~ paste("H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&gt;", nh_boot, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&lt;", nh_boot, sep = ""),
      alt_boot == "two.sided" ~ paste("H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&ne;", nh_boot, sep = "")
    )
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(footnotes_html, fn2)
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