#' @title Two-Sample Bootstrap Inference
#' @description This function performs bootstrap analysis to calculate a
#'   confidence interval for the difference in means between two groups and a
#'   p-value for a permutation test. It is designed to be a robust alternative
#'   to a standard t-test, particularly when assumptions of normality are
#'   violated. The analysis leverages the `infer` package for its clear and
#'   principled framework.
#'
#' @param data A data frame containing the variables for the analysis.
#' @param variable A character string specifying the name of the numeric
#'   variable of interest.
#' @param by A character string specifying the name of the grouping factor
#'   variable. This factor must have exactly two levels.
#' @param conf_boot A numeric value between 0 and 1 specifying the confidence
#'   level for the bootstrap confidence interval.
#' @param nh_boot A numeric value representing the null hypothesis mean difference.
#'   Defaults to 0, representing the null hypothesis of no difference between
#'   group means.
#' @param alt_boot A character string specifying the alternative hypothesis,
#'   must be one of "two-sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for rounding
#'   the results.
#' @param font_size A numeric value for the font size of the output table.
#' @param testyn_boot A logical value; if `TRUE`, a p-value and the
#'   corresponding alternative hypothesis footnote are included in the table.
#'   Defaults to `FALSE`.
#'
#' @return A list containing three elements:
#' \item{table}{A `kableExtra` object representing the formatted results table.}
#' \item{plot_null}{A `ggplot` object visualizing the null distribution and
#'   the observed statistic.}
#' \item{plot_interval}{A `ggplot` object visualizing the bootstrap distribution
#'   and the percentile confidence interval.}
#'
#' @details The function first prepares the data in a format suitable for the
#'   `infer` package. It then generates a bootstrap distribution to compute a
#'   percentile-based confidence interval. For the p-value, it creates a
#'   permutation-based null distribution under the assumption of independence
#'   between the groups. The results are presented in a professionally styled
#'   `kableExtra` table with conditional columns and footnotes, along with
#'   visualizations of the distributions.
#'
#' @importFrom dplyr if_else select
#' @importFrom infer specify calculate generate hypothesize visualize shade_p_value get_p_value get_confidence_interval
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom knitr kable
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot

#' @examples
#' # For this example, we'll create a dummy data frame.
#' # In a Jamovi context, 'data' would come from the user's dataset.
#' set.seed(42)
#' my_data <- data.frame(
#'   Response = c(rnorm(50, 10, 2), rnorm(50, 12, 2)),
#'   Group = factor(c(rep("Group 1", 50), rep("Group 2", 50)))
#' )
#'
#' # Run the bootstrap inference with a two-sided test
#' boot_results <- twomean____boot(
#'   data = my_data,
#'   variable = "Response",
#'   by = "Group",
#'   conf_boot = 0.95,
#'   nh_boot = 0,
#'   alt_boot = "two-sided",
#'   nd = 3,
#'   font_size = 12,
#'   testyn_boot = TRUE
#' )
#'
#' # Display the resulting table and plots
#' boot_results$table
#' print(boot_results$plot_null)
#' print(boot_results$plot_interval)
#'
#' # Run the function with only the confidence interval
#' boot_ci_only <- twomean____boot(
#'   data = my_data,
#'   variable = "Response",
#'   by = "Group",
#'   conf_boot = 0.99,
#'   nh_boot = 0,
#'   alt_boot = "two-sided",
#'   nd = 2,
#'   font_size = 12,
#'   testyn_boot = FALSE
#' )
#'
#' boot_ci_only$table
#' 
twomean____boot <- function(data, variable, by, conf_boot, nh_boot, alt_boot, nd, font_size, testyn_boot = FALSE) {
  
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
    infer::calculate(stat = "diff in means", order = levels(data[[by]]))
  
  # Generate a bootstrap distribution of sample means.
  # This is used to calculate the confidence interval.
  boot_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg) %>%
    infer::generate(reps = 1000, type = "bootstrap") %>%
    infer::calculate(stat = "diff in means", order = levels(data[[by]]))
  
  # Generate a null distribution centered at the null hypothesis mean.
  # This is used to calculate the p-value.
  null_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg) %>%
    infer::hypothesize(null = "independence") %>%
    infer::generate(reps = 1000, type = "permute") %>%
    infer::calculate(stat = "diff in means", order = levels(data[[by]]))
  
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
  col_headers_html <- c("Bootstrap<br> Diff in Means",
    paste(conf_boot * 100, "% Bootstrap CI for &mu;<sub>1</sub> - &mu;<sub>2</sub><sup>1</sup>", sep = ""),
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
      alt_boot == "greater" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&gt;", nh_boot, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&lt;", nh_boot, sep = ""),
      alt_boot == "two-sided" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&ne;", nh_boot, sep = "")
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