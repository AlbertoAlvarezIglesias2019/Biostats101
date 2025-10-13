#' @title Perform a Two-Proportion Bootstrap Test and Generate an HTML Table
#' @description This function uses the `infer` package to perform a two-proportion bootstrap
#'   test and then formats the results into a styled HTML table using `kableExtra`.
#'   It calculates a confidence interval and, optionally, a p-value, and also returns
#'   the plots for the null and bootstrap distributions.
#'
#' @param data A data frame containing the variables for the test.
#' @param variable A variable from the `data` frame to use as the response. Must be unquoted.
#' @param by A variable from the `data` frame to use as the explanatory variable. Must be unquoted.
#' @param conf_boot A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_boot A character string specifying the alternative hypothesis direction. Can be "two-sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for the confidence interval.
#' @param font_size An integer to set the font size of the table.
#' @param testyn_boot A logical value; if `TRUE`, a p-value column is included in the output table.
#'
#' @return A `list` containing three elements: `table`, which is a `kableExtra` HTML table object,
#'   `plot_null`, a ggplot object for the null distribution plot, and `plot_interval`, a ggplot
#'   object for the bootstrap confidence interval plot.
#'
#' @examples
#' # Load necessary libraries
#' library(dplyr)
#' library(infer)
#' library(gtsummary)
#' library(knitr)
#' library(kableExtra)
#'
#' # Example 1: Calculate the confidence interval and p-value (default)
#' # This example uses the 'trial' dataset from gtsummary.
#' trial$response <- as.factor(trial$response)
#' trial$trt <- as.factor(trial$trt)
#' tbl <- twoprop____boot(data = trial,
#'                 variable = "response",
#'                 by = "trt",
#'                 conf_boot = 0.95,
#'                 alt_boot = "two-sided",
#'                 nd = 3)
#' tbl$table
#' tbl$plot_null
#' tbl$plot_interval
#'
#' # Example 2: Calculate only the confidence interval
#' tbl <- twoprop____boot(data = trial,
#'                 variable = "response",
#'                 by = "trt",
#'                 conf_boot = 0.95,
#'                 alt_boot = "two-sided",
#'                 nd = 3,
#'                 testyn_boot = FALSE)
#' tbl$table
#' tbl$plot_null
#' tbl$plot_interval
#' 
#' 
#' # Example 3: Set a different null hypothesis and alternative direction
#' tbl <- twoprop____boot(data = trial,
#'                 variable = "grade",
#'                 by = "trt",
#'                 conf_boot = 0.95,
#'                 alt_boot = "greater",
#'                 nd = 3,testyn_boot = TRUE)
#' tbl$table
#' tbl$plot_null
#' tbl$plot_interval
#'                 
#'                 
twoprop____boot <- function(data, variable, by, conf_boot, alt_boot, nd, font_size=16, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION AND INFER WORKFLOW
  # =========================================================================
  

  df <- data.frame(rrr = data[[variable]],
                   ggg = data[[by]])
  
  df <- df %>%
    dplyr::mutate(temp = if_else(rrr==levels(data[[variable]])[1],rrr,paste("No-",levels(data[[variable]])[1],sep=""))) %>% 
    dplyr::select(rrr = temp,ggg)
    
  
  df <- df %>% na.omit()
  #rrr <- data[[variable]]
  #ggg <- data[[by]]
  
  #stop(paste(df$ggg,collapse="; "))
  
  # Calculate the observed sample mean from the data.
  d_hat <- df %>% 
    infer::specify(response = rrr,
                   explanatory = ggg,
                   success = levels(data[[variable]])[1])  %>% 
    infer::calculate(stat = "diff in props", order = levels(data[[by]]))
  
  # Generate a bootstrap distribution of sample proportions
  # This is used to calculate the confidence interval.
  boot_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg,
                   success = levels(data[[variable]])[1]) %>%
    infer::generate(reps = 1000, type = "bootstrap") %>%
    infer::calculate(stat = "diff in props", order = levels(data[[by]]))
  
  # Generate a null distribution centered at the null hypothesis mean.
  # This is used to calculate the p-value.
  null_dist <- df %>%
    infer::specify(response = rrr,
                   explanatory = ggg,
                   success = levels(data[[variable]])[1]) %>%
    infer::hypothesize(null = "independence") %>%
    infer::generate(reps = 1000, type = "permute") %>%
    infer::calculate(stat = "diff in props", order = levels(data[[by]]))
  
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
  number <- round(pv, 3)
  pvalue <- dplyr::if_else(pv < 0.001, "<0.001", sprintf("%.3f", number))
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
  percentile_ci <- paste(round(percentile_ci*100, nd),"%",sep="")
  percentile_ci <- paste("(", paste(percentile_ci, collapse = ", "), ")", sep = "")
  
  # =========================================================================
  # 3. BUILD THE TABLE DATA AND HTML STRINGS
  # =========================================================================
  
  # Create the data frame for the table.
  dframe <- data.frame(
    Ci = percentile_ci,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    paste(conf_boot * 100, "% Bootstrap CI for p<sub>1</sub> - p<sub>2</sub><sup>1</sup>", sep = ""),
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
      alt_boot == "greater" ~ paste("H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&gt; ", 0, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&lt; ", 0, sep = ""),
      alt_boot == "two-sided" ~ paste("H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&ne; ", 0, sep = "")
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