#' @title Two-Sample Bootstrap Inference (Base R Version)
#' @description This function performs bootstrap analysis to calculate a
#'    confidence interval for the difference in means between two groups and a
#'    p-value for a permutation test using base R and ggplot2, avoiding jamovi formula sandbox errors.
#'
#' @param data A data frame containing the variables for the analysis.
#' @param variable A character string specifying the name of the numeric
#'    variable of interest.
#' @param by A character string specifying the name of the grouping factor
#'    variable. This factor must have exactly two levels.
#' @param conf_boot A numeric value between 0 and 1 specifying the confidence
#'    level for the bootstrap confidence interval.
#' @param nh_boot A numeric value representing the null hypothesis mean difference.
#'    Defaults to 0.
#' @param alt_boot A character string specifying the alternative hypothesis,
#'    must be one of "two-sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for rounding.
#' @param font_size A numeric value for the font size of the output table.
#' @param testyn_boot A logical value; if TRUE, includes p-value in the table.
#'
#' @return A list containing three elements: table, plot_null, and plot_interval.
#'
#' @importFrom dplyr if_else select
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom knitr kable
#' @importFrom stats setNames quantile
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline labs theme_minimal
#'
twomean____boot <- function(data, variable, by, conf_boot, nh_boot, alt_boot, nd, font_size, testyn_boot = FALSE) {
  
  # =========================================================================
  # 1. DATA PREPARATION & OBSERVED STATISTIC
  # =========================================================================
  
  df <- data.frame(
    rrr = data[[variable]],
    ggg = as.factor(data[[by]])
  )
  df <- stats::na.omit(df)
  
  lvl <- levels(df$ggg)
  if (length(lvl) != 2) {
    stop("The grouping variable must have exactly two levels.")
  }
  
  # Calculate observed difference in means (Group 1 - Group 2)
  mean1 <- mean(df$rrr[df$ggg == lvl[1]])
  mean2 <- mean(df$rrr[df$ggg == lvl[2]])
  d_hat <- mean1 - mean2
  
  # =========================================================================
  # 2. BOOTSTRAP & PERMUTATION DISTRIBUTIONS (BASE R)
  # =========================================================================
  
  reps <- 1000
  n_rows <- nrow(df)
  
  boot_diffs <- numeric(reps)
  null_diffs <- numeric(reps)
  
  # Set seed for reproducibility if desired, or let user control
  set.seed(42)
  
  for (i in 1:reps) {
    # Bootstrap sample (sampling rows with replacement)
    boot_df <- df[sample(n_rows, n_rows, replace = TRUE), ]
    m1_b <- mean(boot_df$rrr[boot_df$ggg == lvl[1]])
    m2_b <- mean(boot_df$rrr[boot_df$ggg == lvl[2]])
    boot_diffs[i] <- m1_b - m2_b
    
    # Permutation sample (shuffling group labels)
    perm_ggg <- sample(df$ggg)
    m1_p <- mean(df$rrr[perm_ggg == lvl[1]])
    m2_p <- mean(df$rrr[perm_ggg == lvl[2]])
    null_diffs[i] <- m1_p - m2_p
  }
  
  # Shift null distribution to align with null hypothesis mean difference (nh_boot)
  null_diffs_centered <- null_diffs + nh_boot
  
  # =========================================================================
  # 3. P-VALUE & CONFIDENCE INTERVAL CALCULATIONS
  # =========================================================================
  
  # Calculate P-value based on alternative hypothesis direction
  if (alt_boot == "two-sided") {
    obs_dev <- abs(d_hat - nh_boot)
    null_dev <- abs(null_diffs_centered - nh_boot)
    pv <- mean(null_dev >= obs_dev)
  } else if (alt_boot == "greater") {
    pv <- mean(null_diffs_centered >= d_hat)
  } else if (alt_boot == "less") {
    pv <- mean(null_diffs_centered <= d_hat)
  } else {
    pv <- 1
  }
  
  pvalue <- pvformat(pv)
  
  # Calculate percentile confidence interval
  alpha <- 1 - conf_boot
  percentile_ci_vals <- stats::quantile(boot_diffs, probs = c(alpha / 2, 1 - (alpha / 2)))
  
  # Generate plots using ggplot2
  plot_null_df <- data.frame(stat = null_diffs_centered)
  plot_null <- ggplot2::ggplot(plot_null_df, ggplot2::aes(x = stat)) +
    ggplot2::geom_histogram(bins = 30, fill = "lightblue", color = "white") +
    ggplot2::geom_vline(xintercept = d_hat, color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::labs(title = "Null Distribution", x = "Difference in Means", y = "Count") +
    ggplot2::theme_minimal()
  
  plot_boot_df <- data.frame(stat = boot_diffs)
  plot_interval <- ggplot2::ggplot(plot_boot_df, ggplot2::aes(x = stat)) +
    ggplot2::geom_histogram(bins = 30, fill = "lightgreen", color = "white") +
    ggplot2::geom_vline(xintercept = percentile_ci_vals[1], color = "blue", linetype = "dotted", linewidth = 1) +
    ggplot2::geom_vline(xintercept = percentile_ci_vals[2], color = "blue", linetype = "dotted", linewidth = 1) +
    ggplot2::labs(title = "Bootstrap Distribution", x = "Difference in Means", y = "Count") +
    ggplot2::theme_minimal()
  
  # Format confidence interval into text string
  formatted_ci <- ndformat(percentile_ci_vals, nd)
  percentile_ci_str <- paste("(", paste(formatted_ci, collapse = ", "), ")", sep = "")
  
  # =========================================================================
  # 4. BUILD TABLE & KABLEEXTRA OUTPUT
  # =========================================================================
  
  dframe <- data.frame(
    Pe = ndformat(d_hat, nd),
    Ci = percentile_ci_str,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  col_headers_html <- c(
    "Bootstrap<br> Diff in Means",
    paste(conf_boot * 100, "% Bootstrap CI for &mu;<sub>1</sub> - &mu;<sub>2</sub><sup>1</sup>", sep = ""),
    "P-value<sup>2</sup>"
  )
  
  if (!testyn_boot) {
    dframe <- dframe %>% dplyr::select(-Pval)
    col_headers_html <- col_headers_html[!col_headers_html %in% "P-value<sup>2</sup>"]
  }
  
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Bootstrap inference</p>",
    sep = ""
  )
  
  if (testyn_boot) {
    fn1 <- "Based on percentiles"
    fn2 <- dplyr::case_when(
      alt_boot == "greater" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&gt;", nh_boot, sep = ""),
      alt_boot == "less" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&lt;", nh_boot, sep = ""),
      alt_boot == "two-sided" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>2</sub>&ne;", nh_boot, sep = "")
    )
    footnotes_html <- c(paste("<i>", fn1, "</i>", sep = ""), paste("<i>", fn2, "</i>", sep = ""))
  } else {
    footnotes_html <- paste("<i>Based on percentiles</i>", sep = "")
  }
  
  table_out <- knitr::kable(
    dframe,
    format = "html",
    align = "c",
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) %>%
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd; padding-left: 10px; padding-right: 10px;"
    ) %>%
    kableExtra::row_spec(
      row = 1,
      extra_css = "border-bottom: 2px solid #666; border-top: 1px solid #ddd;"
    ) %>%
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  list(table = table_out, plot_null = plot_null, plot_interval = plot_interval)
}