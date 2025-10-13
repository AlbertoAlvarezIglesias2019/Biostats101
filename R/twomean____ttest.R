#' @title Perform a Two-Sample t-Test
#' @description This function conducts a two-sample t-test to compare the means of a
#'   numeric variable across two groups defined by a factor. It generates two separate
#'   HTML tables: one for group-wise summary statistics and another for the t-test
#'   inference results.
#'
#' @param data A data frame containing the numeric and grouping variables.
#' @param variable A character string specifying the name of the numeric variable.
#' @param by A character string specifying the name of the grouping variable. This
#'   variable must be a factor with exactly two levels.
#' @param conf_ttest A numeric value between 0 and 1, specifying the confidence
#'   level for the confidence interval.
#' @param alt_ttest A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two.sided"`.
#' @param ev A logical value. If \code{TRUE} (the default), a standard t-test assuming
#'   equal variances is performed. If \code{FALSE}, Welch's t-test is used.
#' @param nh_ttest A numeric value for the null hypothesis mean difference. Defaults
#'   to 0.
#' @param nd The number of decimal places for rounding the numeric output.
#' @param font_size The font size for the output HTML tables.
#' @param imis A logical value. If \code{TRUE}, a column for the number of
#'   missing values is included in the summary table. Defaults to \code{FALSE}.
#' @param testyn_ttest A logical value. If \code{TRUE}, the p-value is included
#'   in the inference table. Defaults to \code{FALSE}.
#' @return A list containing two `kableExtra` HTML table objects:
#'   \item{table_summ}{A table of group-wise summary statistics (N, mean, SD).}
#'   \item{table_infe}{A table of t-test inference results (difference, SE, CI, p-value).}
#' @details The function performs a two-sample t-test using \code{stats::t.test()} and
#'   \code{dplyr} to calculate group-wise statistics. It produces two distinct tables
#'   to separate the descriptive summary from the inferential results.
#' @note The grouping variable specified by `by` must be a factor with two levels.
#' @seealso \code{\link[stats]{t.test}}
#' @importFrom stats t.test
#' @importFrom stats sd
#' @importFrom dplyr select group_by summarise pull if_else case_when contains
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above column_spec row_spec footnote
#' @importFrom rlang .data all_of
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#'
#' @examples
#' # Create a sample dataset for a two-sample t-test
#' set.seed(123)
#' sample_data <- tibble::tibble(
#'   group = factor(rep(c("Group A", "Group B"), 50)),
#'   value = c(rnorm(50, mean = 20, sd = 3), rnorm(50, mean = 22, sd = 3))
#' )
#'
#' # Example 1: Basic two-sample t-test with a two-sided alternative
#' # Includes missing value count and p-value by default
#' twomean____ttest(
#'   data = sample_data,
#'   variable = "value",
#'   by = "group",
#'   conf_ttest = 0.95,
#'   alt_ttest = "two.sided",
#'   ev = TRUE,
#'   nd = 2,
#'   font_size = 12,
#'   imis = TRUE,
#'   testyn_ttest = TRUE
#' )
#'
#' # Example 2: One-sided t-test assuming non-equal variances, no missing/p-value
#' twomean____ttest(
#'   data = sample_data,
#'   variable = "value",
#'   by = "group",
#'   conf_ttest = 0.99,
#'   alt_ttest = "less",
#'   ev = FALSE,
#'   nd = 3,
#'   font_size = 14,
#'   imis = FALSE,
#'   testyn_ttest = TRUE
#' )
#' 

twomean____ttest <- function(data, variable, by, conf_ttest, alt_ttest, ev=TRUE,nh_ttest=0, nd, font_size, imis = FALSE, testyn_ttest = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(variable,by) ))
  
  var_data <- df[[variable]]
  by_data <- factor(df[[by]])
  

  # Run the one-sample t-test.
  fit <- stats::t.test(var_data ~ by_data, var.equal = ev,mu = nh_ttest,conf.level=conf_ttest,alternative = alt_ttest)
  
  # Calculate and format descriptive statistics.
  me <- round(mean(data[[variable]], na.rm = TRUE), nd)
  me <- df %>% group_by(.data[[by]]) %>% summarise(mean = mean(.data[[variable]],na.rm=TRUE)) %>% pull(mean)
  sd_val <- df %>% group_by(.data[[by]]) %>% summarise(sd = sd(.data[[variable]],na.rm=TRUE)) %>% pull(sd)
  esti <- paste(round(me,nd), " (", round(sd_val,nd), ")", sep = "")
  
  # Get counts for N and missing values.
  nn <- df %>% group_by(.data[[by]]) %>% summarise(nn = n()) %>% pull(nn)
  mm <- df %>% group_by(.data[[by]]) %>% summarise(mm = sum(is.na(.data[[variable]]))) %>% pull(mm)
  
  # Correct N if missing values are not to be included in the count.
  if (!imis) nn <- df %>% group_by(.data[[by]]) %>% summarise(mm = sum(!is.na(.data[[variable]]))) %>% pull(mm)
  
  # Calculate and format the confidence interval.
  diff <- ndformat(fit$estimate[1]-fit$estimate[2],nd)
  se <- ndformat(fit$stderr, nd + 1)
  out <- ndformat(fit$conf.int, nd)
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  defr <- fit$parameter
  tt <- round(fit$statistic,2)
  
  # Format the p-value with boundary conditions.
  pval <- pvformat(fit$p.value)


  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = variable,
    N1 = nn[1],
    Mis1 = mm[1],
    Mean1 = esti[1],
    N2 = nn[2],
    Mis2 = mm[2],
    Mean2 = esti[2]
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Variable", "N", "Mis", "Mean (SD)", "N", "Mis", "Mean (SD)"
  )
  
  # Conditionally remove columns for missing values and p-value.
  header_vector <- setNames(c(1, 3, 3), c(" ", levels(df[[by]])[1], levels(df[[by]])[2]))
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-contains("Mis") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
    header_vector <- setNames(c(1, 2, 2), c(" ", levels(df[[by]])[1], levels(df[[by]])[2]))
  }

  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Descriptive Statistics</p>",
    sep = ""
  )
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  if (imis) aa <- 1 else aa <- 0
  table_out_summ <- knitr::kable(
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
    add_header_above(header_vector)%>%
    # Add vertical borders and padding to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    )%>%
    # --- ADD THICKER VERTICAL LINES TO SEPARATE THE GROUPS ---
    # Add a thicker line after the first ungrouped column and after the first group (l1)
    kableExtra::column_spec(
      column = c(1, 3+aa,5+2*aa), 
      border_right = "3px solid #666" 
    ) %>%
    kableExtra::column_spec(
      column = 1, 
      border_left = "3px solid #666" 
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
    ) 
  
  
  
  
  # --- 4. BUILD THE Inference DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    diff = diff,
    Se = se,
    Ci = ci,
    Tt = tt,
    Df = defr,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Diff in means", "SE Diff",
    paste(conf_ttest * 100, "% CI for &mu;<sub>1</sub> - &mu;<sub>2</sub><sup>1</sup>", sep = ""),
    "T-Value","DF",
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!testyn_ttest) {
    dframe <- dframe %>% dplyr::select(-Pval,-Tt,-Df)
    col_headers_html <- col_headers_html[!col_headers_html %in% c("T-Value","DF","P-value<sup>2</sup>") ]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Two-sample t-test</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  if (testyn_ttest) {
    fn1 <- dplyr::if_else(alt_ttest == "two.sided", "Two-sided", "One-sided")
    fn2 <- dplyr::case_when(
      alt_ttest == "greater" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>1</sub>&gt;", nh_ttest, sep = ""),
      alt_ttest == "less" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>1</sub>&lt;", nh_ttest, sep = ""),
      alt_ttest == "two.sided" ~ paste("H<sub>1</sub>: &mu;<sub>1</sub> - &mu;<sub>1</sub>&ne;", nh_ttest, sep = "")
    )
    temp <- if_else(ev,"Equal variances","Non-equal variances")
    fn2 <- paste(fn2,"; (",temp,")",sep="")
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- dplyr::if_else(alt_ttest == "two.sided", "Two-sided", "One-sided")
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    footnotes_html <- fn1
  }
  
  # --- 5. BUILD THE KABLEEXTRA TABLE ---
  
  table_out_infe <- knitr::kable(
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
    )  %>%
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
  list(table_summ = table_out_summ,table_infe = table_out_infe)
}