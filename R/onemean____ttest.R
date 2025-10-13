#' @title Perform a One-Sample t-Test
#' @description This function conducts a one-sample t-test for a single mean. It
#'   calculates key statistics, including the mean, standard deviation, and standard
#'   error, and presents the results along with a confidence interval and optional
#'   p-value in a customizable HTML table.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A character string specifying the name of the numeric variable.
#' @param conf_ttest A numeric value between 0 and 1 specifying the confidence
#'   level for the confidence interval.
#' @param nh_ttest The numeric value of the null hypothesis mean (\eqn{\mu_0}).
#' @param alt_ttest A character string specifying the alternative hypothesis
#'   direction. Must be one of `"greater"`, `"less"`, or `"two.sided"`.
#' @param nd The number of decimal places for rounding the numeric output.
#' @param font_size The font size for the output HTML table.
#' @param imis A logical value. If \code{TRUE}, a column for the number of
#'   missing values is included in the table. Defaults to \code{FALSE}.
#' @param testyn_ttest A logical value. If \code{TRUE}, a p-value for the
#'   hypothesis test is included in the table. Defaults to \code{FALSE}.
#' @return A list containing one element:
#'   \item{table}{A \code{kableExtra} HTML table object with the test results.}
#' @details The function uses the standard \code{t.test()} from base R to perform
#'   the statistical analysis. It conditionally includes columns for missing values
#'   and the p-value based on the \code{imis} and \code{testyn_ttest} flags.
#' @seealso \code{\link[stats]{t.test}}
#' @importFrom stats t.test
#' @importFrom stats sd
#' @importFrom stats na.rm
#' @importFrom dplyr select if_else case_when
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @examples
#' # Create a sample dataset from a normal distribution with some missing values
#' set.seed(789)
#' sample_data <- tibble::tibble(
#'   my_var = c(rnorm(95, mean = 65, sd = 15), rep(NA, 5))
#' )
#'
#' # Example 1: Default usage with a two-sided test
#' # Includes missing value count and the p-value
#' onemean____ttest(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_ttest = 0.95,
#'   nh_ttest = 70,
#'   alt_ttest = "two.sided",
#'   nd = 2,
#'   font_size = 12,
#'   imis = TRUE,
#'   testyn_ttest = TRUE
#' )
#'
#' # Example 2: No hypothesis test and no missing value count
#' onemean____ttest(
#'   data = sample_data,
#'   variable = "my_var",
#'   conf_ttest = 0.99,
#'   nh_ttest = 70,
#'   alt_ttest = "less",
#'   nd = 3,
#'   font_size = 14,
#'   imis = FALSE,
#'   testyn_ttest = FALSE
#' )

onemean____ttest <- function(data, variable, conf_ttest, nh_ttest, alt_ttest, nd, font_size, imis = FALSE, testyn_ttest = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  # Extract the variable data from the data frame.
  df <- data.frame(rrr = data[[variable]])
  
  # Run the one-sample t-test.
  fit <- stats::t.test(df$rrr, mu = nh_ttest, conf.level = conf_ttest, alternative = alt_ttest)
  
  # Calculate and format descriptive statistics.
  me <- ndformat(mean(data[[variable]], na.rm = TRUE), nd)
  sd_val <- ndformat(stats::sd(data[[variable]], na.rm = TRUE), nd)
  esti <- paste(me, " (", sd_val, ")", sep = "")
  se <- ndformat(fit$stderr, nd + 1)
  
  # Calculate and format the confidence interval.
  out <- ndformat(fit$conf.int, nd)
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  
  defr <- fit$parameter
  tt <- ndformat(fit$statistic,2)
  # Format the p-value with boundary conditions.
  #out <- dplyr::if_else(fit$p.value < 0.001, "<0.001", as.character(round(fit$p.value, 3)))
  #pval <- dplyr::if_else(fit$p.value > 0.999, ">0.999", out)
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
    Mean = esti,
    Se = se,
    Ci = ci,
    Tt = tt,
    Df = defr,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Variable", "N", "Mis", "Mean (SD)", "SE Mean",
    paste(conf_ttest * 100, "% CI for &mu;<sup>1</sup>", sep = ""),
    "T-Value","DF",
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-Mis)
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
  }
  if (!testyn_ttest) {
    dframe <- dframe %>% dplyr::select(-Pval,-Tt,-Df)
    col_headers_html <- col_headers_html[!col_headers_html %in% c("T-Value","DF","P-value<sup>2</sup>")]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>One-sample t-test</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  if (testyn_ttest) {
    fn1 <- dplyr::if_else(alt_ttest == "two.sided", "Two-sided", "One-sided")
    fn2 <- dplyr::case_when(
      alt_ttest == "greater" ~ paste("H<sub>1</sub>:&#956;&gt; &#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest, sep = ""),
      alt_ttest == "less" ~ paste("H<sub>1</sub>:&#956;&lt;&#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest, sep = ""),
      alt_ttest == "two.sided" ~ paste("H<sub>1</sub>:&#956;&ne;&#956;<sub>0</sub>; &#956;<sub>0</sub>= ", nh_ttest, sep = "")
    )
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- dplyr::if_else(alt_ttest == "two.sided", "Two-sided", "One-sided")
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