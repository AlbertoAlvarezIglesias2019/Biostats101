#' @title Perform a Mann-Whitney U Test and Generate an HTML Table
#' @description This function performs a Mann-Whitney U test (also known as the Wilcoxon
#'   Rank-Sum test) to compare the difference in location between two groups. It
#'   calculates a confidence interval and p-value, and then formats the results into a
#'   styled HTML table using `kableExtra`.
#'
#' @details This test is the non-parametric alternative to an independent samples t-test.
#'   It is used to test for a difference between the population medians of two independent
#'   groups. The function is designed for use in a Jamovi module, which is why it returns
#'   a list containing a `kableExtra` table object.
#'
#' @param data A data frame containing the variables to be tested.
#' @param variable A character string specifying the name of the dependent variable to analyze.
#' @param by A character string specifying the name of the independent variable used to
#'   create the two groups.
#' @param conf_test A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param nh_test A numeric value for the null hypothesis difference in location. This is
#'   typically 0.
#' @param alt_test A character string specifying the alternative hypothesis direction.
#'   Can be "two.sided", "greater", or "less".
#' @param nd An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param imis A logical value; if `TRUE`, missing values are included in the total 'N' count.
#'   If `FALSE`, they are excluded from the `N` count.
#' @param testyn_test A logical value; if `TRUE`, the W-statistic and P-value columns are
#'   included in the output table.
#'
#' @return A `list` containing one element: `table`, which is a `kableExtra` HTML table object
#'   that summarizes the results of the Mann-Whitney U test.
#'
#' @examples
#' # Create example data
#' data_mannwhitney <- data.frame(
#'   group = factor(c("A", "A", "A", "B", "B", "B")),
#'   value = c(23, 25, 22, 28, 30, 27)
#' )
#'
#' # Example 1: Basic usage with a two-sided test and 95% confidence
#' mannwhitney____test(
#'   data = data_mannwhitney,
#'   variable = "value",
#'   by = "group",
#'   conf_test = 0.95,
#'   nh_test = 0,
#'   alt_test = "two.sided",
#'   nd = 3
#' )
#'
#' # Example 2: Exclude the test statistics and p-value
#' mannwhitney____test(
#'   data = data_mannwhitney,
#'   variable = "value",
#'   by = "group",
#'   conf_test = 0.95,
#'   nh_test = 0,
#'   alt_test = "two.sided",
#'   nd = 3,
#'   testyn_test = FALSE
#' )
#'
#' # Example 3: One-sided test
#' mannwhitney____test(
#'   data = data_mannwhitney,
#'   variable = "value",
#'   by = "group",
#'   conf_test = 0.95,
#'   nh_test = 0,
#'   alt_test = "less",
#'   nd = 3,
#'   testyn_test = TRUE
#' )
#' 
#' 

mannwhitney____test <- function(data, variable,by, conf_test, nh_test, alt_test, nd, font_size=16, imis = FALSE, testyn_test = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  # Extract the variable data from the data frame.
  df <- data.frame(rrr = data[[variable]],
                   ggg = factor(data[[by]]))
  

  # Calculate and format descriptive statistics.
  me <- df %>% dplyr::group_by(ggg) %>% summarise(medi = median(rrr,na.rm=TRUE)) %>% pull(medi)
  iqr <- df %>% group_by(ggg) %>% summarise(iqr = IQR(rrr,na.rm=TRUE)) %>% pull(iqr)
  esti <- paste(ndformat(me,nd), " (", ndformat(iqr,nd), ")", sep = "")
  
  # Get counts for N and missing values.
  nn <- df %>% group_by(ggg) %>% summarise(nn = n()) %>% pull(nn)
  mm <- df %>% group_by(ggg) %>% summarise(mm = sum(is.na(rrr))) %>% pull(mm)
  
  # Correct N if missing values are not to be included in the count.
  if (!imis) nn <- nn - mm
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = variable,
    N1 = nn[1],
    Mis1 = mm[1],
    Median1 = esti[1],
    N2 = nn[2],
    Mis2 = mm[2],
    Median2 = esti[2]
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Variable", "N", "Mis", "Median (IQR)", "N", "Mis", "Median (IQR)"
  )
  
  # Conditionally remove columns for missing values and p-value.
  header_vector <- setNames(c(1, 3, 3), c(" ", levels(df$ggg)[1], levels(df$ggg)[2]))
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-contains("Mis") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
    header_vector <- setNames(c(1, 2, 2), c(" ", levels(df$ggg)[1], levels(df$ggg)[2]))
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
  
  
  
  
  
  
  
  # Run the one-sample t-test.
  fit <- suppressWarnings(stats::wilcox.test(rrr~ggg,data = df, mu = nh_test, conf.level = conf_test, alternative = alt_test, conf.int = TRUE))
  
  # Calculate and format descriptive statistics.
  esti <- ndformat(fit$estimate,nd) 

  # Calculate and format the confidence interval.
  out <- ndformat(fit$conf.int,nd)   
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  st <- fit$statistic
  
  # Format the p-value with boundary conditions.
  pval <- pvformat(fit$p.value)


  # --- 2. BUILD THE TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Median = esti,
    Ci = ci,
    St = st,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Difference<br> in Location", paste(conf_test * 100, "% CI for &nu;<sub>1</sub> - &nu;<sub>2</sub><sup>1</sup>", sep = ""),
    "W-statistic","P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!testyn_test) {
    dframe <- dframe %>% dplyr::select(-Pval,-St)
    col_headers_html <- col_headers_html[!col_headers_html %in% c("W-statistic","P-value<sup>2</sup>")]
  }
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Mann-Whitney Test</p>",
    sep = ""
  )
  
  fn1 <- dplyr::if_else(alt_test == "two.sided",
                        "&nu;<sub>1</sub> - &nu;<sub>2</sub>: population difference in location (Two-sided interval)",
                        "&nu;<sub>1</sub> - &nu;<sub>2</sub>: population difference in location (One-sided interval)")
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  footnotes_html <- fn1
  # Define the HTML strings for footnotes based on test type.
  if (testyn_test) {
    fn2 <- dplyr::case_when(
      alt_test == "greater" ~ paste(fit$method," H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&gt;", nh_test, sep = ""),
      alt_test == "less" ~ paste(fit$method," H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&lt;", nh_test, sep = ""),
      alt_test == "two.sided" ~ paste(fit$method," H<sub>1</sub>: &nu;<sub>1</sub> - &nu;<sub>2</sub>&ne;", nh_test, sep = "")
    )
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(footnotes_html, fn2)
  } 
  
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
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