#' @title Two-sample Proportions Test
#'
#' @description
#' Performs a two-sample proportions test using the normal approximation (equivalent to a Chi-squared test) and formats the output into professional HTML tables.
#'
#' @param data A data frame containing the variables for the analysis.
#' @param variable The name of the outcome variable (as a string). This should be a factor or a variable with two levels, where the first level is considered the event of interest.
#' @param by The name of the grouping variable (as a string). This must be a factor with exactly two levels.
#' @param conf_normal The confidence level for the interval, as a decimal (e.g., `0.95` for a 95% confidence interval).
#' @param alt_normal A string specifying the alternative hypothesis, one of `"two.sided"`, `"less"`, or `"greater"`.
#' @param yatc A logical value (`TRUE`/`FALSE`) indicating whether to apply Yates' continuity correction.
#' @param nd The number of decimal places for rounding the output.
#' @param font_size The font size for the output tables (in pixels).
#' @param imis A logical value (`TRUE`/`FALSE`). If `TRUE`, missing values are counted and displayed in the descriptive table; otherwise, they are omitted.
#' @param testyn_normal A logical value (`TRUE`/`FALSE`). If `TRUE`, the inference table with test statistics and P-value is included in the output.
#'
#' @details
#' This function uses `prop.test()` from base R to perform the statistical test. The data is prepared by first subsetting based on the `by` variable, and then all `NA` values in both the grouping and outcome variables are removed using `na.omit()`.
#'
#' The function produces two HTML tables formatted using `kableExtra`:
#' - **Descriptive Statistics Table**: Summarizes the sample sizes ($N$), missing values ($Mis$), and proportions ($\% (n)$) for each of the two groups.
#' - **Test for Two Proportions Table**: Provides the difference in proportions, confidence interval, $\chi^2$ statistic, degrees of freedom, and P-value.
#'
#' @return A list containing two `kableExtra` HTML table objects:
#'   - `table_summ`: An HTML object representing the descriptive statistics table.
#'   - `table_infe`: An HTML object representing the test for two proportions table.
#'
#' @seealso [prop.test()], [dplyr::mutate()], [kableExtra::kable_styling()]
#'
#' @examples
#' # A simple, runnable example
#'
#' # Create a dummy data frame
#' dummy_data <- data.frame(
#'   outcome = sample(factor(c("Yes", "No", "Yes", "No", "Yes", "Yes", "No", "No", "Yes", "No")),size=20,replace = TRUE),
#'   group = sample(factor(c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B")),size=20,replace = TRUE)
#' )
#'
#' # Run the function with sample parameters
#' results <- twoprop____normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   yatc = TRUE,
#'   nd = 2,
#'   font_size = 14,
#'   imis = FALSE,
#'   testyn_normal = TRUE
#' )
#'
#' # Access the returned tables
#' if (requireNamespace("knitr", quietly = TRUE) && requireNamespace("kableExtra", quietly = TRUE)) {
#'   print(results$table_summ)
#'   print(results$table_infe)
#' }
#' 
#'
#'twoprop____normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   yatc = TRUE,
#'   nd = 2,
#'   font_size = 14,
#'   imis = FALSE,
#'   meth = "asymptotic",
#'   testyn_normal = TRUE
#' ) 
#' 
#' twoprop____normal(
#'   data = dummy_data,
#'   variable = "outcome",
#'   by = "group",
#'   conf_normal = 0.95,
#'   alt_normal = "two.sided",
#'   yatc = TRUE,
#'   nd = 2,
#'   font_size = 14,
#'   imis = FALSE,
#'   meth = "score",
#'   testyn_normal = TRUE
#' ) 

twoprop____normal <- function(data, variable, by, conf_normal, alt_normal, yatc, nd, font_size, imis = FALSE,meth="asymptotic", testyn_normal = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN TEST ---
  
  df <- data.frame(outcome = data[[variable]],
                   group = data[[by]])
  misgroup <- sum(is.na(df$group))
  df <- df %>% filter(!is.na(group))
  
  grol <- levels(df$group)[1]
  refl <- levels(df$outcome)[1]
  misoutcome <- df %>% group_by(group) %>% summarise(m = sum(is.na(outcome))) %>% ungroup() 
  
  df <- df %>% 
    na.omit() %>% 
    group_by(group) %>% 
    summarise(n=n(),
              x = sum(outcome == refl) ) %>% 
    dplyr::ungroup()
  
  df <- df %>% left_join(misoutcome)
  
  x1 <- df %>% filter(group==grol) %>% pull(x)
  x2 <- df %>% filter(!group==grol) %>% pull(x)
  n1 <- df %>% filter(group==grol) %>% pull(n)
  n2 <- df %>% filter(!group==grol) %>% pull(n)
  
  # Calculate and format descriptive statistics.
  cc <- df$x
  pp <- round(df$x/df$n*100,nd)
  pp <- paste(pp,"%",sep="")
  esti <- paste(pp, " (", cc, ")", sep = "")
  
  # Get counts for N and missing values.
  nn <- df$n
  mm <- df$m
  
  # Correct N if missing values are not to be included in the count.
  if (!imis) nn <- nn - mm
  
  
  # Run the test
  fit <- prop.test(c(x1,x2),c(n1,n2),conf.level=conf_normal,alternative = alt_normal,correct = yatc)

  # Calculate and format the confidence interval.
  diff <- paste(ndformat( (fit$estimate[1]-fit$estimate[2])*100,nd),"%",sep="")
  out <- paste(ndformat(fit$conf.int*100, nd),"%",sep="")
  out <- paste(out, collapse = ", ")
  ci <- paste("(", out, ")", sep = "")
  
  
  if (meth == "asymptotic") {
    ffiitt <- wald_prop_diff_ci(x1,n1,x2,n2,conf.level=conf_normal,alternative= alt_normal)
    out <- paste(ndformat(c(ffiitt$Lower_CI,ffiitt$Upper_CI)*100, nd),"%",sep="")
    out <- paste(out, collapse = ", ")
    ci <- paste("(", out, ")", sep = "")
    ssee <- paste(ndformat(ffiitt$SE*100,nd),"%",sep="")
  } else {ssee <- NA}
  

  
  # Calculate DF and test statistic
  defr <- fit$parameter
  tt <- round(sqrt(fit$statistic),2)
  
  # Format the p-value with boundary conditions.
  pv <- fit$p.value
  pval <- pvformat(pv)

  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = refl,
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
    variable, "N", "Mis", "% (n)", "N", "Mis", "% (n)"
  )
  
  # Conditionally remove columns for missing values and p-value.
  header_vector <- setNames(c(1, 3, 3), c(" ", levels(data[[by]])[1], levels(data[[by]])[2]))
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-contains("Mis") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
    header_vector <- setNames(c(1, 2, 2), c(" ", levels(data[[by]])[1], levels(data[[by]])[2]))
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
    kableExtra::add_header_above(header_vector)%>%
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
    Se = ssee,
    Ci = ci,
    Tt = tt,
    #Df = defr,
    Pval = pval
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Diff","SE Diff", 
    paste(conf_normal * 100, "% CI for p<sub>1</sub> - p<sub>2</sub><sup>1</sup>", sep = ""),
    "Z",#"DF",
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!testyn_normal) {
    dframe <- dframe %>% dplyr::select(-Pval,-Tt)
    col_headers_html <- col_headers_html[!col_headers_html %in% c("Z","P-value<sup>2</sup>") ]
  }
  
  # Conditionally remove columns for not showing SE.
  if (!meth == "asymptotic") {
    dframe <- dframe %>% dplyr::select(-contains("Se") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "SE Diff"]
  }
  
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Test for Two Proportions</p>",
    sep = ""
  )
  
  ffnn1 <- dplyr::if_else(meth == "asymptotic","Interval method: Wald","Interval method: Score")
  # Define the HTML strings for footnotes based on test type.
  if (testyn_normal) {
    fn1 <- ffnn1
    fn2 <- dplyr::case_when(
      alt_normal == "greater" ~ c("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&gt; 0"),
      alt_normal == "less" ~ c("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&lt; 0"),
      alt_normal == "two.sided" ~ c("Normal approximation; H<sub>1</sub>:p<sub>1</sub> - p<sub>2</sub>&ne; 0")
    )
    temp <- if_else(yatc,"with Yates correction","")
    fn2 <- paste(fn2,"; (",temp,")",sep="")
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    fn1 <- ffnn1
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