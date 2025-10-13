#' @title Perform a One-Proportion Test and Generate an HTML Table
#' @description This function performs a one-proportion test, calculating a confidence interval
#'   using the `binom` package and a p-value using the `stats` package. The results are
#'   formatted into a styled HTML table using `kableExtra`.
#'
#' @param data A data frame containing the variables for the test.
#' @param variable A variable from the `data` frame to use as the response. Must be unquoted.
#' @param conf_normal A numeric value specifying the confidence level for the interval (e.g., 0.95).
#' @param alt_normal A character string specifying the alternative hypothesis direction.
#'   Can be "two.sided", "greater", or "less".
#' @param nh_normal A numeric value for the null hypothesis proportion (p0).
#' @param nd An integer specifying the number of decimal places for the output.
#' @param font_size An integer to set the font size of the table.
#' @param imis A logical value; if `TRUE`, missing values are included in the total 'N' count.
#'   If `FALSE`, they are excluded.
#' @param meth A character string specifying the confidence interval method to be used by
#'   `binom::binom.confint()`. Options include "exact", "ac", "asymptotic", and "wilson".
#' @param testyn_normal A logical value; if `TRUE`, a p-value column is included in the output table.
#'
#' @return A `list` containing one element: `table`, which is a `kableExtra` HTML table object
#'   that summarizes the results of the one-proportion test.
#'
#' @examples
#' # Load necessary libraries
#' library(dplyr)
#' library(knitr)
#' library(kableExtra)
#' library(binom)
#' library(gtsummary)
#'
#' # Example 1: Basic usage with default settings on gtsummary's trial dataset
#' # This calculates the 95% CI and a two-sided p-value for the 'response' variable.
#' trial$response <- factor(trial$response,levels = c("1","0"))
#' oneprop____normal(data = trial,
#'                  variable = "response",
#'                  conf_normal = 0.95,
#'                  alt_normal = "two.sided",
#'                  nh_normal = 0.5,
#'                  nd = 3,
#'                  font_size = 16,
#'                  meth = "asymptotic")
#'
#' # Example 2: Exclude p-value and change CI method
#' # This shows how to calculate only the CI using the "exact" method.
#' oneprop____normal(data = trial,
#'                  variable = "response",
#'                  conf_normal = 0.95,
#'                  alt_normal = "two.sided",
#'                  nh_normal = 0.5,
#'                  nd = 3,
#'                  font_size = 12,
#'                  testyn_normal = FALSE,
#'                  meth = "exact")
#'
#' # Example 3: Different null hypothesis and alternative direction
#' # This example uses a one-sided test with a different null proportion.
#' oneprop____normal(data = trial,
#'                  variable = "response",
#'                  conf_normal = 0.95,
#'                  alt_normal = "greater",
#'                  nh_normal = 0.6,
#'                  nd = 3,
#'                  font_size = 12,
#'                  imis = FALSE,
#'                  testyn_normal = TRUE,
#'                  meth = "exact")
#'                  
#' oneprop____normal(data = trial,
#'                  variable = "response",
#'                  conf_normal = 0.95,
#'                  alt_normal = "greater",
#'                  nh_normal = 0.6,
#'                  nd = 3,
#'                  font_size = 12,
#'                  imis = FALSE,
#'                  testyn_normal = TRUE,
#'                  meth = "asymptotic")
#'                  
#'                  
oneprop____normal <- function(data, variable, conf_normal, alt_normal,nh_normal, nd, font_size, imis = FALSE,meth,testyn_normal=FALSE) {
  
  # --- 1. PREPARE DATA AND RUN TEST ---
  
  var_data <- data[[variable]]
  var_data <- factor(var_data)
  levelchosen <- levels(data[[variable]])[1]
  xx <- sum(var_data==levelchosen,na.rm=TRUE)
  nn <- length(var_data)
  mm <- length(var_data[is.na(var_data)])
  
  
  fit <- binom::binom.confint(xx,nn-mm,conf.level=conf_normal,methods = meth)
  
  if (meth == "asymptotic") {
    df <- nn-mm -1
    #z_score <- qnorm(1 - (1-conf_normal)/2)
    #ssee <- (fit$upper - fit$lower) / (2 * z_score)
    t_score <-  qt(1 - (1-conf_normal)/2, df)
    ssee <- (fit$upper - fit$lower) / (2 * t_score)
    ssee <- paste(ndformat(ssee*100,nd),"%",sep="")
  } else {ssee <- NA}
  
  ci <- paste("(",ndformat(fit$lower*100,nd),"%, ",ndformat(fit$upper*100,nd),"%)",sep="")
  number <- ndformat(xx/(nn-mm)*100,nd)
  sp <- paste(number,"%",sep="")
  fit <- stats::binom.test(xx,nn-mm,conf.level=conf_normal,alternative=alt_normal,p=nh_normal)
  pv <- fit$p.value
  #number <- round(pv, 3)
  #pvalue <- dplyr::if_else(pv < 0.001, "<0.001", sprintf("%.3f", number))
  #pvalue <- dplyr::if_else(pv > 0.999, ">0.999", pvalue)
  pvalue <- pvformat(pv)
  
  # Correct N if missing values are not to be included in the count.
  if (!imis) nn <- nn - mm

  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- data.frame(
    Variable = levelchosen,
    n = nn,
    Mis = mm,
    x = xx,
    Sp = sp,
    Se = ssee,
    Ci = ci,
    Pval = pvalue
  )
  row.names(dframe) <- NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    variable, "N", "Mis", "Event","Sample p","SE Prop", 
    paste(conf_normal * 100, "% CI for p<sup>1</sup>", sep = ""),
    "P-value<sup>2</sup>"
  )
  
  # Conditionally remove columns for missing values and p-value.
  if (!imis) {
    dframe <- dframe %>% dplyr::select(-contains("Mis") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "Mis"]
  }
  if (!testyn_normal) {
    dframe <- dframe %>% dplyr::select(-contains("Pval") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "P-value<sup>2</sup>"]
  }
  
  # Conditionally remove columns for not showing SE.
  if (!meth == "asymptotic") {
    dframe <- dframe %>% dplyr::select(-contains("Se") )
    col_headers_html <- col_headers_html[!col_headers_html %in% "SE Prop"]
  }

  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Test for One Proportion</p>",
    sep = ""
  )
  
  
  # Define the HTML strings for footnotes based on test type.
  metex <- dplyr::case_when(meth == "exact"~"Exact",
                            meth == "ac"~"Agresti-Coull",
                            meth == "asymptotic"~"Wald",
                            meth == "wilson"~"Wilson")
  
  if (testyn_normal) {
    fn1 <- paste("Interval method: ",metex,sep="")
    fn2 <- dplyr::case_when(
      alt_normal == "greater" ~ paste("Exact binomial test; H<sub>1</sub>:p&gt; ",nh_normal,sep=""),
      alt_normal == "less" ~ paste("Exact binomial test; H<sub>1</sub>:p&lt; ",nh_normal,sep=""),
      alt_normal == "two.sided" ~ paste("Exact binomial test; H<sub>1</sub>:p&ne; ",nh_normal,sep="")
    )
    fn1 <- paste("<i>", fn1, "<i>", sep = "")
    fn2 <- paste("<i>", fn2, "<i>", sep = "")
    footnotes_html <- c(fn1, fn2)
  } else {
    
    fn1 <- paste("Interval method: ",metex,sep="")
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
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
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
      extra_css = "border-bottom: 1px solid #666; border-top: 1px solid #ddd;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  
  # Return the table as a list.
  list(table = table_out)
}