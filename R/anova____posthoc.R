#' @title Generate an HTML Table of Post-Hoc Pairwise Comparisons
#'
#' @description This function performs pairwise comparisons (t-tests or Tukey's HSD) and
#' formats the results into a well-styled, publication-quality HTML table.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#' @param nd An integer specifying the number of decimal places to round the numeric results (estimates and confidence intervals). Defaults to 1.
#' @param ev A logical value. This parameter is currently unused and is included for internal purposes.
#' @param font_size A numeric value for the font size of the table.
#' @param imis A logical value. This parameter is currently unused and is included for internal purposes.
#' @param type A character string specifying the type of post-hoc test to perform and the p-value adjustment method.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item \code{"hommel"}: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test.
#'   }
#'
#' @return A character string containing the HTML code for the formatted table.
#'
#' @details
#' This function re-uses the same statistical engine as \code{\link{anova____ph_interval_plot}}
#' and \code{\link{anova____ph_inference_plot}}. It performs the specified post-hoc test,
#' extracts key results, and formats them into a clean, readable table.
#'
#' The function relies on the `rstatix`, `dplyr`, `knitr`, and `kableExtra` packages
#' to perform the statistical analysis and table generation.
#'
#' The output is an HTML table (`knitr::kable` object) that can be easily rendered
#' in environments like Jamovi. The `ndformat()` and `pvformat()` functions are assumed
#' to be defined elsewhere in the module's code to handle the specific formatting of
#' numeric and p-values, respectively.
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#' }
#'
#' @seealso \code{\link[rstatix]{t_test}}, \code{\link[rstatix]{tukey_hsd}}, \code{\link{anova____ph_interval_plot}}, \code{\link{anova____ph_inference_plot}}
#'
#' @examples
#' # Note: The ndformat() and pvformat() functions must be defined to run this example
#' ndformat <- function(x, nd) round(x, nd)
#' pvformat <- function(x) {
#'   dplyr::case_when(
#'     x >= 0.05 ~ paste0(">", 0.05),
#'     x < 0.001 ~ "< .001",
#'     TRUE ~ as.character(round(x, 3))
#'   )
#' }
#'
#' # Create a sample data frame
#' data_df <- data.frame(
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate a post-hoc table using Bonferroni correction
#' anova____posthoc(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   nd = 2,
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#'
#' # Generate a post-hoc table using Tukey's HSD
#' anova____posthoc(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   nd = 3,
#'   font_size = 16,
#'   type = "tukey"
#' )
#' 
#' 

anova____posthoc <- function(data, variable, by, nd=1, ev = TRUE, font_size, imis = FALSE,type) {
  
  cole <- 0.95  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(variable,by) ))
  
  var_data <- df[[variable]]
  by_data <- factor(df[[by]])
  
  ##############################################################################
  ## Check if the grouping variable has at least two observations per category
  ##############################################################################
  #temp <- table(by_data)
  #if (any(temp<2)) return(NULL)

  ##################################
  ### Choose the label for the type
  ##################################
  typelab <- dplyr::case_when(type == "none"~"None",
                              type == "holm"~"Holm",
                              type == "hochberg"~"Hochberg",
                              type == "hommel"~"Hommel",
                              type == "bonferroni"~"Bonferroni",
                              type == "tukey"~"Tukey")


  # holm = Holm (1979)
  # hochberg = Hochberg (1988)
  # hommel = Hommel (1988)
  #Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65–70. https://www.jstor.org/stable/4615733.
  #Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika, 75, 383–386. doi:10.2307/2336190.
  #Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800–803. doi:10.2307/2336325.
  formu <- as.formula(paste(variable ,"~", by,sep=""))

  if (type == "tukey") {
    fit <- stats::aov(var_data ~ by_data)
    fit <- rstatix::tukey_hsd(fit)
    fit <- fit %>% dplyr::select(group1,group2,estimate,conf.low,conf.high,p.adj) %>% dplyr::mutate(adjustmethod = "tukey")
    fit <- fit %>% 
      dplyr::mutate(estimate = -estimate,
                    temp = conf.low,
                    conf.low = -conf.high,
                    conf.high = -temp ) %>% 
      dplyr::select(-temp) %>% 
      dplyr::mutate(p.adj.signif = case_when(p.adj>=0.05~"ns",
                                             p.adj>=0.01 & p.adj<0.05~"*",
                                             p.adj>=0.001 & p.adj<0.01~"**",
                                             p.adj<0.001~"***"))
    temp <- df %>% rstatix::t_test(formu,
                                  detailed = TRUE,
                                  p.adjust.method="none",
                                  paired = FALSE)%>%
      rstatix::add_y_position() %>% dplyr::pull(y.position)
    fit <- fit %>% mutate(y.position = temp)
  }
  
  if (!type == "tukey") {
    fit <- df %>% rstatix::t_test(formu,
                                   detailed = TRUE,
                                   p.adjust.method=type,
                                   paired = FALSE)%>%
      rstatix::add_y_position()
  }
  
  ypos <- fit %>% dplyr::pull(y.position)
  
  out <- fit %>% 
    dplyr::mutate(contrast = paste(group1,group2,sep = " - ")) %>% 
    dplyr::select(contrast,estimate,conf.low,conf.high,p.adj) 
  
  if (type == "bonferroni") {
    temp <- df %>% rstatix::t_test(formu,
                                   detailed = TRUE,
                                   p.adjust.method="none",
                                   conf.level = 1 - (1-cole)/(dim(out)[1]),
                                   paired = FALSE)
    out$conf.low <- temp$conf.low
    out$conf.high  <- temp$conf.high 
  }
    
  dframe <- out %>% 
    dplyr::mutate(estimate = ndformat(estimate,nd),
           conf.low = ndformat(conf.low,nd),
           conf.high = ndformat(conf.high,nd),
           p.adj = pvformat(p.adj)) %>% 
    dplyr::mutate(Ci = paste("(",conf.low,", ",conf.high,")",sep="")) %>% 
    dplyr::select(contrast,estimate,Ci,p.adj)
  
  
  col_headers_html <- c("Contrast", "Diff in Means","95% adjusted CI<sup>1</sup>","Adjusted P-value<sup>1</sup>")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Post-Hoc Pairwise comparisons</p>",
    sep = ""
  )
  
  
  table_out_infe <- knitr::kable(
    dframe,
    format = "html",
    align = "lccc",
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
      border_left = "0.5px solid #ddd",
      border_right = "0.5px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    )  %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    )%>%
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "border-top: 1px solid #ddd;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = paste("<i>",paste("Using a ",typelab," correction",sep=""),"</i>"),
      escape = FALSE
    )
  

  #plot_intervals <- ggpubr::ggarrange(plot_intervals,plot_signif,ncol=1)
  
  table_out_infe
}
