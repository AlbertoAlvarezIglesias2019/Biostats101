#' @title Perform Post-Hoc Pairwise Proportions Tests and Generate an HTML Table
#'
#' @description This function performs pairwise comparisons of proportions for two
#' categorical variables, typically as a post-hoc analysis following a significant
#' chi-squared test. It adjusts the p-values for multiple comparisons and
#' presents the results in a styled HTML table.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#' @param nd An integer specifying the number of decimal places to round numeric values.
#' @param font_size A numeric value for the font size of the table.
#' @param type A character string specifying the p-value adjustment method for multiple comparisons.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'   }
#'
#' @return A character string containing the HTML code for the formatted table.
#'
#' @details
#' The function uses `rstatix::pairwise_prop_test()` to perform pairwise
#' comparisons of proportions between all levels of the two categorical variables.
#' This is a common and appropriate post-hoc test when a chi-squared test indicates a
#' significant association between the variables.
#'
#' The results are presented in a table containing the unadjusted p-value, the
#' adjusted p-value based on the selected method, and the significance level.
#' The function relies on the `rstatix`, `knitr`, and `kableExtra` packages.
#' The `pvformat()` helper function is assumed to be defined elsewhere in the module's code.
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#' }
#'
#' @seealso \code{\link[rstatix]{pairwise_prop_test}}, \code{\link{chisquare____test}}
#'
#' @examples
#' # Note: The pvformat() function must be defined to run this example
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
#'   gender = factor(c(rep("Male", 30), rep("Female", 20))),
#'   opinion = factor(c(rep("Agree", 15), rep("Disagree", 15), rep("Agree", 10), rep("Disagree", 10)))
#' )
#'
#' # Perform post-hoc tests with Bonferroni correction
#' chisquare____posthoc(
#'   data = data_df,
#'   ccc = "gender",
#'   rrr = "opinion",
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#'
#'
#' data <- data.frame(level = factor(sample(c("A","B","C"),size=100,replace = TRUE)),
#'  treatment = factor(sample(c("Treat","Control","Other"),size=100,replace = TRUE)))
#' # Perform post-hoc tests with Holm correction
#' chisquare____posthoc(
#'   data = data,
#'   ccc = "level",
#'   rrr = "treatment",
#'   font_size = 14,
#'   type = "holm"
#' )
#' 
#' 





chisquare____posthoc <- function(data, ccc, rrr, nd=1, font_size,type="bonferroni") {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(rrr,ccc) )) %>% 
    na.omit()
  
  colu <- droplevels(df[[ccc]])
  rows <- droplevels(df[[rrr]])
  rows1 <- dplyr::if_else(rows == levels(rows)[1],levels(rows)[1],"OOtthheerr")
  rows1 <- factor(rows1,levels = c(levels(rows)[1],"OOtthheerr"))
  
  xtab <- table(colu,rows1)
  
  

  ##################################
  ### Choose the label for the type
  ##################################
  typelab <- dplyr::case_when(type == "none"~"None",
                              type == "holm"~"Holm",
                              type == "hochberg"~"Hochberg",
                              type == "hommel"~"Hommel",
                              type == "bonferroni"~"Bonferroni")
  
  
  # holm = Holm (1979)
  # hochberg = Hochberg (1988)
  # hommel = Hommel (1988)
  #Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65–70. https://www.jstor.org/stable/4615733.
  #Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika, 75, 383–386. doi:10.2307/2336190.
  #Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800–803. doi:10.2307/2336325.
  fit <- t(xtab) %>% rstatix::pairwise_prop_test(p.adjust.method=type)

  
  ######################
  ### Get the intervals
  ######################
  fit1 <- getintervals_bc(data, ccc, rrr)
  
  fit <- fit1 %>% dplyr::left_join(fit) %>% dplyr::select(-p.adj.signif,-p)
  
  out <- fit %>% 
    dplyr::mutate(contrast = paste(group1,group2,sep = " - ")) %>% 
    dplyr::mutate(dipro = paste(ndformat(estimate*100,nd),"%",sep=""))%>% 
    dplyr::mutate(ci = paste(paste(ndformat(conf.low*100,nd),"%",sep=""),paste(ndformat(conf.high*100,nd),"%",sep=""),sep=" to ")) %>% 
    #dplyr::mutate(ci = paste("(",ci,")",sep="")) %>% 
    dplyr::select(contrast,dipro,ci,p.adj) 
  
  dframe <- out %>% 
    dplyr::mutate(p.adj = pvformat(p.adj)) 
  
  col_headers_html <- c("Contrast","Diff in Prop<sup>1</sup>","95% adjusted CI<sup>2</sup>","Adjusted P-value<sup>3</sup>")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Post-Hoc Pairwise comparisons</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  fn1 <- paste("Proportion of ",rrr," = ",levels(data[[rrr]])[1],sep="" )
  fn2 <- "Interval method: Asymptotic (with Bonferroni correction)"
  fn3 <- paste("Using a ",typelab," correction",sep="")
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  fn2 <- paste("<i>", fn2, "<i>", sep = "")
  fn3 <- paste("<i>", fn3, "<i>", sep = "")
  footnotes_html <- c(fn1, fn2,fn3)

  
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
      number = footnotes_html,
      escape = FALSE
    )
  
  
  #plot_intervals <- ggpubr::ggarrange(plot_intervals,plot_signif,ncol=1)
  
  table_out_infe
}
