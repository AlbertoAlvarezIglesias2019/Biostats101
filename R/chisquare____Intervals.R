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
#' chisquare____intervals(
#'   data = data,
#'   ccc = "level",
#'   rrr = "treatment",
#'   font_size = 14,
#'   type = "bonferroni"
#' )
#' 
#' 





chisquare____intervals <- function(data, ccc, rrr, nd=1, font_size,type="bonferroni") {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(rrr,ccc) )) %>% 
    na.omit()
  
  colu <- droplevels(df[[ccc]])
  rows <- droplevels(df[[rrr]])
  cl1 <- levels(colu)[1]
  if (length(levels(colu))==2) {cl2<-levels(colu)[2]} else {
    cl2 <- paste(levels(colu)[-1],collapse=" or ")
    #cl2 <- paste("[",cl2,"]",sep="")
    }
  colu1 <- dplyr::if_else(colu == levels(colu)[1],cl1,cl2)
  colu1 <- factor(colu1,levels = c(cl1,cl2) )
  
  xtab <- table(rows,colu1)
  
  alphac <- 0.05/dim(xtab)[1]
  

  ##################################
  ### Choose the label for the type
  ##################################
  typelab <- dplyr::case_when(type == "none"~"Without correction",
                              type == "bonferroni"~"With Bonferroni correction")
  alpha <- dplyr::case_when(type == "none"~0.05,
                            type == "bonferroni"~0.05/dim(xtab)[1])
  
  temp <- lapply(1:dim(xtab)[1],function(i){
    #fit <- prop.test(c(x1,x2),c(n1,n2),conf.level=conf_normal,alternative = alt_normal,correct = yatc)
    fit <- prop.test(c(xtab[i,1],xtab[i,2]),c(sum(xtab[,1]),sum(xtab[,2])),
                     conf.level=1-alpha)
    diff <- paste(ndformat( (fit$estimate[1]-fit$estimate[2])*100,nd),"%",sep="")
    out <- paste(ndformat(fit$conf.int*100, nd),"%",sep="")
    out <- paste(out, collapse = ", ")
    ci <- paste("(", out, ")", sep = "")
    pv <- fit$p.value
    pval <- pvformat(pv)
    dframe <- data.frame(
      Lab = paste(row.names(xtab)[i],sep=""),
      p1 = paste(ndformat( (fit$estimate[1])*100,nd),"%",sep=""),
      p2 = paste(ndformat( (fit$estimate[2])*100,nd),"%",sep=""),
      diff = diff,
      pe = fit$estimate[1]-fit$estimate[2],
      lb = fit$conf.int[1],
      ub = fit$conf.int[2],
      Ci = ci,
      Pval = pval
    )
    row.names(dframe) <- NULL
    dframe
  })
  DF <- do.call("rbind",temp)
  # holm = Holm (1979)
  # hochberg = Hochberg (1988)
  # hommel = Hommel (1988)
  #Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65–70. https://www.jstor.org/stable/4615733.
  #Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika, 75, 383–386. doi:10.2307/2336190.
  #Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800–803. doi:10.2307/2336325.
 
  dframe <- DF %>% dplyr::select(-lb,-ub,-pe)
  col_headers_html <- c(rrr,levels(colu1)[1],levels(colu1)[2],"Diff in Prop","95% CI<sup>1</sup>","P-value<sup>1</sup>")
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Intervals Two-sample Proportions</p>",
    sep = ""
  )
  
  # Define the HTML strings for footnotes based on test type.
  fn1 <- typelab
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  footnotes_html <- fn1

  header_vector <- setNames(c(1, 2, 3), c(" ", ccc, " "))
  
  
  table_out_infe <- knitr::kable(
    dframe,
    format = "html",
    align = "lccccc",
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
  
  list(table = table_out_infe,data = DF)
}
