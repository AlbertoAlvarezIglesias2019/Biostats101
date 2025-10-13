#' @title Perform Chi-Square and Fisher's Exact Tests and Generate HTML Tables
#'
#' @description This function performs a standard chi-squared test of independence
#' and Fisher's exact test for two categorical variables. It returns a list of
#' two separate, styled HTML tables: a detailed contingency table and a summary
#' of the test results.
#'
#' @param data A data frame containing the two categorical variables.
#' @param rrr A character string specifying the name of the first categorical variable.
#' @param ccc A character string specifying the name of the second categorical variable.
#' @param nd An integer specifying the number of decimal places for numeric values in the contingency table. Defaults to 1.
#' @param font_size A numeric value for the font size of the tables.
#'
#' @return A `list` containing two `knitr::kable` HTML tables:
#'   \itemize{
#'     \item `table_summ`: A contingency table showing observed counts, expected counts, and contributions to the chi-square statistic for each cell.
#'     \item `table_infe`: A summary table with the Chi-square statistic, degrees of freedom, and p-values for both Pearson's and Fisher's tests.
#'   }
#'
#' @details
#' The function first constructs a contingency table, calculating the observed
#' counts, expected counts, and the contribution of each cell to the overall
#' chi-square statistic. It then formats these three values into a single cell,
#' with each value on a new line, and generates a fully styled HTML table.
#'
#' Next, it performs Pearson's Chi-squared test (`stats::chisq.test()`) and Fisher's exact test
#' (`stats::fisher.test()`) on the data. The results (statistic, degrees of freedom,
#' and p-values) are presented in a separate, styled HTML table.
#'
#' **Note:** The `ndformat()` and `pvformat()` functions are assumed to be defined
#' elsewhere in the module's code to handle numeric and p-value formatting, respectively.
#'
#' @references
#' \itemize{
#'   \item Pearson, K. (1900). On the criterion that a given system of deviations from the probable in the case of a correlated system of variables is such that it can be reasonably supposed to have arisen from random sampling. \emph{Philosophical Magazine}, 5(50), 157-175.
#'   \item Fisher, R. A. (1922). On the interpretation of chi-squared from contingency tables, and the calculation of P. \emph{Journal of the Royal Statistical Society}, 85(1), 87-94.
#' }
#'
#' @seealso \code{\link[stats]{chisq.test}}, \code{\link[stats]{fisher.test}}, \code{\link[knitr]{kable}}
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
#' 
#' data_df <- data.frame(rrr = sample(c("A","B","C"),size=100,replace = TRUE),
#' ccc = sample(c("Treat","Control"),size=100,replace = TRUE))
#'
#' # Generate the chi-square tables
#' chi_tables <- chisquare____test(
#'   data = data_df,
#'   rrr = "rrr",
#'   ccc = "ccc",
#'   nd = 2,
#'   font_size = 14
#' )
#'
#' # To display the tables:
#' # The contingency table
#' print(chi_tables$table_summ)
#' # The test results table
#' print(chi_tables$table_infe)
#' 
#' data(bugs)
#' bugs[["Region"]][bugs[["Region"]]==""]<-NA
#' bugs[["Education"]][bugs[["Education"]]==""]<-NA
#' bugs[["Region"]] <- factor(bugs[["Region"]])
#' bugs[["Region"]] <- relevel(bugs[["Region"]],ref="Other")
#' 
#' chisquare____test(bugs,rrr = "Region",ccc="Education",imis = FALSE,font_size=16)
#' 
#' dat <- data.frame(Bloodtype = c("O","A","B","AB"),Ulcer = c(698,472,102,29), nonUlcer= c(2892,2625,570,226) )
#' dat <- dat %>% tidyr::pivot_longer(all_of(summ_data),names_to = "Group",values_to = "N")
#' expanded_dat <- dat %>% tidyr::uncount(N)
#' expanded_dat <- expanded_dat %>% mutate(Bloodtype = factor(Bloodtype,levels = c("O","A","B","AB")))
#' expanded_dat <- expanded_dat %>% mutate(Group = factor(Group,levels = c("Ulcer","nonUlcer")))
#' chisquare____test(data = expanded_dat,rrr = "Bloodtype",ccc="Group",imis = FALSE,font_size=16)





chisquare____test <- function(data, rrr, ccc, nd=1, font_size,imis) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(rrr,ccc) )) %>% 
    na.omit()
  
  var_data <- droplevels(df[[rrr]])
  by_data <- droplevels(factor(df[[ccc]]))
  
  
  ttt1 <- table(var_data,by_data)
  ttt2 <- addmargins(ttt1)
  #plot(ttt1,xlab=rrr,ylab= ccc)
  
  fit <- chisq.test(ttt1)
  obse <- fit$observed
  expe <- as.data.frame(as.table(fit$expected))
  expe <- expe %>% rename(Expe = Freq)
  cont <- as.table( (fit$expected - fit$observed)^2/fit$expected )
  cont <- as.data.frame(cont)
  cont <- cont %>% rename(Cont = Freq)
  
  out <- as.data.frame(ttt2)
  
  if (imis) {
    out <- my_tbl_cross(data,rrr,ccc) %>%
      dplyr::slice(-1) %>% 
      dplyr::select(-test_name,-p.value) %>% 
      tidyr::pivot_longer(-RRR,names_to = "by_data",values_to = "Freq" ) %>% 
      dplyr::rename(var_data="RRR")

    out <- out %>% 
      dplyr::mutate(var_data = if_else(var_data=="MMM","<i>(Unknown)</i>",var_data))%>% 
      dplyr::mutate(by_data = if_else(by_data=="MMM","<i>(Unknown)</i>",by_data))
  }

  out <- out %>% dplyr::left_join(expe,by = c("var_data","by_data"))
  
  out <- out %>% dplyr::left_join(cont,by = c("var_data","by_data"))
  out <- out %>%
    dplyr::mutate(Expe = ndformat(Expe,2)) %>%
    dplyr::mutate(Cont = ndformat(Cont,3)) 

  out <- out %>% 
    dplyr::mutate(cellval = paste(Freq,"<br>",Expe,"<br>",Cont,sep="")) %>% 
    dplyr::select(var_data, by_data,cellval) 
  
  out <- out %>% dplyr::mutate(var_data = if_else(!var_data =="Sum",paste("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",var_data,sep=""),var_data))
  
  
  out <- out %>%
    mutate(var_data = if_else(var_data=="Sum",paste("<b>","All","</b>",sep=""),as.character(var_data)))%>%
    mutate(by_data = if_else(by_data=="Sum",paste("<b>","All","</b>",sep=""),as.character(by_data)))
  
  dframe <- out %>% tidyr::pivot_wider(values_from = cellval,names_from = by_data) 
  

  ##########################
  ### Add a first blank row
  ##########################
  temp <- dframe[1,]
  temp[1,] <- ""
  temp[1,1] <- paste("<b>",rrr,"</b>",sep="") 
  dframe <- rbind(temp,dframe)
  
  
  #*************************
  # Define the table column headers with HTML formatting.
  #*************************
  col_headers_html <- names(dframe)
  col_headers_html[col_headers_html=="var_data"] <- " "

  
  
  #*************************
  # Create the HTML string for the table caption.
  #*************************
  caption_html <- paste(
    "<p style='text-align: center; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>","Contingency Table","</p>",
    sep = ""
  )
  
  #*************************
  # Define the HTML strings for footnotes 
  #*************************
  footnotes_html <- "&nbsp;&nbsp;&nbsp;<i>Count<i><br>  &nbsp;&nbsp;&nbsp;<i>Expected count<i><br>  &nbsp;&nbsp;&nbsp;<i>Contribution to Chi-square<i>"


  #*************************
  # Define the HTML header vector
  #*************************
  wher <- !names(dframe) %in% c("var_data","<b>All</b>") 
  tna <- names(dframe)
  tna <- if_else(wher,ccc," ")
  header_vector <- setNames(rle(wher)$lengths, rle(tna)$values)
  ttt <- rle(wher)$lengths
  
  alit <- c("l",rep("l",dim(dframe)[2]-1))
  alit <- paste(alit,collapse="")
  
  
  # ---  BUILD THE KABLEEXTRA TABLE ---
  #dframe <- dframe[1,]
  table_out_summ <- knitr::kable(
    as.data.frame(dframe),
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    bold = FALSE,
    escape = FALSE
  )  %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      #bold = TRUE,
      extra_css = "font-weight: normal;"
    )%>%
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    ) %>%
    kableExtra::add_header_above(header_vector) %>% 
    kableExtra::row_spec(
      row = c(0,dim(dframe)[1]),
      #bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = dim(dframe)[1],
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; border-top: 1px solid #ddd;; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = 1:dim(dframe)[1],
      extra_css = "white-space: nowrap; border-top: 1px solid #ddd; padding-bottom: 5px; padding-top: 5px; padding-left: 10px; padding-right: 10px;"
    )%>%
    # Add footnotes to the table.
    kableExtra::footnote(
      general = footnotes_html,
      general_title = "Cell Contents",
      escape = FALSE
    )
  
  
  
  
  ###########################
  # Run the chi square test.
  ###########################
  ttt <- table(var_data,by_data)
  fit1 <- stats::chisq.test(ttt)
  expected_counts <- as.data.frame(as.table(fit$expected))$Freq
  
  messa <- NULL
  if (any(expected_counts<5) ) messa <- "<i>Warning: Expected counts are less than 5.<br> Chi-Square test may be unreliable<i>"
  
  fit2 <- stats::fisher.test(ttt)
  
  dframe <- data.frame(chisq = ndformat(fit1$statistic,3),df = fit1$parameter,pv1 = pvformat(fit1$p.value),pv2 = pvformat(fit2$p.value))
  row.names(dframe) <-NULL
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Chi-Square", "DF","P-value<sup>1</sup>","P-value<sup>2</sup>"
  )
  

  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Chi-Square Test</p>",
    sep = ""
  )
  
  
  fn1 <- "Pearson's Chi-squared test"
  fn2 <- "Fisher's Exact Test for Count Data"
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  fn2 <- paste("<i>", fn2, "<i>", sep = "")
  footnotes_html <- c(fn1, fn2)

  
  
  
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
      extra_css = "border-bottom: 1px solid #666; border-top: 1px solid #ddd;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      number = footnotes_html,
      escape = FALSE
    )
  
  if (!is.null(messa)) {
    table_out_infe <- table_out_infe %>% kableExtra::footnote(
      general = messa,
      general_title = "",
      escape = FALSE,
    )
  }
  
  
  
  
  
  
  
  # Return the table as a list.
  list(table_summ = table_out_summ,table_infe = table_out_infe)
}
