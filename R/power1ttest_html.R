#' @title Power and Sample Size Calculation for One-Sample t-Test (HTML Table and Plot Data)
#'
#' @description
#' Calculates one of the four quantities: sample size (\code{n}), minimum detectable
#' difference (\code{delta}), or power, for a **one-sample t-test** using
#' \code{\link[stats]{power.t.test}}. Unlike standard power functions, this function accepts
#' **vectors** for input parameters and generates results for all combinations.
#' The primary output is a formatted HTML table, and the secondary output is a data frame
#' suitable for plotting power curves.
#'
#' @param solvefor A character string indicating the quantity to solve for. Must be one of
#'   \code{"n"}, \code{"delta"}, or \code{"power"}. The corresponding argument
#'   (\code{n}, \code{delta}, or \code{power}) must be the value(s) to solve for (e.g., set n to the desired sample size range if solving for delta).
#' @param n A numeric vector of sample size(s) to use. If \code{solvefor="n"}, this parameter
#'   is ignored internally (though a placeholder value should be passed) and the calculated `n` is returned.
#' @param delta A numeric vector of expected difference(s) in means (\eqn{|\mu - \mu_0|}).
#' @param sd A single numeric value for the assumed population standard deviation.
#' @param sig.level A single numeric value for the significance level (\eqn{\alpha}). Default is 0.05.
#' @param power A numeric vector of desired statistical power values.
#' @param alt_ttest A character string specifying the alternative hypothesis. Must be one of
#'   \code{"two.sided"} (default), \code{"less"}, or \code{"greater"}.
#' @param font_size Numeric value specifying the font size (in pixels) for the HTML table. Default is 18.
#'
#' @return
#' A list containing two elements:
#' \itemize{
#'   \item \code{table_out}: An object of class \code{kableExtra::kbl}, which is an HTML table
#'     displaying the calculated results for all combinations of input parameters. The column
#'     corresponding to \code{solvefor} is highlighted.
#'   \item \code{data_plot}: A data frame (\code{pocu}) containing the X and Y coordinates necessary
#'     to plot the power curve(s) for the calculated results. This is ready for use with \code{ggplot2}.
#' }
#'
#' @details
#' The function uses the Cartesian product of the input vectors (via \code{expand.grid})
#' to calculate power, sample size, or minimum detectable difference for every possible combination.
#'
#' \strong{Note on \code{solvefor="n"}}: The calculated sample size (\code{Ss}) is **rounded up** to the nearest whole number.
#' When solving for \code{n}, the table includes an \code{Actual Power} column showing the power achieved
#' by using the rounded-up integer sample size.
#'
#' The function relies on helper function \code{ndformat} (not provided here) and the
#' \code{dplyr}, \code{knitr}, and \code{kableExtra} packages.
#'
#' @examples
#' # Requires installation of the 'dplyr', 'knitr', and 'kableExtra' packages.
#'
#' \dontrun{
#' # --- 1. Calculate Sample Size (n) for multiple Delta and Power values ---
#' results_n <- power1ttest_html(
#'   solvefor = "n",
#'   delta = c(0.5, 1.0, 1.5), # Three different effect sizes
#'   sd = 2,
#'   sig.level = 0.05,
#'   power = c(0.80, 0.90),    # Two different power targets
#'   alt_ttest = "two.sided"
#' )
#'
#' # View the HTML table output (e.g., in RStudio viewer or RMarkdown)
#' results_n$table_out
#'
#' # Plot the power curves for the calculated sample sizes
#' # power1ttest_plot(results_n$data_plot) # Assuming power1ttest_plot is available
#'
#' # --- 2. Calculate Minimum Detectable Difference (Delta) ---
#' results_delta <- power1ttest_html(
#'   solvefor = "delta",
#'   n = c(20, 50),
#'   #delta = 1, # Placeholder, solved for
#'   sd = 1.2,
#'   sig.level = 0.05,
#'   power = 0.90,
#'   alt_ttest = "less"
#' )
#' results_delta$table_out
#'
#' # power1ttest_plot(results_delta$data_plot) # Assuming power1ttest_plot is available
#' 
#' # --- 3. Calculate Power for multiple N and Delta values ---
#' results_power <- power1ttest_html(
#'   solvefor = "power",
#'   n = c(30, 60),
#'   delta = c(0.4, 0.8),
#'   sd = 1,
#'   sig.level = 0.05,
#'   power = 0.8, # Placeholder, solved for
#'   alt_ttest = "greater"
#' )
#' results_power$table_out
#' 
#' # power1ttest_plot(results_power$data_plot) # Assuming power1ttest_plot is available
#' }
#'
#' @seealso
#' \code{\link[stats]{power.t.test}}, \code{\link[knitr]{kable}}, \code{\link[kableExtra]{kable_styling}}
#' @importFrom stats power.t.test
#' @importFrom dplyr case_when select
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec row_spec footnote
#' @export


power1ttest_html <- function(solvefor="n",n=NULL, delta=NULL, sd, sig.level, power=NULL, alt_ttest = "two.sided", font_size=18,nd=1) { 
  
  if (solvefor=="n") {
    #n <- NULL
    pointer <- expand.grid(delta = delta,power = power)
    }
  if (solvefor=="delta") {
    #delta <- NULL
    pointer <- expand.grid(n=n,power = power)
  } 
  if (solvefor=="power") {
    #power <- NULL
    pointer <- expand.grid(n=n,delta = delta)
  } 
  
  alternative <- dplyr::case_when(
    alt_ttest == "greater" ~ "one.sided",
    alt_ttest == "less" ~ "one.sided",
    alt_ttest == "two.sided" ~ "two.sided"
  )
  
  
  
  temp <- lapply(1:dim(pointer)[1],function(i) {
    fit <- stats::power.t.test(n=pointer$n[i], delta=pointer$delta[i], sd=sd, sig.level=sig.level, power=pointer$power[i],
                               type ="one.sample", alternative = alternative,
                               strict = TRUE)
    
    if (solvefor=="n") {
      fittp <- stats::power.t.test(n=ceiling(fit$n),
                                   delta=pointer$delta[i], sd=sd, sig.level=sig.level,
                                   power=NULL,type ="one.sample", alternative = alternative,
                                   strict = TRUE)
      fittp <- ndformat(fittp$power,nd)
    } else fittp<- NA
    
    dframe <- data.frame(
      Diff = fit$delta,
      Ss = ceiling(fit$n),
      TargetP = fit$power,
      ActualP = fittp
    )
    row.names(dframe) <- NULL
    dframe
  })
  dframe <- do.call("rbind",temp)
  dframe <- dframe %>% arrange(-Ss)
  
  dafra <- dframe
  
  if (solvefor=="delta") dafra$Diff <- ndformat(dafra$Diff,nd)
  if (solvefor=="power") dafra$TargetP <- ndformat(dafra$TargetP,nd)

  if (alt_ttest == "two.sided") dafra$Diff <- paste("&plusmn;",dafra$Diff,sep="")
  if (alt_ttest == "less") dafra$Diff <- paste("-",dafra$Diff,sep="")
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(
    "Difference", "Sample Size", "Target Power", "Actual Power")
  
  # Conditionally remove columns if !solvefor=="n"
  if (!solvefor=="n") {
    dafra <- dafra %>% dplyr::select(-ActualP)
    col_headers_html <- col_headers_html[!col_headers_html %in% "Actual Power"]
  }
  
  
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Results</p>",
    sep = ""
  )
  
  
  # Define the HTML strings for footnotes based on test type.
  fn1 <- "One-Sample t Test"
  fn2 <- dplyr::case_when(
    alt_ttest == "greater" ~ paste("Testing &mu; &le; &mu;<sub>0</sub> (versus &mu; &gt; &mu;<sub>0</sub>)", sep = ""),
    alt_ttest == "less" ~ paste("Testing &mu; &ge; &mu;<sub>0</sub> (versus &mu; &lt; &mu;<sub>0</sub>)", sep = ""),
    alt_ttest == "two.sided" ~ paste("Testing &mu; = &mu;<sub>0</sub> (versus &mu; &ne; &mu;<sub>0</sub>)", sep = "")
  )
  fn3 <- dplyr::case_when(
    alt_ttest == "greater" ~ paste("Calculating power for &mu; = &mu;<sub>0</sub> + difference", sep = ""),
    alt_ttest == "less" ~ paste("Calculating power for &mu; = &mu;<sub>0</sub> - difference", sep = ""),
    alt_ttest == "two.sided" ~ paste("Calculating power for &mu; = &mu;<sub>0</sub> &plusmn; difference", sep = "")
  )
  fn4 <- paste("&alpha; = ",sig.level,sep="")
  fn5 <- paste("Assumed standard deviation = ",sd,sep="")
  
  fn1 <- paste("<i>", fn1, "<i>", sep = "")
  fn2 <- paste("<i>", fn2, "<i>", sep = "")
  fn3 <- paste("<i>", fn3, "<i>", sep = "")
  fn4 <- paste("<i>", fn4, "<i>", sep = "")
  fn5 <- paste("<i>", fn5, "<i>", sep = "")
  footnotes_html <- c(fn1, fn2, fn3, fn4, fn5)
  
  
  ####################################
  ### Choose what column to highlight
  ####################################
  if (solvefor=="n") pico <- 2
  if (solvefor=="delta") pico <- 1
  if (solvefor=="power") pico <- 3
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  table_out <- knitr::kable(
    dafra,
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
      column = 1:dim(dafra)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Make the first column (Variable) bold.
    kableExtra::column_spec(
      column = pico,
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
      row = dim(dafra)[1],
      extra_css = "border-bottom: 2px solid #666;"
    ) %>%
    # Add footnotes to the table.
    kableExtra::footnote(
      general = footnotes_html,
      escape = FALSE,
      general_title = ""
    )
  
  
  #############################################################
  ### Now creeates the power curves from the entries in dframe
  #############################################################
  
  #extrax <- (max(as.numeric(dframe$Diff) )*2)/10
  
  dframe <- dframe %>% mutate(rangemin = case_when(alt_ttest == "greater"~0,
                                                   alt_ttest == "less"~-as.numeric(dframe$Diff),
                                                   alt_ttest == "two.sided"~-as.numeric(dframe$Diff) ))     
  dframe <- dframe %>% mutate(rangemax = case_when(alt_ttest == "greater"~as.numeric(dframe$Diff),
                                                   alt_ttest == "less"~0,
                                                   alt_ttest == "two.sided"~as.numeric(dframe$Diff) ))     
  dframe <- dframe %>% mutate(extra = (rangemax-rangemin)/10 )
  if (alt_ttest == "greater") dframe <- dframe %>% mutate(rangemax=rangemax+extra)
  if (alt_ttest == "less") dframe <- dframe %>% mutate(rangemin=rangemin -extra)
  if (alt_ttest == "two.sided") dframe <- dframe %>% mutate(rangemin=rangemin-extra,rangemax=rangemax+extra)
  
  temp <- lapply(1:dim(dframe)[1],function(i) {
    delta_range <- seq(dframe$rangemin[i],dframe$rangemax[i],length.out = 300) 
    power_values <- sapply(delta_range, function(d) {
      stats::power.t.test(
        n = dframe$Ss[i], 
        delta = abs(d), 
        sd = sd, 
        sig.level = sig.level, 
        power = NULL, 
        type = "one.sample", 
        alternative = alternative,
        strict = TRUE
      )$power
    })
    if (solvefor=="n") {tp <- dframe$ActualP[i]} else {tp <- dframe$TargetP[i]}
    out <- data.frame(Diff = dframe$Diff[i],
                      Ss = dframe$Ss[i],
                      TargetP = dframe$TargetP[i],
                      X = delta_range,
                      Y = power_values,
                      #name = paste("Curve ",i,sep=""),
                      alt_ttest = alt_ttest,
                      alpha = sig.level,
                      sd = sd,
                      ActualP = tp)
    if (alt_ttest == "less") {
      out$Diff <- paste("-",out$Diff,sep="")
      #out$Y <- rev(out$Y)
    }
    out
  })
  pocu <- do.call("rbind",temp)
  

  
  list(table_out=table_out,data_plot = pocu)
  }
  