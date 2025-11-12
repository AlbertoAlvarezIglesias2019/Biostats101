#' @title Plot Power Curves for Two-Sample t-Test
#'
#' @description
#' Generates an informative power curve plot for a one-sample t-test using data typically
#' derived from a set of power calculations (like the output of a function similar to
#' \code{power1ttest_html} or a custom power iteration). The plot visualizes the statistical
#' power (Y) across a range of true mean differences (X) for one or more sample sizes.
#'
#' @param pocu A data frame containing the necessary data for plotting the power curve.
#'   This data frame is expected to have the following columns (as seen in the example):
#'   \itemize{
#'     \item \code{X}: The range of mean differences (effect sizes) used for the x-axis.
#'     \item \code{Y}: The calculated power corresponding to each \code{X} value for a given sample size.
#'     \item \code{Ss}: The sample size(s) used for the calculation (used for grouping/aesthetics).
#'     \item \code{Diff}: The specific mean difference used for the initial calculation (for plotting a point).
#'     \item \code{ActualP}: The actual power achieved by the integer sample size (for plotting a point).
#'     \item \code{alt_ttest}: The type of alternative hypothesis ("two.sided", "greater", or "less").
#'     \item \code{alpha}: The significance level (\eqn{\alpha}).
#'     \item \code{sd}: The assumed standard deviation.
#'   }
#'
#' @return
#' A \code{ggplot} object (a power curve plot) showing power (Y-axis) versus the mean
#' difference (X-axis). The plot is styled with \code{theme_minimal()} and includes a
#' custom legend for sample size, and detailed calculation assumptions in a left-aligned caption.
#'
#' @details
#' The function relies on the `ggplot2` and `dplyr` packages. It plots a power curve for
#' each unique sample size (\code{Ss}) found in the data frame, mapping \code{Ss} to both
#' \code{colour} and \code{linetype} to create a unified legend.
#'
#' It automatically identifies the type of alternative hypothesis (two-sided, greater, or less)
#' and includes the appropriate symbol ($\neq$, $>$, or $<$) in the plot caption along with $\alpha$
#' and the assumed standard deviation.
#'
#' Additionally, the function plots a large point at the input parameters' specified
#' difference (\code{Diff}) and the resulting "Actual Power" (\code{ActualP}).
#'
#' @examples
#' \dontrun{
#' # Assuming 'pocu_data' is a data frame structured like the example,
#' # typically created by iterating power calculations over a range of delta (X) values:
#' # The data must contain columns: X, Y, Ss, alt_ttest, alpha, sd, Diff, ActualP.
#'
#' # Example Plot Call (assuming pocu_data is pre-calculated)
#' power1ttest_plot(pocu = pocu_data)
#'
#' # Example using different sample sizes (if multiple Ss values exist in pocu_data)
#' # This will produce multiple lines on the graph, each corresponding to an Ss.
#' # The legend combines colour and linetype for clarity.
#' }
#'
#' @seealso
#' \code{\link[stats]{power.t.test}}, \code{\link[ggplot2]{ggplot}}
#' @importFrom ggplot2 ggplot aes geom_line geom_point ylim labs theme_minimal theme element_text
#' @importFrom dplyr case_when select distinct
#' @export
#' 
#' 


power2prop_plot <- function(pocu) { 
  
  library(ggplot2)
  
  alte <- dplyr::case_when(pocu$alt_ttest[1] == "two.sided"~"\u2260",
                           pocu$alt_ttest[1] == "greater"~">",
                           pocu$alt_ttest[1] == "less"~"<",)
  
  datpo <- pocu %>% dplyr::select(p1,p2,Ss,ActualP) %>% dplyr::distinct()
  datpo$ActualP <- as.numeric(datpo$ActualP)
  
  #if (pocu$alt_ttest[1]=="two.sided") {
  #  temp <- datpo %>% dplyr::mutate(p2 = p1 - abs(p1-p2) )
  #  datpo <- rbind(datpo,temp)
  #}
  # --- 5. Generate the Power Curve Plot ---
  capt <- paste("Assumptions:\n",
                "\u03b1 =",pocu$alpha[1],"\n",
                "p1 = ",pocu$p1[1],"\n",
                "Alternative: ",alte,sep="" )
  p1_value <- pocu$p1[1]
  power_curve <- ggplot(pocu, aes(x = X, y = Y,colour = as.factor(Ss),linetype = as.factor(Ss))) +
    # Add the power line
    geom_line() +
    geom_point(data = datpo,aes(x=as.numeric(p2),y=ActualP),size=3) +
    scale_y_continuous(
      limits = c(0, 1), # Sets the minimum and maximum limits
      breaks = seq(0, 1, by = 0.2) # Sets the tick mark locations
    )+
    geom_hline(yintercept = pocu$alpha[1], linetype = "dashed", color = "red")+    
    labs(
      title = paste("Power Curve for Two Proportions"),
      color = "Sample Size",
      linetype = "Sample Size",
      caption = capt#,
      #subtitle = paste("n = ", n, ", sd = ", sd, ", alpha = ", sig.level),
     # x = expression(paste("True Difference (", delta, ")")),
    #  y = "Statistical Power (1 - Beta)"
    ) +
    xlab(bquote(paste("Comparison ",p[1]," = ",.(p1_value),sep=""))) +
    ylab("Power")+
    theme_minimal(base_size = 20)  +
    theme(plot.caption = element_text(hjust = 0 ))
  
  power_curve
}
