#' @title Generate a Forest Plot for Adjusted Differences in Proportions
#'
#' @description This function creates a forest plot to visualize the pairwise
#' differences in proportions between levels of a categorical variable. The plot
#' displays the point estimates and their corresponding confidence intervals, which
#' are adjusted for multiple comparisons using the Bonferroni method.
#'
#' @param data A data frame containing the two categorical variables.
#' @param ccc A character string specifying the name of the first categorical variable (column variable).
#' @param rrr A character string specifying the name of the second categorical variable (row variable).
#'
#' @return A `ggplot` object representing the forest plot.
#'
#' @details
#' The plot is generated in two main steps. First, the function calls
#' `getintervals_bc()` to calculate the difference in proportions and the
#' Bonferroni-adjusted confidence intervals for each pairwise comparison.
#'
#' Second, it uses `ggplot2` to create the plot:
#' \itemize{
#'   \item The point estimate of the difference is plotted as a circle.
#'   \item The confidence interval is shown as a horizontal error bar.
#'   \item A vertical dashed line at zero highlights where the difference is
#'   statistically non-significant (i.e., when the confidence interval crosses zero).
#' }
#'
#' This visualization provides a clear and intuitive way to interpret the results
#' of post-hoc proportion tests.
#'
#' @references
#' \itemize{
#'   \item Bonferroni, C. E. (1936). Teoria statistica delle classi e calcolo delle probabilità. Pubblicazioni del R. Istituto Superiore di Scienze Economiche e Commerciali di Bari, 8, 1–62.
#' }
#'
#' @seealso \code{\link{getintervals_bc}}, \code{\link{chisquare____posthoc}}
#'
#' @examples
#' # Note: The getintervals_bc() function must be defined to run this example
#' getintervals_bc <- function(data, ccc, rrr) {
#'   # (function code from the previous example)
#' }
#'
#' # Create a sample data frame
#' data_df <- data.frame(level = factor(sample(c("A","B","C"),size=100,replace = TRUE)),
#'  treatment = factor(sample(c("Treat","Control","Other"),size=100,replace = TRUE)))
#'
#' # Generate the forest plot
#' plot_intervals <- chisquare____ph_interval_plot(
#'   data = data_df,
#'   ccc = "treatment",
#'   rrr = "level"
#' )
#'
#' # Print the plot
#' print(plot_intervals)
#' 
#' 



chisquare____ph_interval_plot <- function(data, ccc, rrr) {
  

  fit <- getintervals_bc(data, ccc, rrr)
  
  cctt <- paste(fit$group1,fit$group2,sep = " - ")
  out <- fit %>% 
    dplyr::mutate(contrast = cctt) %>% 
    dplyr::mutate(contrast = factor(contrast,levels = rev(cctt) ))%>% 
    dplyr::select(contrast,estimate,conf.low,conf.high) 
  
  # Create the forest plot
  plot_intervals <- ggplot2::ggplot(out, ggplot2::aes(x = estimate, y = contrast)) +
    # Add horizontal lines for confidence intervals
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), 
                            height = 0.2, linewidth = 1.2) +
    # Add points for the estimate
    ggplot2::geom_point(size = 3) +
    # Add a vertical reference line at 0
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "blue", linewidth = 2) +
    # Define labels and titles with large font size
    ggplot2::labs(
      title = "Forest Plot of Group Differences",
      subtitle = "Confidence intervals (with Bonferroni corrections)",
      x = paste("Difference in Proportions (",rrr,"=",levels(data[[rrr]])[1],")",sep=""),
      y = "Contrast"
    ) +
    # Customize the theme to increase font sizes
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 22, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 18),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 22),
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16)
    )
  
  plot_intervals
  }