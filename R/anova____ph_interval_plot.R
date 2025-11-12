#' @title Generate a Forest Plot of Pairwise Comparison Results
#'
#' @description This function performs pairwise comparisons (either t-tests or Tukey's HSD)
#' and generates a forest plot to visualize the point estimates and confidence
#' intervals of the differences between groups.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#' @param imis A logical value. This parameter is currently unused and is included for internal purposes.
#' @param type A character string specifying the type of post-hoc test to perform and the p-value adjustment method.
#'   Valid options are:
#'   \itemize{
#'     \item `"none"`: No p-value adjustment.
#'     \item `"holm"`: Holm (1979) method.
#'     \item `"hochberg"`: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test (Tukey, 1949).
#'   }
#'
#' @return A `ggplot` object representing the forest plot of the pairwise comparisons.
#'
#' @details
#' The function relies on the `rstatix`, `dplyr`, and `ggplot2` packages.
#' For methods other than `"tukey"`, it uses `rstatix::t_test()`. For `"tukey"`,
#' it performs a one-way ANOVA with `stats::aov()` followed by `rstatix::tukey_hsd()`.
#' The Tukey's HSD results are then manually re-oriented to match the format of the
#' t-test results for consistent plotting.
#'
#' The generated plot is a forest plot showing the estimate of the difference
#' between groups and its 95% confidence interval. A vertical dashed line at 0
#' helps identify contrasts where the confidence interval includes zero (not
#' statistically significant).
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#'   \item Tukey, J. W. (1949). The problem of multiple comparisons. Unpublished manuscript.
#' }
#'
#' @seealso \code{\link[rstatix]{t_test}}, \code{\link[rstatix]{tukey_hsd}}
#'
#' @examples
#' # Create a sample data frame
#' data_df <- data.frame(
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate a forest plot using Bonferroni correction
#' plot1 <- anova____ph_interval_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "bonferroni"
#' )
#' print(plot1)
#'
#' # Generate a forest plot using Tukey's HSD
#' plot2 <- anova____ph_interval_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "tukey"
#' )
#' print(plot2)



anova____ph_interval_plot <- function(data, variable, by,imis = FALSE,type) {
  
  cole <- 0.95 
  library(rstatix)
  
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
    fit <- stats::aov(formu, data = data)
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
    temp <- data %>% rstatix::t_test(formu,
                                   detailed = TRUE,
                                   p.adjust.method="none",
                                   paired = FALSE)%>%
      rstatix::add_y_position() #%>% dplyr::pull(y.position)
    #fit <- fit %>% mutate(y.position = temp)
    fit <- fit %>% mutate(y.position = temp$y.position)
  }
  
  if (!type == "tukey") {
    fit <- data %>% rstatix::t_test(formu,
                                  detailed = TRUE,
                                  p.adjust.method=type,
                                  paired = FALSE)%>%
      rstatix::add_y_position()
  }
  
  #ypos <- fit %>% dplyr::pull(y.position)
  ypos <- fit$y.position
  
  out <- fit %>% 
    dplyr::mutate(contrast = paste(group1,group2,sep = " - ")) %>% 
    dplyr::select(contrast,estimate,conf.low,conf.high,p.adj) 
  
  if (type == "bonferroni") {
    temp <- data %>% rstatix::t_test(formu,
                                  detailed = TRUE,
                                   p.adjust.method="none",
                                   conf.level = 1 - (1-cole)/(dim(out)[1]),
                                   paired = FALSE)
    out$conf.low <- temp$conf.low
    out$conf.high  <- temp$conf.high 
  }
  
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
      title = "Interval Plot of Group Differences",
      subtitle = paste("Confidence intervals (method = ",typelab,")",sep=""),
      x = "Difference (Point Estimate)",
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