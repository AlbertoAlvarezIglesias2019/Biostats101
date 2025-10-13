#' @title Generate a Boxplot with Pairwise Comparison Results
#'
#' @description This function performs pairwise comparisons (either t-tests or Tukey's HSD)
#' and generates a boxplot annotated with significance bars and p-value stars
#' to visualize the post-hoc test results.
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
#'     \item \code{"hochberg"}: Hochberg (1988) method.
#'     \item `"hommel"`: Hommel (1988) method.
#'     \item `"bonferroni"`: Bonferroni correction.
#'     \item `"tukey"`: Tukey's Honest Significant Difference test.
#'   }
#'
#' @return A `ggplot` object representing the annotated boxplot.
#'
#' @details
#' This function is a companion to \code{\link{anova____ph_interval_plot}}, sharing the same
#' statistical engine but providing a different visualization. It creates a boxplot
#' of the dependent variable for each level of the grouping variable. It then adds
#' significance bars with arrowheads and p-value star labels based on the
#' chosen post-hoc test method.
#'
#' The function relies on the `rstatix`, `dplyr`, and `ggplot2` packages. For methods
#' other than `"tukey"`, it uses `rstatix::t_test()`. For `"tukey"`, it performs a
#' one-way ANOVA with `stats::aov()` followed by `rstatix::tukey_hsd()`.
#'
#' @references
#' \itemize{
#'   \item Holm, S. (1979). A simple sequentially rejective multiple test procedure. \emph{Scandinavian Journal of Statistics}, 6, 65–70.
#'   \item Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. \emph{Biometrika}, 75, 800–803.
#'   \item Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. \emph{Biometrika}, 75, 383–386.
#' }
#'
#' @seealso \code{\link[rstatix]{t_test}}, \code{\link[rstatix]{tukey_hsd}}, \code{\link{anova____ph_interval_plot}}
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
#' # Generate a boxplot with Bonferroni correction
#' plot1 <- anova____ph_inference_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "bonferroni"
#' )
#' print(plot1)
#'
#' # Generate a boxplot with Tukey's HSD
#' plot2 <- anova____ph_inference_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose",
#'   type = "tukey"
#' )
#' print(plot2)
#' 


anova____ph_inference_plot <- function(data, variable, by,imis = FALSE,type) {
  
  ##################################
  ### Choose the label for the type
  ##################################
  typelab <- dplyr::case_when(type == "none"~"None",
                              type == "holm"~"Holm",
                              type == "hochberg"~"Hochberg",
                              type == "hommel"~"Hommel",
                              type == "bonferroni"~"Bonferroni",
                              type == "tukey"~"Tukey")
  
  
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
      rstatix::add_y_position() %>% dplyr::pull(y.position)
    fit <- fit %>% mutate(y.position = temp)
  }
  
  if (!type == "tukey") {
    fit <- data %>% rstatix::t_test(formu,
                                  detailed = TRUE,
                                  p.adjust.method=type,
                                  paired = FALSE)%>%
      rstatix::add_y_position()
  }
  
  ypos <- fit %>% dplyr::pull(y.position)
  
  out <- fit %>% 
    dplyr::mutate(contrast = paste(group1,group2,sep = " - ")) %>% 
    dplyr::select(contrast,estimate,conf.low,conf.high,p.adj) 
  
  ########################################
  ## Creates the boxplot + inference plot
  ########################################
  pwc <- fit
  max_y_pwc <- max(pwc$y.position, na.rm = TRUE)
  #y_limits <- c(0, max(max_y_data, max_y_pwc) * 1.15) # Add 15% buffer
  y_limits <- c(NA,max_y_pwc)
  ytextbuffer <- diff(pwc$y.position)[1]/4
  # 2. Build the plot step-by-step
  plot_signif <- ggplot2::ggplot(data = data, ggplot2::aes(x = factor(.data[[by]]), y = .data[[variable]])) +
    # A. Add the boxplot
    ggplot2::geom_boxplot() +
    # B. Add the horizontal significance bars
    ggplot2::geom_segment(
      data = pwc,
      ggplot2::aes(x = group1, xend = group2, y = y.position, yend = y.position),
      inherit.aes = FALSE,
      lineend = "butt",
      arrow = ggplot2::arrow(ends = "both", angle = 80, length = ggplot2::unit(.1, "cm"))
    ) +
    # C. Add the text labels (p-adj stars)
    ggplot2::geom_text(
      data = pwc,
      ggplot2::aes(
        x = (as.numeric(factor(group1,levels = levels(data[[by]])) ) + as.numeric(factor(group2,levels = levels(data[[by]])))) / 2,
        y = y.position +ytextbuffer, #+ diff(y_limits) * 0.015, # Position the text slightly above the bar
        label = p.adj.signif
      ),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold"
    ) +
    # D. Add titles and labels
    ggplot2::labs(
      title = "Boxplot with Post Hoc Test Results",
      subtitle = paste("Pairwise t-tests with ",typelab," correction",sep=""),
      x = by,
      y = variable
    ) +
    # E. Set the y-axis limits
    ggplot2::coord_cartesian(ylim = y_limits) +
    # F. (Optional) Customize the theme for a cleaner look
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5,size = 19, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5,size = 15),
      axis.title = ggplot2::element_text(size = 17),
      axis.text = ggplot2::element_text(size = 19),
      legend.title = ggplot2::element_text(size = 15),
      legend.text = ggplot2::element_text(size = 13)
    )
  
  plot_signif
  }