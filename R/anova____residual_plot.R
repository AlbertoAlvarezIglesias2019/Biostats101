#' @title Generate Diagnostic Plots for ANOVA Assumptions
#'
#' @description This function performs diagnostic tests for a one-way ANOVA, checking
#' for normality of residuals and homogeneity of variances. It then generates
#' a combined plot featuring a histogram and a QQ plot of the studentized residuals,
#' with the test results annotated in the subtitle.
#'
#' @param data A data frame containing the variables.
#' @param variable A character string specifying the name of the dependent variable (the continuous variable).
#' @param by A character string specifying the name of the grouping variable (the categorical variable).
#'
#' @return A `patchwork` object (a subclass of `ggplot`) containing the combined
#'   histogram and QQ plot of the residuals, with statistical test results
#'   in the subtitle.
#'
#' @details
#' The function first fits a one-way ANOVA model using `stats::aov()`. It then
#' performs two key diagnostic tests on the model's studentized residuals:
#' \itemize{
#'   \item **Normality:** Assessed using the **Shapiro-Wilk test** (`stats::shapiro.test()`).
#'   \item **Homogeneity of Variances (Homoscedasticity):** Assessed using **Levene's test** (`car::leveneTest()`).
#' }
#' The results of these tests (p-values) are included in the plot's subtitle. The
#' generated plots provide a visual assessment of these assumptions: the histogram
#' shows the distribution of the residuals, and the QQ plot compares the residuals
#' against a theoretical normal distribution.
#'
#' This function requires the `car` and `patchwork` packages, in addition to `ggplot2`.
#' The `pvformat()` function is assumed to be defined elsewhere to format the p-values.
#'
#' @references
#' \itemize{
#'   \item Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete samples). \emph{Biometrika}, 52(3/4), 591-611.
#'   \item Levene, H. (1960). Robust tests for equality of variances. In I. Olkin (Ed.), \emph{Contributions to Probability and Statistics}. Stanford University Press.
#' }
#'
#' @seealso \code{\link[stats]{aov}}, \code{\link[stats]{shapiro.test}}, \code{\link[car]{leveneTest}}
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
#'   len = c(4.2, 11.5, 7.3, 5.8, 6.4, 10, 11.2, 11.2, 5.2, 7,
#'           16.5, 16.5, 15.2, 17.3, 22.5, 17.3, 13.6, 14.5, 18.8, 15.5,
#'           23.6, 18.5, 33.9, 25.5, 26.4, 32.5, 26.7, 21.5, 23.3, 29.5),
#'   dose = factor(rep(c("Low", "Medium", "High"), each = 10))
#' )
#'
#' # Generate the residual plots
#' residual_plot <- anova____residual_plot(
#'   data = data_df,
#'   variable = "len",
#'   by = "dose"
#' )
#' print(residual_plot)
#' 
anova____residual_plot <- function(data, variable, by) {
  
  formu <- as.formula(paste(variable ,"~", by,sep=""))
  aov_object <- stats::aov(formu,data=data)
  
  # Extract studentized residuals
  residuals <- stats::rstudent(aov_object)
  
  # Perform Shapiro-Wilk Test for Normality
  shapiro_test <- stats::shapiro.test(residuals)
  
  # Extract original data and grouping variable for Levene's Test
  model_frame <- model.frame(aov_object)
  response_var <- colnames(model_frame)[1]
  group_var <- colnames(model_frame)[2]
  
  # Perform Levene's Test for Equal Variances
  levene_test <- car::leveneTest(
    as.formula(paste(response_var, "~", group_var)),
    data = model_frame
  )
  interpretation_leven <- ifelse(levene_test$`Pr(>F)`[1] > 0.05,
                           "(Variances may be equal)",
                           "(Variances likely not equal)")
  
  interpretation_shapi <- ifelse(shapiro_test$p.value > 0.05,
                                 "(Data may be normal)",
                                 "(Data likely not normal)")
  
  # Format p-values for text annotation
  shapiro_pval <- pvformat(shapiro_test$p.value)
  levene_pval <- pvformat(levene_test$`Pr(>F)`[1])
  
  # Create a data frame for plotting
  residuals_df <- data.frame(residuals = residuals)
  
  # Create the histogram of residuals
  hist_plot <- ggplot2::ggplot(residuals_df, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 10, fill = "darkblue", color = "white", alpha = 0.7) +
    ggplot2::labs(
      title = "Histogram of Studentized Residuals",
      x = "Studentized Residuals",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()
  
  # Create the QQ plot of residuals
  qq_plot <- ggplot2::ggplot(residuals_df, ggplot2::aes(sample = residuals)) +
    ggplot2::geom_qq(color = "darkblue") +
    ggplot2::geom_qq_line(linetype = "dashed", color = "red") +
    ggplot2::labs(
      title = "QQ Plot of Studentized Residuals",
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    ggplot2::theme_minimal()
  
  # Combine plots using patchwork and add text annotations
  combined_plot <- patchwork::wrap_plots(hist_plot, qq_plot) +
    ggplot2::labs(
      title = "ANOVA Diagnostic Plots",
      subtitle = paste(
        "Normality (Shapiro-Wilk): p =", shapiro_pval,interpretation_shapi,
        "\nEqual Variances (Levene's): p =", levene_pval,interpretation_leven
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
    )
  
  return(combined_plot)
}


