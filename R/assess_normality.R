assess_normality <- function(x, var_name = "Data") {
  
  # --- Input Validation ---
  # Ensure x is a numeric vector and has enough non-missing values
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }
  
  # Remove missing values for the analysis
  x <- x[!is.na(x)]
  
  if (length(x) < 3) {
    stop("Input vector must have at least 3 non-missing values.")
  }
  
  # --- 1. Perform Shapiro-Wilk Normality Test ---
  # The Shapiro-Wilk test is widely recommended for normality testing.
  shapiro_test <- shapiro.test(x) # [7]
  
  
  # --- 2. Set up the plotting layout ---
  # We will create a layout with 3 regions:
  # 1: Title, 2: Boxplot, 3: QQ-Plot, 4: Test Results
  layout(matrix(c(1, 1, 2, 3, 4, 4), nrow = 3, ncol = 2, byrow = TRUE), 
         heights = c(0.5, 2, 0.8)) # [9, 10]
  
  # Set margins for the plots (bottom, left, top, right)
  par(mar = c(4, 4, 2, 1))
  
  # --- PLOT 1: Main Title for the entire figure ---
  par(mar = c(0, 0, 2, 0)) # No margins for the title plot
  plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
  title_text <- paste("Normality Assessment for", var_name)
  text(0, 0, title_text, cex = 1.5, font = 2) # [6]
  
  # --- PLOT 2: Boxplot ---
  par(mar = c(4, 4, 2, 1)) # Reset margins
  graphics::boxplot(x, main = "Boxplot", xlab = var_name, col = "lightblue", border = "grey30", horizontal = TRUE)
  
  # --- PLOT 3: QQ-Plot ---
  stats::qqnorm(x, main = "Normal Q-Q Plot", pch = 16, col = "grey40")
  stats::qqline(x, col = "steelblue", lwd = 2)
  
  # --- PLOT 4: Display Shapiro-Wilk Test Results ---
  par(mar = c(0, 0, 0, 0)) # No margins for text plot
  plot(0, 0, type = 'n', axes = FALSE, xlab = "", ylab = "")
  
  # Create the text for the test results
  w_statistic <- sprintf("W = %.4f", shapiro_test$statistic)
  p_value_text <- sprintf("p-value = %.4f", shapiro_test$p.value)
  
  # Interpretation of the p-value
  interpretation <- ifelse(shapiro_test$p.value > 0.05, 
                           "p > 0.05: Fail to reject H0 (Data may be normal)",
                           "p <= 0.05: Reject H0 (Data likely not normal)")
  
  # Display the text on the plot area
  legend("center", 
         legend = c("Shapiro-Wilk Test:", w_statistic, p_value_text, "", interpretation),
         bty = "n", # No box around the legend
         cex = 1.2,
         text.font = c(2, 1, 1, 1, 2)) # Bold for titles
  
  # --- Reset graphical parameters to default ---
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
  layout(1)
}
