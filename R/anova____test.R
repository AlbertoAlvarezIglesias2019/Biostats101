#' @title One-Way ANOVA Test and Summary Tables
#' @description This function performs a one-way ANOVA test, providing descriptive statistics,
#'   ANOVA results, and a diagnostic plot. It can also perform Welch's ANOVA for unequal
#'   variances.
#'
#' @param data A `data.frame` or `tibble` containing the variables.
#' @param variable A character string specifying the name of the numeric variable
#'   (dependent variable) for the analysis.
#' @param by A character string specifying the name of the categorical variable
#'   (independent variable) used for grouping.
#' @param nd An integer specifying the number of decimal places for formatting numeric values.
#' @param ev A logical value (`TRUE` by default) to assume equal variances. If `FALSE`,
#'   the function will perform a Welch one-way ANOVA test.
#' @param font_size An integer specifying the font size for the output tables.
#' @param imis A logical value (`FALSE` by default) to include a "missing" column
#'   in the descriptive statistics table.
#'
#' @return A `list` containing three elements:
#'   \itemize{
#'     \item \strong{table_summ}: A `kableExtra` HTML table with descriptive statistics.
#'     \item \strong{table_infe}: A `kableExtra` HTML table with the ANOVA results.
#'     \item \strong{plot_residuals}: A `ggplot` object from `anova_diagnostic_plot()`.
#'   }
#' @examples
#' # Example using the built-in `mtcars` dataset
#' # We will test if the number of cylinders affects miles per gallon
#' mtcars$cyl <- as.factor(mtcars$cyl)
#'
#' # Run the ANOVA test assuming equal variances
#' fit <- anova_results <- anova____test(
#'   data = mtcars,
#'   variable = "mpg",
#'   by = "cyl",
#'   nd = 2,
#'   ev = TRUE,
#'   font_size = 14
#' )
#'
#' # Access the tables and plot
#' fit$table_summ
#' fit$table_infe
#' fit$plot_residuals
#' fit$plot_data
#'
#' # Run the ANOVA test without assuming equal variances (Welch's test)
#' anova_welch_results <- anova____test(
#'   data = mtcars,
#'   variable = "mpg",
#'   by = "cyl",
#'   nd = 2,
#'   ev = FALSE,
#'   font_size = 14
#' )
#'
#' # Access the results from the Welch test
#' anova_welch_results$table_summ
#' anova_welch_results$table_infe
#'
#'
#'

anova____test <- function(data, variable, by, nd=1, ev = TRUE, font_size, imis = FALSE) {
  
  # --- 1. PREPARE DATA AND RUN T-TEST ---
  
  df <- data %>%
    dplyr::select(all_of(c(variable,by) ))
  
  var_data <- df[[variable]]
  by_data <- factor(df[[by]])
  
  ##############################################################################
  ## Check if the grouping variable has at least two observations per category
  ##############################################################################
  temp <- table(by_data)
  if (any(temp<2)) return(NULL)

  
  # Run the one-sample t-test.
  fit <- stats::aov(var_data ~ by_data)
  fit1 <- broom::tidy(fit)

  
  # Calculate and format descriptive statistics.
  fit2 <- df %>% dplyr::group_by(.data[[by]]) %>% dplyr::summarise(nn = n(),
                                                     mm = sum(is.na(.data[[variable]])),
                                                     me = mean(.data[[variable]],na.rm=TRUE),
                                                     sd = sd(.data[[variable]],na.rm=TRUE))
  
  fit2 <- fit2 %>% 
    dplyr::mutate(me = ndformat(me,nd),
           sd = ndformat(sd,nd))

  

  # Format the p-value with boundary conditions.
  fit1 <- fit1 %>% dplyr::mutate(p.value = pvformat(p.value))


  # --- 2. BUILD THE SUMMARY TABLE DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- fit2
  row.names(dframe) <- NULL
  
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c(by, "N", "Mis", "Mean", "SD")
  # Correct N if missing values are not to be included in the count.
  if (!imis) {
    dframe <- dframe %>% dplyr::mutate(nn = nn - mm) %>% dplyr::select(-mm)
    col_headers_html <- c(by, "N", "Mean", "SD")
  } 
  
  # Conditionally remove columns for missing values and p-value.
  header_vector <- setNames(c(3, 2), c(" ",variable ))
  if (!imis) {
    header_vector <- setNames(c(2, 2), c(" ", variable))
  }
  
  
  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>Descriptive Statistics</p>",
    sep = ""
  )
  
  # --- 3. BUILD THE KABLEEXTRA TABLE ---
  
  if (!imis) alit <- "lccc" else alit <- "lcccc"
  table_out_summ <- knitr::kable(
    dframe,
    format = "html",
    align = alit,
    col.names = col_headers_html,
    caption = caption_html,
    escape = FALSE
  ) %>%
    # Style the table layout and font.
    kableExtra::kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = font_size
    )  %>%
    kableExtra::add_header_above(header_vector) %>%
    # Add vertical borders and padding to columns.
    kableExtra::column_spec(
      column = 1:dim(dframe)[2],
      border_left = "1px solid #ddd",
      border_right = "1px solid #ddd",
      extra_css = "white-space: nowrap; padding-top: 2px; padding-bottom: 2px; padding-left: 10px; padding-right: 10px;"
    ) %>%
    # Make the first column (Variable) bold.
    kableExtra::column_spec(
      column = 1,
      bold = TRUE
    ) %>%
    # Add horizontal borders to the header and bold the text.
    kableExtra::row_spec(
      row = 0,
      bold = TRUE,
      extra_css = "white-space: nowrap; border-bottom: 2px solid #666; padding-left: 10px; padding-right: 10px;"
    ) 
  
  
  
  
  # --- 4. BUILD THE Inference DATA AND HTML STRINGS ---
  
  # Create the data frame for the kableExtra table.
  dframe <- fit1
  row.names(dframe) <- NULL
  
  dframe <- dframe %>% dplyr::mutate(term = dplyr::if_else(term == "by_data","Between Groups",term))
  dframe <- dframe %>% dplyr::mutate(term = dplyr::if_else(term == "Residuals","Within Groups",term))
  dfrow <-data.frame(term = "Total",df = sum(dframe$df),sumsq = sum(dframe$sumsq))
  dframe <- dframe %>% dplyr::bind_rows(dfrow)  
  
  
  # Define the table column headers with HTML formatting.
  col_headers_html <- c("Source", "df","Sum of<br> Squares","Mean Square","F","P-value")
  

  # Create the HTML string for the table caption.
  caption_html <- paste(
    "<p style='text-align: left; margin-left: 0; font-size: ",
    font_size + 2,
    "px; color: maroon; font-weight: bold;'>One-Way ANOVA test</p>",
    sep = ""
  )
  
  dframe <- dframe %>% 
    mutate(df = as.character(df),
           sumsq = ndformat(sumsq,1),
           meansq = ndformat(meansq ,1),
           statistic = ndformat(statistic,2))

  wher <- is.na(dframe)
  dframe[wher] <- ""

  # --- 5. BUILD THE KABLEEXTRA TABLE ---
  
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
    # Add horizontal borders to the first data row.
    kableExtra::row_spec(
      row = c(2,3),
      extra_css = "border-bottom: 1.5px solid #666; border-top: 1px solid #ddd;"
    ) 
  
  
  if (!ev) {
    fit <- stats::oneway.test(var_data ~ by_data)
    
    dframe <- data.frame(numdf = ndformat(fit$parameter[1],2),
                         dendf = ndformat(fit$parameter[2],2),
                         statistic = ndformat(fit$statistic,2),
                         pvalue = pvformat(fit$p.value))
    
    col_headers_html <- c("Numerator df", "Denominator df","F","P-value<sup>1</sup>")
    
    # Create the HTML string for the table caption.
    caption_html <- paste(
      "<p style='text-align: left; margin-left: 0; font-size: ",
      font_size + 2,
      "px; color: maroon; font-weight: bold;'>Welch One-Way ANOVA Test</p>",
      sep = ""
    )
    
    
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
        extra_css = "border-bottom: 1.5px solid #666; border-top: 1px solid #ddd;"
      ) %>%
      # Add footnotes to the table.
      kableExtra::footnote(
        number = paste("<i>",fit$method,"</i>"),
        escape = FALSE
      )
    
    
  }
  
  # Return the table as a list.
  list(table_summ = table_out_summ,table_infe = table_out_infe)
}
