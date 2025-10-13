#' Summarize Variables for Table 1 Generation
#'
#' This is an internal function designed to compute detailed summary statistics
#' for continuous and categorical variables, optionally stratified by a grouping
#' variable. It calculates group-wise statistics, missing values, and appropriate
#' p-values for comparison across groups.
#'
#' @param data A data.frame or tibble containing the data.
#' @param variable A character vector specifying the name(s) of the variable(s) to summarize.
#' @param by A character string specifying the name of the grouping variable for stratification.
#'   Can be \code{NULL} for an overall summary.
#' @param suco A string specifying the format for \strong{continuous} statistics.
#'   Options supported: \code{"meansd"}, \code{"medianiqr"}, \code{"medianqs"}, or \code{"all"}.
#' @param suca A string specifying the format for \strong{categorical} statistics.
#'   Options supported: \code{"n"}, \code{"p"}, \code{"nNp"}, \code{"np"}, or \code{"pn"}.
#' @param nd An integer specifying the number of decimal places for numeric statistics.
#'
#' @return A data frame (tibble) with summary statistics for the specified variables.
#'   The data frame is structured in a long format suitable for further processing
#'   by display functions like \code{basicstatistics____html}. Key columns include
#'   \code{row_type}, \code{label}, \code{test_name}, and \code{p.value}.
#'
#' @details This function determines whether a variable is continuous or categorical
#'   and applies the appropriate statistical summaries and hypothesis tests.
#'   \strong{Note:} This function relies on an external, unprovided function
#'   \code{ndformat()} for number formatting. It also uses \code{shapiro.test}
#'   and \code{car::leveneTest} for assumption checks for continuous variable testing.
#'
#'
#'#' @examples
#' # The examples below are illustrative and assume 'ndformat' exists.
#' \dontrun{
#' # Create sample data
#' data(iris)
#' my_data <- iris %>% dplyr::mutate(
#'   Petal.Length = dplyr::if_else(Species == "virginica", NA_real_, Petal.Length),
#'   Size = factor(dplyr::if_else(Sepal.Length > 5.8, "Large", "Small"))
#' )
#'
#' # --- Example 1: Continuous variable (Sepal.Width) stratified by 'Species'
#' # Using Mean (SD) format
#' fit <- my_tbl_summary(
#'   data = my_data,
#'   variable = "Sepal.Width",
#'   by = "Species",
#'   suco = "meansd",
#'   suca = "nNp",
#'   nd = 2
#' )
#' print(continuous_summary)
#'
#' # --- Example 2: Categorical variable (Size) stratified by 'Species'
#' # Using n / N (%) format
#' categorical_summary <- my_tbl_summary(
#'   data = my_data,
#'   variable = "Size",
#'   by = "Species",
#'   suco = "meansd",
#'   suca = "nNp",
#'   nd = 1
#' )
#' print(categorical_summary)
#'
#' # --- Example 3: Continuous variable with no stratification (by = NULL)
#' overall_summary <- my_tbl_summary(
#'   data = my_data,
#'   variable = "Petal.Width",
#'   by = NULL,
#'   suco = "medianqs",
#'   suca = "nNp",
#'   nd = 2
#' )
#' print(overall_summary)
#' }
#'


my_tbl_summary <- function(data, variable, by, suco, suca, nd_num,nd_cat) {
  
  df <- data
  
  temp <- lapply(variable, function(vvv) {
    
    if (is.null(by)) {
      tmpDat <- data.frame(var_data = df[[vvv]])
    } else {
      tmpDat <- data.frame(var_data = df[[vvv]], by_data = factor(df[[by]]))
      tmpDat <- tmpDat %>% dplyr::filter(!is.na(by_data))
    }
    
    
    #################
    ### Numeric data
    #################
    if (is.numeric(tmpDat$var_data)) {
      
      if (is.null(by)) {
        ddd1 <- tmpDat %>%
          dplyr::summarise(by_data = "",
                           n = dplyr::n(),
                           Mis = sum(is.na(var_data)),
                           Mean = mean(var_data, na.rm = TRUE),
                           SD = stats::sd(var_data, na.rm = TRUE),
                           Median = stats::median(var_data, na.rm = TRUE),
                           Q1 = stats::quantile(var_data, 0.25, na.rm = TRUE),
                           Q3 = stats::quantile(var_data, 0.75, na.rm = TRUE),
                           Min = min(var_data, na.rm = TRUE),
                           Max = max(var_data, na.rm = TRUE))
      } else {
        ddd0 <- tmpDat %>%
          dplyr::summarise(by_data = "Overall",
                           n = dplyr::n(),
                           Mis = sum(is.na(var_data)),
                           Mean = mean(var_data, na.rm = TRUE),
                           SD = stats::sd(var_data, na.rm = TRUE),
                           Median = stats::median(var_data, na.rm = TRUE),
                           Q1 = stats::quantile(var_data, 0.25, na.rm = TRUE),
                           Q3 = stats::quantile(var_data, 0.75, na.rm = TRUE),
                           Min = min(var_data, na.rm = TRUE),
                           Max = max(var_data, na.rm = TRUE))
        
        
        ddd1 <- tmpDat %>%
          dplyr::group_by(by_data) %>%
          dplyr::summarise(n = dplyr::n(),
                           Mis = sum(is.na(var_data)),
                           Mean = mean(var_data, na.rm = TRUE),
                           SD = stats::sd(var_data, na.rm = TRUE),
                           Median = stats::median(var_data, na.rm = TRUE),
                           Q1 = stats::quantile(var_data, 0.25, na.rm = TRUE),
                           Q3 = stats::quantile(var_data, 0.75, na.rm = TRUE),
                           Min = min(var_data, na.rm = TRUE),
                           Max = max(var_data, na.rm = TRUE))
        ddd1 <- rbind(ddd0, ddd1)
      }
      
      
      ddd1 <- ddd1 %>% dplyr::mutate(html_label = paste(by_data, "<br> N = ", n, sep = ""))
      # varname is defined but not used later in this block, keeping only if suco == "all" logic is kept.
      # ddd1 <- ddd1 %>% dplyr::mutate(varname = paste("stat_", 0:(dim(ddd1)[1] - 1), sep = ""))
      
      
      ## Row with the missing
      rr3 <- ddd1 %>%
        dplyr::select(html_label, Mis) %>%
        tidyr::pivot_wider(values_from = Mis, names_from = html_label) %>%
        dplyr::mutate(row_type = "missing") %>%
        dplyr::mutate(label = "(Unknown)")
      
      if (suco == "meansd") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(ndformat(Mean, nd_num), " (", ndformat(SD, nd_num), ")", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "label") %>%
          dplyr::mutate(label = vvv)
        
        out <- rbind(rr2, rr3) %>% dplyr::mutate(summary_label = "Mean (SD)")
      }
      if (suco == "medianiqr") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(ndformat(Median, nd_num), " (", ndformat(Q3 - Q1, nd_num), ")", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "label") %>%
          dplyr::mutate(label = vvv)
        
        out <- rbind(rr2, rr3) %>% dplyr::mutate(summary_label = "Median (IQR)")
      }
      if (suco == "medianqs") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(ndformat(Median, nd_num), " (", ndformat(Q3, nd_num), ",", ndformat(Q1, nd_num), ")", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "label") %>%
          dplyr::mutate(label = vvv)
        
        out <- rbind(rr2, rr3) %>% dplyr::mutate(summary_label = "Median (Q1,Q3)")
        
      }
      
      if (suco == "all") {
        
        rr2 <- ddd1 %>% dplyr::mutate(Mean = ndformat(Mean, nd_num),
                                      SD = ndformat(SD, nd_num),
                                      Median = ndformat(Median, nd_num),
                                      Q1 = ndformat(Q1, nd_num),
                                      Q3 = ndformat(Q3, nd_num))
        rr2 <- rr2 %>% dplyr::select(Mean:Max) %>% t() %>% as.data.frame()
        names(rr2) <- ddd1$html_label
        rn <- row.names(rr2)
        rr2 <- rr2 %>% dplyr::mutate(row_type = "level", label = rn)
        
        rr1 <- rr3
        rr1[1, ] <- NA
        rr1$row_type <- "label"
        rr1 <- rr1 %>% dplyr::mutate(label = vvv)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "")
      }
      
      
      test_used <- NA
      p_val <- NA
      if (!is.null(by)) {
        
        by_data <- tmpDat$by_data
        var_data <- tmpDat$var_data
        
        normal_assumption_met <- TRUE
        for (level in levels(by_data)) {
          group_data <- var_data[by_data == level & !is.na(var_data)]
          if (length(group_data) >= 3) {
            if (stats::shapiro.test(group_data)$p.value < 0.05) {
              normal_assumption_met <- FALSE
              break
            }
          } else {
            # Less than 3 observations is too few for meaningful shapiro test.
            normal_assumption_met <- FALSE
            break
          }
        }
        
        homogeneity_assumption_met <- TRUE
        if (nlevels(by_data) >= 2) {
          # Use stats::aov for formula in car::leveneTest
          if (car::leveneTest(var_data ~ by_data)$"Pr(>F)"[1] < 0.05) {
            homogeneity_assumption_met <- FALSE
          }
        }
        
        if (normal_assumption_met && homogeneity_assumption_met) {
          if (nlevels(by_data) == 2) {
            test_res <- stats::t.test(var_data ~ by_data, var.equal = TRUE)
            p_val <- test_res$p.value
            test_used <- "t-test"
          } else if (nlevels(by_data) > 2) {
            fit <- stats::aov(var_data ~ by_data)
            test_res <- summary(fit)
            p_val <- test_res[[1]][["Pr(>F)"]][1]
            test_used <- "ANOVA"
          }
        } else {
          if (nlevels(by_data) == 2) {
            test_res <- stats::wilcox.test(var_data ~ by_data)
            p_val <- test_res$p.value
            test_used <- "Wilcoxon rank-sum test"
          } else if (nlevels(by_data) > 2) {
            test_res <- stats::kruskal.test(var_data ~ by_data)
            p_val <- test_res$p.value
            test_used <- "Kruskal-Wallis test"
          }
        }
      }
      
      
      out$test_name <- test_used
      out$p.value <- p_val
      
      out$variable <- vvv
      out$var_type <- "continuous"
      
      out$n <- sum(!is.na(tmpDat$var_data))
    }
    #######################
    ### Numeric data (END)
    #######################
    
    
    #################
    ### Categorical
    #################
    if (!is.numeric(tmpDat$var_data)) {
      if (is.null(by)) {
        ddd1 <- tmpDat %>%
          dplyr::mutate(N = dplyr::n(),
                        Mis = sum(is.na(var_data))) %>%
          dplyr::filter(!is.na(var_data)) %>%
          dplyr::group_by(var_data) %>%
          dplyr::summarise(by_data = "",
                           N = N[1],
                           Mis = Mis[1],
                           n = dplyr::n())
        
      } else {
        ddd0 <- tmpDat %>%
          dplyr::mutate(N = dplyr::n(),
                        Mis = sum(is.na(var_data))) %>%
          dplyr::filter(!is.na(var_data)) %>%
          dplyr::group_by(var_data) %>%
          dplyr::summarise(by_data = "Overall",
                           N = N[1],
                           Mis = Mis[1],
                           n = dplyr::n())
        ddd1 <- tmpDat %>%
          dplyr::group_by(by_data) %>%
          dplyr::mutate(N = dplyr::n(),
                        Mis = sum(is.na(var_data))) %>%
          dplyr::filter(!is.na(var_data)) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(by_data, var_data) %>%
          dplyr::summarise(N = N[1],
                           Mis = Mis[1],
                           n = dplyr::n())
        
        ddd1 <- rbind(ddd0, ddd1)
      }
      
      ddd1 <- ddd1 %>% dplyr::mutate(html_label = paste(by_data, "<br> N = ", N, sep = ""))
      
      dd <- ddd1 %>% dplyr::select(by_data) %>% dplyr::distinct()
      # varname is defined but not used later in this block, keeping only if suco == "all" logic is kept.
      # dd <- dd %>% dplyr::mutate(varname = paste("stat_", 0:(dim(dd)[1] - 1), sep = ""))
      # ddd1 <- ddd1 %>% dplyr::left_join(dd, by = "by_data")
      
      # head_label is defined but not used.
      # head_label <- ddd1 %>% dplyr::select(html_label, varname) %>% dplyr::distinct()
      
      
      ## Row for missing
      dd_miss <- ddd1 %>% dplyr::group_by(html_label) %>% dplyr::summarise(Mis = Mis[1])
      rr3 <- dd_miss %>%
        tidyr::pivot_wider(values_from = Mis, names_from = html_label) %>%
        dplyr::mutate(row_type = "missing") %>%
        dplyr::mutate(label = "(Unknown)")
      
      ## Row for the first line
      rr1 <- rr3
      rr1[1, ] <- NA
      rr1$row_type <- "label"
      rr1 <- rr1 %>% dplyr::mutate(label = vvv)
      
      if (suca == "n") {
        dout <- ddd1 %>% dplyr::mutate(value = n)
        
        rr2 <- dout %>%
          dplyr::select(var_data, html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "level") %>%
          dplyr::rename(label = var_data)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "n")
      }
      
      if (suca == "p") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(ndformat(n / (N - Mis) * 100, nd_cat), "%", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(var_data, html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "level") %>%
          dplyr::rename(label = var_data)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "%")
      }
      
      if (suca == "nNp") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(n, " / ", N - Mis, " (", ndformat(n / (N - Mis) * 100, nd_cat), "%)", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(var_data, html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "level") %>%
          dplyr::rename(label = var_data)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "n / N (%)")
      }
      
      if (suca == "np") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(n, " (", ndformat(n / (N - Mis) * 100, nd_cat), "%)", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(var_data, html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "level") %>%
          dplyr::rename(label = var_data)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "n (%)")
      }
      
      if (suca == "pn") {
        dout <- ddd1 %>% dplyr::mutate(value = paste(ndformat(n / (N - Mis) * 100, nd_cat), "% (", n, ")", sep = ""))
        
        rr2 <- dout %>%
          dplyr::select(var_data, html_label, value) %>%
          tidyr::pivot_wider(values_from = value, names_from = html_label) %>%
          dplyr::mutate(row_type = "level") %>%
          dplyr::rename(label = var_data)
        
        out <- rbind(rr1, rr2, rr3) %>% dplyr::mutate(summary_label = "% (n)")
      }
      
      
      test_used <- NA
      p_val <- NA
      if (!is.null(by)) {
        by_data <- tmpDat$by_data
        var_data <- tmpDat$var_data
        tabla <- base::table(var_data, by_data)
        chi_test <- base::suppressWarnings(stats::chisq.test(tabla, correct = FALSE))
        if (base::all(chi_test$expected >= 5)) {
          p_val <- chi_test$p.value
          test_used <- "Chi-square test"
        } else {
          fisher_test <- stats::fisher.test(tabla)
          p_val <- fisher_test$p.value
          test_used <- "Fisher's exact test"
        }
      }
      
      
      out$test_name <- test_used
      out$p.value <- p_val
      
      out$variable <- vvv
      out$var_type <- "categorical"
      
      out$n <- sum(!is.na(tmpDat$var_data))
    }
    #######################
    ### Categorical (END)
    #######################
    out
  })
  
  dframe <- base::do.call("rbind", temp)
  
  dframe <- dframe %>%
    dplyr::mutate(stat_label = dplyr::if_else(row_type == "label", summary_label, as.character(NA))) %>%
    dplyr::select(-summary_label)
  
  dframe <- dframe %>%
    dplyr::mutate(n = dplyr::if_else(row_type == "label", n, as.integer(NA)))
  
  dframe <- dframe %>%
    dplyr::mutate(p.value = dplyr::if_else(row_type == "label", p.value, as.numeric(NA)))
  
  if (is.null(by)) names(dframe) <- stringr::str_remove(names(dframe), "<br> ")
  
  dframe
}
