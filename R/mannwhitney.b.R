
# This file is a generated template, your changes will not be overwritten

mannwhitneyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mannwhitneyClass",
    inherit = mannwhitneyBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          if (is.null(self$options$yyy) & is.null(self$options$xxx)) {
            return()
          }
          
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          library(dplyr)
          library(stringr)
          library(kableExtra)
          library(infer)
          
          if (is.null(self$options$yyy) || is.null(self$options$xxx)) {
            self$results$warning_message$setVisible(FALSE)
            self$results$tablestyle_test_1$setVisible(FALSE)
            self$results$tablestyle_test_2$setVisible(FALSE)
            
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
          }
          
          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          if (!is.null(self$options$yyy) & !is.null(self$options$xxx) ) {
            varsName <- self$options$yyy
            byName <- self$options$xxx
            tmpDat <- jmvcore::select(self$data, c(varsName,byName) )
            tmpDat <- as.data.frame(tmpDat)
            
            tmpDat[[byName]] <- factor(tmpDat[[byName]])
            tmpDat[[byName]] <- relevel(tmpDat[[byName]],ref=self$options$refl)
          }
          
          
          
          nd <- as.numeric(self$options$nude)
          
          
          #- name: tasi
          #title: Font size
          #type: List
          #options:
          #  - title: Small
          #name:  ts
          #- title: Medium
          #name:  tm
          #- title: Large
          #name:  tl
          #default: tm
          font_size <- case_when(self$options$tasi=="ts"~12,
                                 self$options$tasi=="tm"~16,
                                 self$options$tasi=="tl"~20)
          
          
          
          ###################################
          ### Creates the output (ttest)
          ###################################
          if (!is.null(self$options$yyy)  & !is.null(self$options$xxx)) {
            conf_test <- as.numeric(self$options$conf_test)/100
            nh_test <- as.numeric(self$options$nh_test)
            alt_test <- self$options$alt_test  

            # Check if the variable is a factor with exactly two levels
            if (nlevels(tmpDat[[byName]]) != 2) {
              self$results$tablestyle_test_1$setVisible(FALSE)
              self$results$tablestyle_test_2$setVisible(FALSE)
              self$results$warning_message$setContent(
                paste(
                  "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                  "The selected Grouping variable has more than two levels.\n",
                  "Only two levels are supported by this analysis.",
                  "</p>"
                )
              )
            }
            
            
            if (nlevels(tmpDat[[byName]]) == 2) {
              
              self$results$warning_message$setVisible(FALSE)
              
              
              call_test <- mannwhitney____test(data = tmpDat,
                                               variable = varsName,
                                               by = byName,
                                               conf_test = conf_test,
                                               nh_test = nh_test,
                                               alt_test = alt_test,
                                               nd = nd,
                                               font_size=font_size,
                                               imis=self$options$imis,
                                               testyn_test=self$options$testyn_test)
              tbl1 <- as.character(call_test$table_summ )
              tbl2 <- as.character(call_test$table_infe )
              
              #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
              final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
              final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
              
              self$results$tablestyle_test_1$setContent(final_html_output1)
              self$results$tablestyle_test_2$setContent(final_html_output2)
              
            }
            
          }
          
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!is.null(self$options$yyy) & !is.null(self$options$xxx) & self$options$boot_yn) {
            conf_boot <- as.numeric(self$options$conf_boot)/100
            nh_boot <- as.numeric(self$options$nh_boot) 
            alt_boot <- self$options$alt_boot
            
            # Check if the variable is a factor with exactly two levels
            if (nlevels(tmpDat[[byName]]) != 2) {
              self$results$tablestyle_boot$setVisible(FALSE)
              self$results$boot_plot_interval$setVisible(FALSE)
              self$results$boot_plot_null$setVisible(FALSE)
              
              self$results$warning_message$setContent(
                paste(
                  "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                  "The selected Grouping variable has more than two levels.\n",
                  "Only two levels are supported by this analysis.",
                  "</p>"
                )
              )
            }
            
            if (nlevels(tmpDat[[byName]]) == 2) {
              self$results$warning_message$setVisible(FALSE)
              
              call_boot <- mannwhitney____boot(tmpDat, varsName, byName,
                                           conf_boot = conf_boot,
                                           nh_boot = nh_boot,
                                           alt_boot = alt_boot,
                                           nd = nd,
                                           font_size = font_size,
                                           testyn_boot = self$options$testyn_boot)
              
              #stop(paste(levels(tmpDat[[byName]]),sep="; "))
              
              tbl <- as.character(call_boot$table)
              wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
              
              self$results$tablestyle_boot$setContent(final_html_output)
              #*************************************
              #*** Bootstrap null distribution plot
              #*************************************
              if (self$options$boot_yn && self$options$boot_nullplot_yn) {
                boot_plot <- call_boot$plot_null
                self$results$boot_plot_null$setState(boot_plot)
              }
              #*********************************
              #*** Bootstrap Visualise Interval
              #*********************************
              if (self$options$boot_yn) {
                boot_plot <- call_boot$plot_interval
                self$results$boot_plot_interval$setState(boot_plot)
              }
            }
            
          }
          
          
          
          
          
        },
        .boot_plot_null = function(image,...){
          if (is.null(self$options$yyy)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        },
        .boot_plot_interval = function(image,...){
          if (is.null(self$options$yyy)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          bp <- image$state
          print(bp)
          return(TRUE)
        })
)
