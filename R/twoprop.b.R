
# This file is a generated template, your changes will not be overwritten

twopropClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "twopropClass",
    inherit = twopropBase,
    private = list(
        .run = function() {

          
          if (is.null(self$options$yyy) & is.null(self$options$xxx) & !self$options$summ_yn) {
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
            self$results$tablestyle_normal_1$setVisible(FALSE)
            self$results$tablestyle_normal_2$setVisible(FALSE)
            
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
            tmpDat[[byName]] <- relevel(tmpDat[[byName]],ref=self$options$grol)
            
            tmpDat[[varsName]] <- factor(tmpDat[[varsName]])
            tmpDat[[varsName]] <- relevel(tmpDat[[varsName]],ref=self$options$refl)
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
          
          
          #######################################
          ### Creates the output (2 proportions)
          #######################################
          if (!is.null(self$options$yyy)  & !is.null(self$options$xxx)) {
            conf_normal <- as.numeric(self$options$conf_normal)/100
            alt_normal <- self$options$alt_normal  
            yatc <- self$options$yatc
            
            # Check if the variable is a factor with exactly two levels
            if (nlevels(tmpDat[[byName]]) != 2) {
              self$results$tablestyle_normal_1$setVisible(FALSE)
              self$results$tablestyle_normal_2$setVisible(FALSE)

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
              
              #data, variable, by, conf_normal, alt_normal, yatc, nd, font_size, imis = FALSE, testyn_normal = FALSE
              call_normal <- twoprop____normal(tmpDat, varsName, byName,
                                              conf_normal,alt_normal,yatc,nd,
                                             font_size=font_size,
                                             imis=self$options$imis,
                                             meth = self$options$meth,
                                             testyn_normal=self$options$testyn_normal)
              tbl1 <- as.character(call_normal$table_summ)
              tbl2 <- as.character(call_normal$table_infe)
              
              #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
              final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
              final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
              
              self$results$tablestyle_normal_1$setContent(final_html_output1)
              self$results$tablestyle_normal_2$setContent(final_html_output2)
            }
            
          }
          
          
          
          ####################################################
          ### Creates the output (2 prop with summarised data)
          ####################################################
          if (self$options$summ_yn) {
            conf_normal <- as.numeric(self$options$conf_normal)/100
            alt_normal <- self$options$alt_normal  
            yatc <- self$options$yatc
            summ_x1 <- as.numeric(self$options$summ_x1)
            summ_n1 <- as.numeric(self$options$summ_n1)
            summ_x2 <- as.numeric(self$options$summ_x2)
            summ_n2 <- as.numeric(self$options$summ_n2)
            
            # Check if x > n
            if (summ_x1>summ_n1 | summ_x2>summ_n2) {
              self$results$tablestyle_normal_summ_1$setVisible(FALSE)
              self$results$tablestyle_normal_summ_2$setVisible(FALSE)
              
              self$results$warning_message_summ$setContent(
                paste(
                  "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                  "The number of events cannot be greater than the number of trials",
                  "</p>"
                )
              )
            }
            
            if (summ_x1<=summ_n1 & summ_x2<=summ_n2) {
              self$results$warning_message_summ$setVisible(FALSE)
              
              dasu1 <- data.frame(Response = c(rep("Event",summ_x1),rep("No Event",summ_n1 - summ_x1)),
                                  Group = "Group 1")
              dasu2 <- data.frame(Response = c(rep("Event",summ_x2),rep("No Event",summ_n2 - summ_x2)),
                                  Group = "Group 2")
              dasu <- rbind(dasu1,dasu2)
              dasu$Group <- factor(dasu$Group)
              dasu$Response <- factor(dasu$Response,levels = c("Event","No Event"))
              
              call_normal <- twoprop____normal(dasu, "Response", "Group",
                                               conf_normal,alt_normal,yatc,nd,
                                               font_size=font_size,
                                               imis=self$options$imis,
                                               meth = self$options$meth,
                                               testyn_normal=self$options$testyn_normal)
              
              tbl1 <- as.character(call_normal$table_summ)
              tbl2 <- as.character(call_normal$table_infe)
              
              tbl1 <- stringr::str_replace(tbl1,"Descriptive Statistics","Descriptive Statistics</br> <span style='font-size: 0.8em; color: black;'>(Summarised Data)</span>")
              tbl2 <- stringr::str_replace(tbl2,"Test for Two Proportions","Test for Two Proportions</br> <span style='font-size: 0.8em; color: black;'>(Summarised Data)</span>")
              
              #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
              final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
              final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
              
              self$results$tablestyle_normal_summ_1$setContent(final_html_output1)
              self$results$tablestyle_normal_summ_2$setContent(final_html_output2)
            }

            
          }
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!is.null(self$options$yyy) & !is.null(self$options$xxx) & self$options$boot_yn) {
            conf_boot <- as.numeric(self$options$conf_boot)/100
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
              
              call_boot <- twoprop____boot(tmpDat, varsName, byName,
                                           conf_boot = conf_boot,
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
