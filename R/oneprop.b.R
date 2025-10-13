
# This file is a generated template, your changes will not be overwritten

onepropClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onepropClass",
    inherit = onepropBase,
    private = list(
        .run = function() {

          if (is.null(self$options$yyy)  & !self$options$summ_yn) {
            return()
          }
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          library(dplyr)
          library(stringr)
          library(kableExtra)
          library(infer)
          
 
          if (is.null(self$options$yyy) ) {
            self$results$warning_message$setVisible(FALSE)
            self$results$tablestyle_normal$setVisible(FALSE)

            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
          }
          

 
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          if (!is.null(self$options$yyy) ) {
            varsName <- self$options$yyy
            tmpDat <- jmvcore::select(self$data, c(varsName) )
            tmpDat <- as.data.frame(tmpDat)
            
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
          ### Creates the output (1 proportion)
          #######################################
          if (!is.null(self$options$yyy) ) {
            conf_normal <- as.numeric(self$options$conf_normal)/100
            alt_normal <- self$options$alt_normal  
            nh_normal <- self$options$nh_normal 
            
            #data, variable, by, conf_normal, alt_normal, yatc, nd, font_size, imis = FALSE, testyn_normal = FALSE
            call_normal <- oneprop____normal(data = tmpDat,
                                             variable = varsName,
                                             conf_normal = conf_normal,
                                             alt_normal = alt_normal,
                                             nh_normal = nh_normal,
                                             nd = nd,
                                             font_size=font_size,
                                             imis=self$options$imis,
                                             meth = self$options$meth,
                                             testyn_normal=self$options$testyn_normal)
            tbl <- as.character(call_normal$table)

            #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)

            self$results$tablestyle_normal$setContent(final_html_output)
            
            
          }
          
          
          
          ####################################################
          ### Creates the output (1 prop with summarised data)
          ####################################################
          if (self$options$summ_yn) {
            conf_normal <- as.numeric(self$options$conf_normal)/100
            alt_normal <- self$options$alt_normal 
            nh_normal <- self$options$nh_normal 
            
            summ_x <- as.numeric(self$options$summ_x)
            summ_n <- as.numeric(self$options$summ_n)
            
            # Check if x > n
            if (summ_x>summ_n) {
              self$results$tablestyle_normal_summ$setVisible(FALSE)
              self$results$warning_message$setContent(
                paste(
                  "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                  "The number of events cannot be greater than the number of trials",
                  "</p>"
                )
              )
            }
            
            if (summ_x<=summ_n) {
              self$results$warning_message$setVisible(FALSE)
              
              dasu <- data.frame(Response = c(rep("Event",summ_x),rep("No Event",summ_n - summ_x)))
              dasu$Response <- factor(dasu$Response,levels = c("Event","No Event"))
              
              call_normal <- oneprop____normal(data = dasu,
                                               variable = "Response",
                                               conf_normal = conf_normal,
                                               alt_normal = alt_normal,
                                               nh_normal = nh_normal,
                                               nd = nd,
                                               font_size=font_size,
                                               imis=self$options$imis,
                                               meth = self$options$meth,
                                               testyn_normal=self$options$testyn_normal)
              
              tbl <- as.character(call_normal$table)
              tbl <- stringr::str_replace(tbl,"Test for One Proportion","Test for One Proportion</br> <span style='font-size: 0.8em; color: black;'>(Summarised Data)</span>")
              
              #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
              final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
              
              
              self$results$tablestyle_normal_summ$setContent(final_html_output)
            }
            
            
          }
          
          
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!is.null(self$options$yyy) & self$options$boot_yn) {
            conf_boot <- as.numeric(self$options$conf_boot)/100
            alt_boot <- self$options$alt_boot
            nh_boot <- self$options$nh_boot 
            
            call_boot <- oneprop____boot(data = tmpDat,
                                         variable = varsName,
                                         conf_boot = conf_boot,
                                         alt_boot = alt_boot,
                                         nh_boot = nh_boot,
                                         nd = nd,
                                         font_size = font_size,
                                         testyn_boot = self$options$testyn_boot)
            

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
