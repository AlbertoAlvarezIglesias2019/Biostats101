
# This file is a generated template, your changes will not be overwritten

onesignClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onesignClass",
    inherit = onesignBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          library(dplyr)
          library(stringr)
          library(kableExtra)
          library(infer)
          
          if (is.null(self$options$yyy)) {
            self$results$tablestyle_test$setVisible(FALSE)
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
          }
          
          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          if (!is.null(self$options$yyy)) {
            varsName <- self$options$yyy
            tmpDat <- jmvcore::select(self$data, varsName)
            tmpDat <- as.data.frame(tmpDat)
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
          if (!is.null(self$options$yyy)) {
            conf_test <- as.numeric(self$options$conf_test)/100
            nh_test <- as.numeric(self$options$nh_test)
            alt_test <- self$options$alt_test  
            
            #tbl_boot <- tbl %>% as_gt()
            call_test <- onesign____test(data = tmpDat,
                                          variable = varsName,
                                          conf_test = conf_test,
                                          nh_test = nh_test,
                                          alt_test = alt_test,
                                          nd = nd,
                                          font_size=font_size,
                                          imis=self$options$imis,
                                          testyn_test=self$options$testyn_test)
            tbl <- as.character(call_test$table)
            #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            self$results$tablestyle_test$setContent(final_html_output)
          }
          
          
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!is.null(self$options$yyy) & self$options$boot_yn) {
            conf_boot <- as.numeric(self$options$conf_boot)/100
            nh_boot <- as.numeric(self$options$nh_boot) 
            alt_boot <- self$options$alt_boot
            
            #tbl_boot <- tbl %>% as_gt()
            call_boot <- onesign____boot(tmpDat, varsName,
                                         conf_boot = conf_boot,
                                         nh_boot = nh_boot,
                                         alt_boot = alt_boot,
                                         nd = nd,
                                         font_size = font_size,
                                         testyn_boot = self$options$testyn_boot)
            tbl <- as.character(call_boot$table)
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
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
