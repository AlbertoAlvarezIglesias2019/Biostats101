
# This file is a generated template, your changes will not be overwritten

onettestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "onettestClass",
    inherit = onettestBase,
    private = list(
        .run = function() {

          #if (is.null(self$options$yyy) & !self$options$summ_yn) {
          #  return()
          #}
          

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          library(dplyr)
          library(stringr)
          library(gtsummary)
          library(gt)
          library(tolerance)
          library(kableExtra)
          library(infer)
          
          
          # Retrieve the user's selected options
          #yyy_selected <- !is.null(self$options$get("yyy")) # `get` is a safe way to check
          #tolerance_selected <- self$options$get("tolerance_yn")
          
          # Set the visibility of the tablestyle_tolerance item
          #if (yyy_selected && tolerance_selected) {
          #  self$results$tablestyle_tolerance$setVisible(TRUE)
          #} else {
          #  self$results$tablestyle_tolerance$setVisible(FALSE)
          #}
          if (is.null(self$options$yyy)) {
            self$results$tablestyle_tolerance$setVisible(FALSE)
            self$results$tolerance_plot$setVisible(FALSE)
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            self$results$nomality_plot$setVisible(FALSE)
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
            conf_ttest <- as.numeric(self$options$conf_ttest)/100
            nh_ttest <- as.numeric(self$options$nh_ttest)
            alt_ttest <- self$options$alt_ttest  
            
            #tbl_boot <- tbl %>% as_gt()
            call_ttest <- onemean____ttest(tmpDat, varsName, conf_ttest,nh_ttest,alt_ttest,nd,
                                        font_size=font_size,
                                        imis=self$options$imis,
                                        testyn_ttest=self$options$testyn_ttest)
            tbl <- as.character(call_ttest$table)
            #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            self$results$tablestyle_ttest$setContent(final_html_output)
          }


          
          
          ####################################################
          ### Creates the output (ttest with summarised data)
          ####################################################
          if (self$options$summ_yn) {
            conf_ttest <- as.numeric(self$options$conf_ttest)/100
            nh_ttest <- as.numeric(self$options$nh_ttest)
            alt_ttest <- self$options$alt_ttest  
            summ_n <- as.numeric(self$options$summ_n)
            summ_mean <- as.numeric(self$options$summ_mean)
            summ_sd <- as.numeric(self$options$summ_sd)
            dasu <- data.frame(Response = scale(rnorm(summ_n))*summ_sd+summ_mean)
            
            #tbl_boot <- tbl %>% as_gt()
            call_ttest <- onemean____ttest(dasu, "Response", conf_ttest,nh_ttest,alt_ttest,nd,
                                        font_size=font_size,
                                        imis=FALSE,
                                        testyn_ttest=self$options$testyn_ttest)
            tbl <- as.character(call_ttest$table)
            tbl <- str_replace(tbl,"One-sample t-test","One-sample t-test (Summarised Data)")
            #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            self$results$tablestyle_ttest_summ$setContent(final_html_output)
          }

          
          
          ###################################
          ### Creates the output (Tolerance)
          ###################################
          if (!is.null(self$options$yyy)) {
            conf_tolerance <- as.numeric(self$options$conf_tolerance)/100
            poco_tolerance <- as.numeric(self$options$poco_tolerance)/100
            side_tolerance <- self$options$side_tolerance
            plotype_tolerance <- self$options$plotype_tolerance 
            
            #tbl_boot <- tbl %>% as_gt()
            call_boot <- onemean____tolerance(tmpDat, varsName, conf_tolerance,poco_tolerance,side_tolerance,
                                           plotype_tolerance,nd = nd,font_size=font_size)
            tbl <- as.character(call_boot$table)
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
            self$results$tablestyle_tolerance$setContent(final_html_output)
            
            #********************
            #*** Tolerance Plot
            #********************
            if (self$options$tolerance_yn && self$options$tolerance_controlcharts_yn) {
              
              # Let's assume this is where you create your ggplot object
              #final_plot <- call_boot$plot
              tolerance_plot <- call_boot$plot
              #plot(3)
              #recorded_plot <- grDevices::recordPlot()
              
              # --- THIS IS THE CRUCIAL STEP ---
              # 1. Get the image object from the results using its name from the YAML file.
              image <- self$results$tolerance_plot
              
              # 2. Save your created ggplot object to the image's "state".
              #    This makes it available to the renderFun.
              image$setState(tolerance_plot)
            }
          }
            
          
          
          
          ###################################
          ### Creates the output (bootstrap)
          ###################################
          if (!is.null(self$options$yyy) & self$options$boot_yn) {
            conf_boot <- as.numeric(self$options$conf_boot)/100
            nh_boot <- as.numeric(self$options$nh_boot) 
            alt_boot <- self$options$alt_boot
            
            #tbl_boot <- tbl %>% as_gt()
            call_boot <- onemean____boot(tmpDat, varsName,
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
            
 
          
          
          
          #*******************************
          #*** Normality plot (if chosen)
          #*******************************
          if (!is.null(self$options$yyy)) {
            if (self$options$norma_assess_yn) {
              #image <- self$results$nomality_plot
              #image$setState(self$data)
              self$results$nomality_plot$setState(self$data)
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
        },
        .tolerance_plot = function(image,...){
          if (is.null(self$options$yyy)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          tp <- image$state
          tolerance::plottol(tol.out=tp$tol.out,
                             x=tp$x,
                             plot.type=tp$plot.type,
                             y.lab=tp$y.lab)
          return(TRUE)
        },
        .nomality_plot = function(image,...){
          if (is.null(self$options$yyy) | !self$options$norma_assess_yn) return(FALSE)
          
          plotData <- image$state
          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          var_data <- plotData[[self$options$yyy]]
          
          assess_normality(var_data,self$options$yyy)
          TRUE
        })
)
