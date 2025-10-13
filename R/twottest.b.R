
# This file is a generated template, your changes will not be overwritten

twottestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "twottestClass",
    inherit = twottestBase,
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
            self$results$tablestyle_ttest_1$setVisible(FALSE)
            self$results$tablestyle_ttest_2$setVisible(FALSE)
      
            self$results$tablestyle_boot$setVisible(FALSE)
            self$results$boot_plot_interval$setVisible(FALSE)
            self$results$boot_plot_null$setVisible(FALSE)
            self$results$nomality_plot$setVisible(FALSE)
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
            conf_ttest <- as.numeric(self$options$conf_ttest)/100
            nh_ttest <- as.numeric(self$options$nh_ttest)
            alt_ttest <- self$options$alt_ttest  
            ev <- self$options$ev
            
            # Check if the variable is a factor with exactly two levels
            if (nlevels(tmpDat[[byName]]) != 2) {
              self$results$tablestyle_ttest_1$setVisible(FALSE)
              self$results$tablestyle_ttest_2$setVisible(FALSE)
              self$results$nomality_plot$setVisible(FALSE)
              
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
              
              #*******************************
              #*** Normality plot (if chosen)
              #*******************************
              if (self$options$norma_assess_yn) self$results$nomality_plot$setState(self$data)
              

              call_ttest <- twomean____ttest(tmpDat, varsName, byName, conf_ttest,alt_ttest,ev,nh_ttest,nd,
                                             font_size=font_size,
                                             imis=self$options$imis,
                                             testyn_ttest=self$options$testyn_ttest)
              tbl1 <- as.character(call_ttest$table_summ)
              tbl2 <- as.character(call_ttest$table_infe)
              
              #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
              wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
              final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
              final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
              
              self$results$tablestyle_ttest_1$setContent(final_html_output1)
              self$results$tablestyle_ttest_2$setContent(final_html_output2)
            }

          }

          
          ####################################################
          ### Creates the output (ttest with summarised data)
          ####################################################
          if (self$options$summ_yn) {
            conf_ttest <- as.numeric(self$options$conf_ttest)/100
            nh_ttest <- as.numeric(self$options$nh_ttest)
            alt_ttest <- self$options$alt_ttest  
            summ_n1 <- as.numeric(self$options$summ_n1)
            summ_mean1 <- as.numeric(self$options$summ_mean1)
            summ_sd1 <- as.numeric(self$options$summ_sd1)
            summ_n2 <- as.numeric(self$options$summ_n2)
            summ_mean2 <- as.numeric(self$options$summ_mean2)
            summ_sd2 <- as.numeric(self$options$summ_sd2)
            dasu1 <- data.frame(Response = scale(rnorm(summ_n1))*summ_sd1+summ_mean1,
                                Group = "Group 1")
            dasu2 <- data.frame(Response = scale(rnorm(summ_n2))*summ_sd2+summ_mean2,
                                Group = "Group 2")
            dasu <- rbind(dasu1,dasu2)
            dasu$Group <- factor(dasu$Group)
            
            
            call_ttest <- twomean____ttest(dasu, "Response", "Group", conf_ttest,alt_ttest,ev=FALSE,nh_ttest,nd,
                                           font_size=font_size,
                                           imis=self$options$imis,
                                           testyn_ttest=self$options$testyn_ttest)
            #tbl1 <- as.character(call_ttest$table_summ)
            tbl2 <- as.character(call_ttest$table_infe)
            
            #tbl1 <- stringr::str_replace(tbl1,"Descriptive Statistics","Descriptive Statistics (Summarised Data)")
            tbl2 <- stringr::str_replace(tbl2,"Two-sample t-test","Two-sample t-test (Summarised Data)")
            
            #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
            wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
            #final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
            final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
            
            #self$results$tablestyle_ttest_1$setContent(final_html_output1)
            self$results$tablestyle_ttest_summ$setContent(final_html_output2)
            
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
              
              call_boot <- twomean____boot(tmpDat, varsName, byName,
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
        },
        .nomality_plot = function(image,...){
          if (is.null(self$options$yyy) | !self$options$norma_assess_yn) return(FALSE)
          
          DD <- image$state
          YY <- self$options$yyy
          XX <- self$options$xxx
          
          l1 <- levels(DD[[XX]])[1]
          l2 <- levels(DD[[XX]])[2]
          
          x1 <- DD[[YY]][DD[[XX]]==l1]
          x2 <- DD[[YY]][DD[[XX]]==l2]
          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          #x1, x2, var_name1 = "Data 1", var_name2 = "Data 2"
          assess_normality_2var(x1,x2,l1,l2)
          TRUE
        })
)
