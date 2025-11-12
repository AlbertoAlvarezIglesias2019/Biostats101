
# This file is a generated template, your changes will not be overwritten

anovaClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          library(dplyr)
          #library(stringr)
          #library(xml2)
          #library(kableExtra)
          library(ggplot2)
          #library(ggpubr)
          library(rstatix)
          


          
          #######################################
          # Initial check: No variables selected
          #######################################
          if (is.null(self$options$yyy) || is.null(self$options$xxx)) {
            self$results$warning_message_1$setVisible(FALSE)
            self$results$warning_message_2$setVisible(FALSE)
            self$results$tablestyle_test_1$setVisible(FALSE)
            self$results$tablestyle_test_2$setVisible(FALSE)
            self$results$tablestyle_ph$setVisible(FALSE)
            
            self$results$residual_plot$setVisible(FALSE)
            self$results$data_plot$setVisible(FALSE)
            self$results$ph_interval_plot$setVisible(FALSE)
            self$results$ph_inference_plot$setVisible(FALSE)
            return()
            
          }
  
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          ev <- self$options$ev
          varsName <- self$options$yyy
          byName <- self$options$xxx
          tmpDat <- jmvcore::select(self$data, c(varsName,byName) )
          tmpDat <- as.data.frame(tmpDat)
          
          tmpDat[[byName]] <- factor(tmpDat[[byName]])
          tmpDat[[byName]] <- relevel(tmpDat[[byName]],ref=self$options$refl)
          
          tmpDat[[byName]] <- mis_label_function(tmpDat[[byName]],"Unknown")
          
          if (!self$options$imis){
            tmpDat <- tmpDat[!tmpDat[[byName]]=="Unknown",]
            tmpDat[[byName]] <- droplevels(tmpDat[[byName]])
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
          font_size <- dplyr::case_when(self$options$tasi=="ts"~12,
                                 self$options$tasi=="tm"~16,
                                 self$options$tasi=="tl"~20)
          
          
          ##########################################################
          # Check the condition for warning message 1 (<= 2 levels)
          ##########################################################
          if (nlevels(tmpDat[[byName]]) <= 2) {
            self$results$warning_message_2$setContent("")
            self$results$warning_message_2$setVisible(FALSE)
            self$results$warning_message_1$setContent(
              paste(
                "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                "The selected Grouping variable has less than three levels.<br>",
                "Only three or more levels are supported by this analysis.",
                "</p>"
              )
            )
            self$results$warning_message_1$setVisible(TRUE)
            return() # End the execution here
          }
          
          
          #################################################################################
          # Check the condition for warning message 2 (less than 2 observations per level)
          # You might need a more robust check here, but the principle is the same
          #################################################################################
          if (any(table(tmpDat[[byName]]) < 2)) {
            self$results$warning_message_1$setContent("")
            self$results$warning_message_1$setVisible(FALSE)
            self$results$warning_message_2$setContent(
              paste(
                "<p style='color: #4D9900; font-size: 1.5em; font-weight: bold;'>",
                "One of the levels in the grouping variable has less than 2 observations.<br>",
                "Only three or more observations per level are supported by this analysis.",
                "</p>"
              )
            )
            self$results$warning_message_2$setVisible(TRUE)
            return()
          }
          
          
          # If we get here, all conditions are good. Hide warnings and run the main analysis.
          self$results$warning_message_1$setContent("")
          self$results$warning_message_2$setContent("")
          self$results$warning_message_1$setVisible(FALSE)
          self$results$warning_message_2$setVisible(FALSE)
          
          ###################################
          ### Creates the output (anova)
          ###################################
          call_test <- anova____test(data = tmpDat,
                                 variable = varsName,
                                 by = byName,
                                 nd = nd,
                                 ev=ev,
                                 font_size=font_size,
                                 imis=self$options$imis)
          
          tbl1 <- as.character(call_test$table_summ)
          tbl2 <- as.character(call_test$table_infe)
          
          #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
          wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
          final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
          
          self$results$tablestyle_test_1$setContent(final_html_output1)
          self$results$tablestyle_test_2$setContent(final_html_output2)
          
          #*******************************
          #*** Residuals plot (if chosen)
          #*******************************
          if (self$options$norma_assess_yn) {
            #resi_plot <- call_test$plot_residuals
            #self$results$residual_plot$setState(call_test$plot_residuals) 
            self$results$residual_plot$setState(self$data) 
          } 
          
          
          #*******************************
          #*** Data plot (if chosen)
          #*******************************
          if (self$options$plotdata_yn) {
            #dat_plot <- call_test$plot_data
            #self$results$data_plot$setState(call_test$plot_data)  
            self$results$data_plot$setState(tmpDat) 
            
          } 
          
          
          ########################################
          ### Creates the output (Post-Hoc anova)
          ########################################
          if (self$options$ph_yn) {
            call_ph<- anova____posthoc(data = tmpDat,
                                       variable = varsName,
                                       by = byName,
                                       nd = nd,
                                       ev=ev,
                                       font_size=font_size,
                                       imis=self$options$imis,
                                       type = self$options$method)
            
            tbl1 <- as.character(call_ph)
            final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
            self$results$tablestyle_ph$setContent(final_html_output1)
          } 
            
          if (self$options$ph_yn & self$options$ph_plot_intervals_yn) {
            #resi_plot <- call_ph$plot_intervals
            #self$results$ph_interval_plot$setState(serialize(call_ph$plot_intervals, NULL)) 
            self$results$ph_interval_plot$setState(self$data)
          } 
          
          if (self$options$ph_yn & self$options$ph_plot_significance_yn) {
            #resi_plot <- call_ph$plot_signif
            #self$results$ph_inference_plot$setState(serialize(call_ph$plot_signif,NULL )) 
            self$results$ph_inference_plot$setState(tmpDat) 
          } 
            
          
                
          
        },
        .ghostplot = function(image,...){
            return(FALSE)
        },
        .residual_plot = function(image,...){
          if (is.null(self$options$yyy) | is.null(self$options$xxx)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          

          pt <- anova____residual_plot(data  = image$state,
                                   variable = self$options$yyy,
                                   by = self$options$xxx)
          print(pt)
          return(TRUE)
        },
        .data_plot = function(image,...){
          if (is.null(self$options$yyy) ) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          pt <- anova____data_plot(data  = image$state,
                                           variable = self$options$yyy,
                                           by = self$options$xxx,
                                           imis = self$options$imis)
          print(pt)
          return(TRUE)
        },
        .ph_interval_plot = function(image,...){
          if (is.null(self$options$yyy)) return(FALSE)
          if (is.null(image$state))
            return(FALSE)
          
          #bp <- image$state
          #plot(unserialize(image$state))
          
          pt <- anova____ph_interval_plot(data  = image$state,
                                          variable = self$options$yyy,
                                          by = self$options$xxx,
                                          imis = self$options$imis,
                                          type = self$options$method)
          print(pt)
          return(TRUE)
        },
        .ph_inference_plot = function(image,...){
          if (is.null(self$options$yyy) ) return(FALSE)
          if (is.null(image$state)) return(FALSE)
          
          pt <- anova____ph_inference_plot(data  = image$state,
                                          variable = self$options$yyy,
                                          by = self$options$xxx,
                                          imis = self$options$imis,
                                          type = self$options$method)
          print(pt)
          return(TRUE)
        })
)
