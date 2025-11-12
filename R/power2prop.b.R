
# This file is a generated template, your changes will not be overwritten

power2propClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "power2propClass",
    inherit = power2propBase,
    private = list(
      .run = function() {
        
        library(dplyr)
        library(stringr)
        library(kableExtra)
        
        


        
        #debug_value <- paste(self$options$solvefor,"\n",self$options$delta,"\n",self$options$power,"\n",
        #                     "n? ",self$options$solvefor=="n","\n",
        #                     "delta? ",is.null(self$options$delta),self$options$delta=="","\n",
        #                     "power? ",is.null(self$options$power),self$options$power=="","\n",sep="")
        #
        #self$results$debug_output$setContent(paste("Debug Check:", debug_value))
        
        if (self$options$solvefor=="n" & any(self$options$p2=="",self$options$power=="")) {
          self$results$tablestyle_html$setVisible(FALSE) 
          self$results$power_plot$setVisible(FALSE) 
          return()
        }
        if (self$options$solvefor=="p2" & any(self$options$n=="",self$options$power=="")) {
          self$results$tablestyle_html$setVisible(FALSE) 
          self$results$power_plot$setVisible(FALSE) 
          return()
        } 
        if (self$options$solvefor=="power" & any(self$options$n=="",self$options$p2=="")) {
          self$results$tablestyle_html$setVisible(FALSE) 
          self$results$power_plot$setVisible(FALSE) 
          return()
        }
        
        
        if (self$options$p1=="") {
          self$results$tablestyle_html$setVisible(FALSE)
          self$results$power_plot$setVisible(FALSE) 
          return()}
        
        n <- as.numeric(unlist(stringr::str_split(self$options$n," "))) 
        p2 <- as.numeric(unlist(stringr::str_split(self$options$p2," "))) 
        power <- as.numeric(unlist(stringr::str_split(self$options$power," "))) 
        p1 <- as.numeric(unlist(stringr::str_split(self$options$p1," "))) 

        #############################################
        ### Warning Check if p2>p1 and alt = greater 
        #############################################
        cond1 <- any(p2 <= p1) & self$options$alt_ttest == "greater" & !self$options$solvefor=="p2"
        cond2 <- any(p2 >= p1) & self$options$alt_ttest == "less" & !self$options$solvefor=="p2"
        if (any(cond1,cond2)) {
          self$results$tablestyle_html$setVisible(FALSE) 
          self$results$power_plot$setVisible(FALSE) 
          if (cond1) {
            self$results$warning_message_1$setContent(
              paste(
                "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                "Warning: p<sub>2</sub> must be greater than p<sub>1</sub> for a <i>greater than</i> alternative hypothesis.",
                "</p>"
              )
            )
            self$results$warning_message_1$setVisible(TRUE)
          }
          if (cond2) {
            self$results$warning_message_1$setContent(
              paste(
                "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
                "Warning: p<sub>2</sub> must be less than p<sub>1</sub> for a <i>less than</i> alternative hypothesis.",
                "</p>"
              )
            )
            self$results$warning_message_1$setVisible(TRUE)
          }
        } else {self$results$warning_message_1$setVisible(FALSE)} 
        
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
        
        

        
        ########################
        ### Creates the output 
        ########################
        results_html <- power2prop_html(
          solvefor = self$options$solvefor,
          n =n,
          p2 = p2,
          p1 = p1,
          sig.level = self$options$sig.level,
          power = power,
          alt_ttest = self$options$alt_ttest,
          font_size = font_size,
          nd = nd
        )
        tbl <- as.character(results_html$table_out)
        #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
        wrapper_div_style <- "width: 150%; max-width: 1300px; overflow-x: auto;"
        final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
        self$results$tablestyle_html$setContent(final_html_output)
        
        
        #*******************************
        #*** Power plot (if chosen)
        #*******************************
        if (self$options$poplyn) {
          #image <- self$results$nomality_plot
          #image$setState(self$data)
          self$results$power_plot$setState(results_html$data_plot)
        }
        
      },
      .power_plot = function(image,...){
        
        plotData <- image$state
        
        pt <- power2prop_plot(plotData)
        print(pt)
        return(TRUE)
      })


)
