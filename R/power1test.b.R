
# This file is a generated template, your changes will not be overwritten

power1testClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "power1testClass",
    inherit = power1testBase,
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
          
          if (self$options$solvefor=="n" & any(self$options$delta=="",self$options$power=="")) {
            self$results$tablestyle_html$setVisible(FALSE) 
            self$results$power_plot$setVisible(FALSE) 
            return()
          }
          if (self$options$solvefor=="delta" & any(self$options$n=="",self$options$power=="")) {
            self$results$tablestyle_html$setVisible(FALSE) 
            self$results$power_plot$setVisible(FALSE) 
            return()
          } 
          if (self$options$solvefor=="power" & any(self$options$n=="",self$options$delta=="")) {
            self$results$tablestyle_html$setVisible(FALSE) 
            self$results$power_plot$setVisible(FALSE) 
            return()
          }
          
          
          if (self$options$sd=="") {
            self$results$tablestyle_html$setVisible(FALSE)
            self$results$power_plot$setVisible(FALSE) 
            return()}
           
          
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
          
          
          n <- as.numeric(unlist(stringr::str_split(self$options$n," "))) 
          delta <- as.numeric(unlist(stringr::str_split(self$options$delta," "))) 
          power <- as.numeric(unlist(stringr::str_split(self$options$power," "))) 
          sd <- as.numeric(unlist(stringr::str_split(self$options$sd," "))) 
          
          ########################
          ### Creates the output 
          ########################
          results_html <- power1ttest_html(
            solvefor = self$options$solvefor,
            n =n,
            delta = delta,
            sd = sd,
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
          
          pt <- power1ttest_plot(plotData)
          print(pt)
          return(TRUE)
        })
)
