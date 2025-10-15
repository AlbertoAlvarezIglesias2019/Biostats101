
# This file is a generated template, your changes will not be overwritten

tablesClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "tablesClass",
    inherit = tablesBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          if (is.null(self$options$rrr) || is.null(self$options$ccc) ) {
            return()
          }
          
          # `self$data` contains the data
          # `self$options` contains the options
          # `self$results` contains the results object (to populate)
          library(dplyr)
          
          font_size <- case_when(self$options$tasi=="ts"~12,
                                 self$options$tasi=="tm"~16,
                                 self$options$tasi=="tl"~20)
          
          newtable <- tables____html(self$data,
                                     row = self$options$rrr,
                                     col = self$options$ccc,
                                     percent = self$options$perc,
                                     marg  = self$options$marg,
                                     imis = self$options$imis,
                                     missing_text = self$options$mist,
                                     margin_text = self$options$mart,
                                     addp = self$options$addp,
                                     font_size = font_size,
                                     mti = self$options$mti,
                                     nd = self$options$nd)
          
      
          
          newtable <- as.character(newtable$table)
          #newtable <- as.character(newtable)
          wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, newtable)
          
          self$results$tablestyle$setContent(final_html_output1)
          

        })
)
