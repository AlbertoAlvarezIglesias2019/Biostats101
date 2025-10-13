
# This file is a generated template, your changes will not be overwritten

histogramClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "histogramClass",
    inherit = histogramBase,
    private = list(
      
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        library(ggplot2)
        
        if (is.null(self$options$yyy)) {
          self$results$plot$setVisible(FALSE)
        }
        
        
        image <- self$results$plot
        image$setState(self$data)
        
      },
      .plot = function(image, ...){
        if (is.null(self$options$yyy) ) return(FALSE)
        
        
        fp <- histogram____plot(data = image$state,
                                  yyy = self$options$yyy,
                                  xxx = self$options$xxx,
                                  ppp = self$options$ppp,
                                  imis = self$options$imis, 
                                  mist = self$options$mist,
                                  logt = self$options$logt,
                                  fosi = self$options$fosi, 
                                  lti = self$options$lti,
                                  xax = self$options$xax, 
                                  mti = self$options$mti, 
                                  msti = self$options$msti,
                                  lpo = self$options$lpo,
                                  alpha_hist = 0.6) 
        
        print(fp)
        return(TRUE)
        
      })
)
