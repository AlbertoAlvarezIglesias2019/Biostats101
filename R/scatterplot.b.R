
# This file is a generated template, your changes will not be overwritten

scatterplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "scatterplotClass",
    inherit = scatterplotBase,
    private = list(
      
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        library(ggplot2)
        
        if (is.null(self$options$yyy)|is.null(self$options$xxx)) {
          self$results$plot$setVisible(FALSE)
        }
        

        image <- self$results$plot
        image$setState(self$data)
        
      },
      .plot = function(image, ...){
        if (is.null(self$options$yyy) | is.null(self$options$xxx)) return(FALSE)
        
        
        fp <- scatterplot____plot(data = image$state,
                                  xxx = self$options$xxx,
                                  yyy = self$options$yyy,
                                  ccc = self$options$ccc,
                                  ppp = self$options$ppp,
                                  imis = self$options$imis,
                                  mist = self$options$mist,
                                  yax = self$options$yax,
                                  xax = self$options$xax,
                                  logt = self$options$logt,
                                  fosi = self$options$fosi,
                                  posi = self$options$posi,
                                  lisi = self$options$lisi,
                                  fli = self$options$fli,
                                  saxy = self$options$saxy,
                                  lieq = self$options$lieq,
                                  befi = self$options$befi,
                                  addi = self$options$addi,
                                  mti = self$options$mti,
                                  msti = self$options$msti,
                                  lti = self$options$lti,
                                  lpo = self$options$lpo) 
          
        print(fp)
        return(TRUE)
        
      }
    )
)
