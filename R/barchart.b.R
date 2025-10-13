
# This file is a generated template, your changes will not be overwritten

barchartClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "barchartClass",
    inherit = barchartBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        library(ggplot2)
        library(GGally)
        
        sumyn <- dplyr::if_else(!is.null(self$options$summ_data) & !is.null(self$options$summ_lab),TRUE,FALSE)
        rawyn <- dplyr::if_else(!is.null(self$options$yyy),TRUE,FALSE)
        
        
        #######################################
        # Initial check: No variables selected
        #######################################
        #if (is.null(self$options$rrr) || is.null(self$options$ccc)) {
        if (!sumyn & !rawyn) {
          self$results$plot$setVisible(FALSE)
          self$results$warning_message$setVisible(FALSE)
          return()
        }
        

        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        if (!sumyn & rawyn) {
          self$results$warning_message$setVisible(FALSE)
          
          yyy <- self$options$yyy
          xxx <- self$options$xxx
          ppp <- self$options$ppp
          tmpDat <- jmvcore::select(self$data, c(yyy,xxx,ppp) )
          tmpDat <- as.data.frame(tmpDat)
          imis <- self$options$imis
        }
        
        if (sumyn) {
          self$results$warning_message$setContent(paste(
            "<h1 style='background-color:Tomato; color:White; font-size: 1.5em; font-weight: bold;'>",
            "Note:",
            "</h1>",
            "<p style='color: #D35400; font-size: 1.5em; font-weight: bold;'>",
            "This analysis was performed on summarized data.",
            "</p>"
          ))
          
          
          dat <- jmvcore::select(self$data, c(self$options$summ_data,self$options$summ_lab) )
          dat <- dat %>%
            tidyr::pivot_longer(all_of(self$options$summ_data),names_to = "Group",values_to = "N") %>% 
            dplyr::filter(!is.na(N))
          
          
          tmpDat <- dat %>% tidyr::uncount(N)
          
          yyy <- self$options$summ_lab
          xxx <- "Group"
          ppp <- NULL
          tmpDat <- as.data.frame(tmpDat)
          
          tmpDat[[yyy]] <- factor(tmpDat[[yyy]])
          tmpDat[[yyy]] <- relevel(tmpDat[[yyy]],ref=self$options$summ_lab_ref)
          
          tmpDat[[xxx]] <- factor(tmpDat[[xxx]])
          tmpDat[[xxx]] <- relevel(tmpDat[[xxx]],ref=self$options$summ_data[1])
          imis <- FALSE
        }
        
        #image <- self$results$plot
        #image$setState(self$data)
        self$results$plot$setState(list(data = tmpDat,yyy = yyy,xxx=xxx, ppp = ppp,imis = imis))
        
      },
      .plot = function(image, ...){
        #if (is.null(self$options$yyy)) return(FALSE)
        
        pt <- barchart____plot(
           data = image$state$data,
           yyy = image$state$yyy,
           xxx = image$state$xxx,
           ppp = image$state$ppp,
           stac = self$options$stac,
           imis = image$state$imis,
           ilab = self$options$ilab,
           fli = self$options$fli,
           rota = self$options$rota,
           lpo = self$options$lpo,
           tasi = self$options$tasi,
           fosi = self$options$fosi,
           suty = self$options$suty,
           mti = self$options$mti,
           msti = self$options$msti,
           lti = self$options$lti,
           mist = self$options$mist,
           nd = self$options$nd
         )
 

        print(pt)
        TRUE
        
      })
)
