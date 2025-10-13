
# This file is a generated template, your changes will not be overwritten

dotplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "dotplotClass",
    inherit = dotplotBase,
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
        if (is.null(self$options$yyy)) return(FALSE)
        
        plotData <- image$state
        
        byName <- self$options$xxx
        varName <- self$options$yyy
        colName <- self$options$ccc
        panName <- self$options$ppp
        imis <- self$options$imis
        mist <- self$options$mist
        fli <- self$options$fli
        logt <- self$options$logt
        
        
        #+++++++++++++++
        #++ Font size
        #+++++++++++++++
        fontsi <- case_when(self$options$fosi=="fs"~15,
                            self$options$fosi=="fm"~20,
                            self$options$fosi=="fl"~25)
        
        #+++++++++++++++
        #++ Points size
        #+++++++++++++++
        rada <- max(plotData[[varName]],na.rm=TRUE) - min(plotData[[varName]],na.rm=TRUE)     # Range of the data
        bw <- case_when(self$options$posi=="ps"~rada/40,
                             self$options$posi=="pm"~rada/30,
                             self$options$posi=="pl"~rada/20)
        
        binwidth = 1.5
        
        tmpDat <- plotData %>% dplyr::select(all_of(c(varName,byName,colName,panName)))
        
        if (!imis) tmpDat <- tmpDat %>% na.omit()
        
        #df <- data.frame(outcome = plotData[[y]])
        #if (!is.null(x)) df$group <- plotData[[x]]
        #if (!is.null(col)) df$color <- plotData[[col]]
        #if (!is.null(pan)) df$panel <- plotData[[panel]]
        
        
        if (is.null(self$options$yax)) ylabe <- varName else ylabe <- self$options$yax
        if (logt) ylabe <- paste("log (",ylabe,")",sep="")
        
        ###############
        ### SCENARIO 1
        ###############
        if (!is.null(varName) & is.null(byName) & is.null(colName)) {
          pt <- ggplot(tmpDat, aes(x = .data[[varName]])) + geom_dotplot(binwidth = bw) +
            scale_y_continuous(NULL, breaks = NULL) 
        }
        
        ###############
        ### SCENARIO 2
        ###############
        if (!is.null(varName) & !is.null(byName) & is.null(colName)) {
          
          if (imis) tmpDat[[byName]] <- mis_label_function(tmpDat[[byName]],mist)
          
          pt <- ggplot(tmpDat, aes(x = .data[[varName]], y = .data[[byName]])) + geom_dotplot(binwidth = bw) 
        }
        
        ###############
        ### SCENARIO 3
        ###############
        if (!is.null(varName) & !is.null(byName) & !is.null(colName)) {
          
          if (imis) {
            tmpDat[[byName]] <- mis_label_function(tmpDat[[byName]],mist)
            tmpDat[[colName]] <- mis_label_function(tmpDat[[colName]],mist)
          } 
          
          pt <- ggplot(tmpDat, aes(x = .data[[varName]], y = .data[[byName]],fill = .data[[colName]])) + geom_dotplot(binwidth = bw) 
          
        }
        
        ###############
        ### SCENARIO 4
        ###############
        if (!is.null(varName) & is.null(byName) & !is.null(colName)) {
          
          if (imis) tmpDat[[colName]] <- mis_label_function(tmpDat[[colName]],mist)
          
          pt <- ggplot(tmpDat, aes(x = .data[[varName]],fill = .data[[colName]])) + geom_dotplot(binwidth = bw) +
            scale_y_continuous(NULL, breaks = NULL) 
        }
        
  
        
        
        if (!is.null(varName) & !is.null(panName)) {
          if (imis) tmpDat[[panName]] <- mis_label_function(tmpDat[[panName]],mist)
          pt <- pt + facet_wrap(~.data[[panName]])
        }
        
        
        
        
        #fp <- p 
        fp <- pt + theme(text = element_text(size = fontsi),legend.position=self$options$lpo) 
        #if (!is.null(self$options$xax) & !is.null(byName)) fp <- fp + xlab(self$options$xax)
        #if (!is.null(self$options$yax)) fp <- fp + ylab(self$options$yax)
        fp <- fp + ylab(ylabe) 
        if (!is.null(self$options$lti)) fp <- fp + labs(fill=self$options$lti)
        if (!is.null(self$options$mti)) {
          fp <- fp + ggtitle(self$options$mti,self$options$msti)+
            theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
        } 
        if (logt) fp <- fp + scale_x_continuous(trans = "log")
        if (fli) fp <- fp + coord_flip()

        print(fp)
        TRUE
        
      })
)
