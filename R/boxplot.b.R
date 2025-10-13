
# This file is a generated template, your changes will not be overwritten

boxplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "boxplotClass",
    inherit = boxplotBase,
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

          x <- self$options$xxx
          y <- self$options$yyy
          col <- self$options$ccc
          pan <- self$options$ppp
          imis <- self$options$imis
          mist <- self$options$mist
          
          #plotData[,x] <- as.character(plotData[,x])
          #plotData[,col] <- as.character(plotData[,col])
          #plotData[,pan] <- as.character(plotData[,pan])
          
          
          if (is.null(self$options$yax)) ylabe <- y else ylabe <- self$options$yax
          if (self$options$logt) ylabe <- paste("log (",ylabe,")",sep="")
          
          plot <- NULL

          #+++++++++++++++
          #++ Font size
          #+++++++++++++++
          fontsi <- case_when(self$options$fosi=="fs"~15,
                              self$options$fosi=="fm"~20,
                              self$options$fosi=="fl"~25)
          
          if (!is.null(y) & !is.null(pan) & !imis) {
            plotData <- plotData[ !is.na(plotData[[pan]]), ]
          }
          
          if (!is.null(y) & !is.null(pan) & imis) {
            plotData[,pan] <- mis_label_function(plotData[,pan],mist)
            #plotData[,pan][is.na(plotData[,pan])] <- mist
          }
          
          p <- ggplot(plotData, aes())
          #p <- ggplot(plotData, aes(x=1,y=.data[[y]]))+ scale_x_discrete() + geom_boxplot() 
          
          if (!is.null(y) & is.null(x) & is.null(col)) {
            p <- ggplot(plotData, aes(x=1,y=.data[[y]]))+ scale_x_discrete() + geom_boxplot(size=2) +xlab(NULL)
          }
          
          if (!is.null(y) & !is.null(x) & is.null(col)) {
            
            if (!imis) plotData <- plotData[ !is.na(plotData[[x]]) ,]
            if (imis) plotData[,x] <- mis_label_function(plotData[,x],mist) #plotData[,x][is.na(plotData[,x])] <- mist

            
            p <- ggplot(plotData, aes(x=.data[[x]],y=.data[[y]])) + geom_boxplot(size=2) 
          } 
          if (!is.null(y) & !is.null(x) & !is.null(col)) {
            
            if (!imis) plotData <- plotData[ !is.na(plotData[[x]]) & !is.na(plotData[[col]]) ,]
            if (imis) {
              plotData[,x] <- mis_label_function(plotData[,x],mist) #plotData[,x][is.na(plotData[,x])] <- mist
              plotData[,col] <- mis_label_function(plotData[,col],mist) #plotData[,col][is.na(plotData[,col])] <- mist
            } 

            
            p <- ggplot(plotData, aes(x=.data[[x]],y=.data[[y]],color=.data[[col]])) + geom_boxplot(size=2) 
          } 
          if (!is.null(y) & is.null(x) & !is.null(col)) {
            
            if (!imis) plotData <- plotData[ !is.na(plotData[[col]]), ]
            if (imis) plotData[,col] <- mis_label_function(plotData[,col],mist) #plotData[,col][is.na(plotData[,col])] <- mist
            
            
            p <- ggplot(plotData, aes(x=1,y=.data[[y]],color=.data[[col]]))+ scale_x_discrete() + geom_boxplot(size=2) +xlab(NULL)
          } 
          
          
          if (self$options$reli) {
            p <- p + geom_hline(yintercept = as.numeric(self$options$rlva),colour = "blue",linewidth = 3,linetype = "longdash" )
          }
          
          
          if (!is.null(y) & !is.null(pan)) {
            p <- p +  facet_wrap(pan,labeller = label_both)
          }


          
          
          #fp <- p 
          fp <- p + theme(text = element_text(size = fontsi),legend.position=self$options$lpo) 
          if (!is.null(self$options$xax) & !is.null(x)) fp <- fp + xlab(self$options$xax)
          #if (!is.null(self$options$yax)) fp <- fp + ylab(self$options$yax)
          fp <- fp + ylab(ylabe) 
          if (!is.null(self$options$lti)) fp <- fp + labs(color=self$options$lti)
          if (!is.null(self$options$mti)) {
            fp <- fp + ggtitle(self$options$mti,self$options$msti)+
              theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
          } 
          if (self$options$logt) fp <- fp + scale_y_continuous(trans = "log")
          if (self$options$fli) fp <- fp + coord_flip()
          if (self$options$dat & !is.null(col)) fp <- fp + geom_jitter(position=position_dodge2(0.8,padding=0.05),shape = 5,size=3)
          if (self$options$dat & is.null(col)) fp <- fp + geom_jitter(width = 0.2,shape = 5,size = 3)
          
          print(fp)
          TRUE
          
        })
)
