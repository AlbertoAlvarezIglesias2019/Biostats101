
# This file is a generated template, your changes will not be overwritten

densityplotClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "densityplotClass",
    inherit = densityplotBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        library(ggplot2)
        library(viridis)
        library(ggridges)
        
        
        if (is.null(self$options$yyy)) {
          self$results$plot$setVisible(FALSE)
        }
        
        image <- self$results$plot
        image$setState(self$data)
        
      },
      .plot = function(image, ...){
        #if (is.null(self$options$yyy)|is.null(self$options$xxx)) return(FALSE)
        if (is.null(self$options$yyy)) return(FALSE)
        
        
        plotData <- image$state
        
        byName <- self$options$xxx
        varName <- self$options$yyy
        panName <- self$options$ppp
        imis <- self$options$imis
        mist <- self$options$mist
        logt <- self$options$logt
        
        
        #+++++++++++++++
        #++ Font size
        #+++++++++++++++
        fontsi <- case_when(self$options$fosi=="fs"~15,
                            self$options$fosi=="fm"~20,
                            self$options$fosi=="fl"~25)
        
        
        #- name: lti
        #title: Legend title
        #type: String
        lti <- varName
        if (!is.null(self$options$lti)) lti <- self$options$lti
        
        
        #- name: xax
        #title: X-axis label
        #type: String
        xax <- varName
        if (!is.null(self$options$xax)) xax <- self$options$xax
        
        #- name: yax
        #title: Y-axis label
        #type: String
        yax <- NULL
        if (!is.null(self$options$yax)) yax <- self$options$yax
        
        
        tmpDat <- plotData %>% dplyr::select(all_of(c(varName,byName,panName)))
        
        if (!imis) tmpDat <- tmpDat %>% na.omit()
        if (imis & !is.null(byName)) tmpDat[[byName]] <- mis_label_function(tmpDat[[byName]],mist)
        if (imis & !is.null(panName)) tmpDat[[panName]] <- mis_label_function(tmpDat[[panName]],mist)
        
        
        #df <- data.frame(outcome = plotData[[y]])
        #if (!is.null(x)) df$group <- plotData[[x]]
        #if (!is.null(col)) df$color <- plotData[[col]]
        #if (!is.null(pan)) df$panel <- plotData[[panel]]
        
        
        if (is.null(self$options$xax)) xlabe <- varName else xlabe <- self$options$xax
        if (is.null(self$options$yax)) ylabe <- byName else ylabe <- self$options$yax
        
        self$results$messages$setContent(" ")
        if (logt) {
          xlabe <- paste("log (",xlabe,")",sep="")
          tmpDat <- tmpDat %>% filter(.data[[varName]] >0)
          self$results$messages$setContent("<p style='font-size: 20px; font-weight: bold; color: red;'>Negative values removed</p>")
        } 
        
        if (is.null(self$options$xxx)) {
          pt <- ggplot(tmpDat, aes(x = .data[[varName]], 
                                   y = 1, 
                                   fill = ..x..)) +
            geom_density_ridges_gradient(jittered_points = TRUE, position = "raincloud",
                                         alpha = 0.7, scale = 0.9) +
            scale_fill_viridis(name = lti, option = "E") +
            labs(x = xlabe,y=NULL) +
            labs(color=lti) +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        }
        
        if (!is.null(self$options$xxx)) {
          pt <- ggplot(tmpDat, aes(x = .data[[varName]], 
                                   y = .data[[byName]], 
                                   fill = ..x..)) +
            geom_density_ridges_gradient(jittered_points = TRUE, position = "raincloud",
                                         alpha = 0.7, scale = 0.9) +
            scale_fill_viridis(name = lti, option = "E") +
            labs(x = xlabe, y = ylabe) +
            labs(color=lti) 
        }

        
        
        if (!is.null(panName)) {
          pt <- pt + facet_wrap(~.data[[panName]])
        }
        
        
        
        if (logt) pt <- pt + scale_x_continuous(trans = "log")
        
        #fp <- p 
        fp <- pt + theme(text = element_text(size = fontsi),legend.position=self$options$lpo) 
        if (!is.null(self$options$mti)) {
          fp <- fp + ggtitle(self$options$mti,self$options$msti)+
            theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
        } 
        #if (logt) fp <- fp + scale_x_continuous(trans = "log")

        print(fp)
        TRUE
        
      })
)
