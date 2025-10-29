
# This file is a generated template, your changes will not be overwritten

pairedtClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "pairedtClass",
    inherit = pairedtBase,
    private = list(
      .run = function() {
      
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        library(stringr)
        library(kableExtra)
        library(infer)
        library(PairedData)
        
        if (is.null(self$options$var1) | is.null(self$options$var2)) {
          self$results$tablestyle_ttest$setVisible(FALSE)
          self$results$tablestyle_boot$setVisible(FALSE)
          self$results$boot_plot_interval$setVisible(FALSE)
          self$results$boot_plot_null$setVisible(FALSE)
          self$results$data_plot$setVisible(FALSE)
        }
        
        
        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        if (!is.null(self$options$var1) & !is.null(self$options$var2)) {
          var1nam  <- self$options$var1
          var2nam  <- self$options$var2
          
          tmpDat <- jmvcore::select(self$data, c(var1nam,var2nam) )
          tmpDat <- as.data.frame(tmpDat)
          var1 <- tmpDat[[var1nam]]
          var2 <- tmpDat[[var2nam]]
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
        font_size <- case_when(self$options$tasi=="ts"~12,
                               self$options$tasi=="tm"~16,
                               self$options$tasi=="tl"~20)
        
        
        
        ###################################
        ### Creates the output (ttest)
        ###################################
        if (!is.null(self$options$var1) & !is.null(self$options$var2)) {
          conf_ttest <- as.numeric(self$options$conf_ttest)/100
          nh_ttest <- as.numeric(self$options$nh_ttest)
          alt_ttest <- self$options$alt_ttest  
          
          #tbl_boot <- tbl %>% as_gt()
          call_ttest <- pairedt____ttest(var1=var1,
                                         var2=var2,
                                         conf_ttest = conf_ttest,
                                         nh_ttest = nh_ttest,
                                         alt_ttest = alt_ttest,
                                         nd = nd,
                                         font_size=font_size,
                                         imis=self$options$imis,
                                         testyn_ttest=self$options$testyn_ttest)
          tbl <- as.character(call_ttest$table)
          #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
          wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
          final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
          self$results$tablestyle_ttest$setContent(final_html_output)
        }
        
        
        
        ###################################
        ### Creates the output (bootstrap)
        ###################################
        if (!is.null(self$options$var1) & !is.null(self$options$var2) & self$options$boot_yn) {
          conf_boot <- as.numeric(self$options$conf_boot)/100
          alt_boot <- self$options$alt_boot
          
          call_boot <- pairedt____boot(var1 = var1,
                                       var2 = var2,
                                       conf_boot = conf_boot,
                                       alt_boot = alt_boot,
                                       nd = nd,
                                       font_size = font_size,
                                       testyn_boot = self$options$testyn_boot)
          tbl <- as.character(call_boot$table)
          wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
          final_html_output <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl)
          
          self$results$tablestyle_boot$setContent(final_html_output)
          #*************************************
          #*** Bootstrap null distribution plot
          #*************************************
          if (self$options$boot_yn && self$options$boot_nullplot_yn) {
            boot_plot <- call_boot$plot_null
            self$results$boot_plot_null$setState(boot_plot)
          }
          #*********************************
          #*** Bootstrap Visualise Interval
          #*********************************
          if (self$options$boot_yn) {
            boot_plot <- call_boot$plot_interval
            self$results$boot_plot_interval$setState(boot_plot)
          }
        }
        
        
        #*******************************
        #*** Visualise plot (if chosen)
        #*******************************
        if (!is.null(self$options$var1) & !is.null(self$options$var1)) {
          if (self$options$plotdata_yn) {
            #image <- self$results$nomality_plot
            #image$setState(self$data)
            self$results$data_plot$setState(self$data)
          }
        }
        
    },
    .boot_plot_null = function(image,...){
      if (is.null(self$options$var1) | is.null(self$options$var2)) return(FALSE)
      if (is.null(image$state))
        return(FALSE)
      
      bp <- image$state
      print(bp)
      return(TRUE)
    },
    .boot_plot_interval = function(image,...){
      if (is.null(self$options$var1) | is.null(self$options$var2)) return(FALSE)
      if (is.null(image$state))
        return(FALSE)
      
      bp <- image$state
      print(bp)
      return(TRUE)
    },
    .data_plot = function(image,...){
      if (is.null(self$options$var1) | is.null(self$options$var2) | !self$options$plotdata_yn) return(FALSE)
      
      plotData <- image$state
      
      #+++++++++++++++++++++
      #+++ Prepare the data
      #+++++++++++++++++++++
      var1 <- plotData[[self$options$var1]]
      var2 <- plotData[[self$options$var2]]
      
      pd <- PairedData::paired(var1, var2)
      names(pd) <- c(self$options$var1,self$options$var2)
      out <- PairedData::plot(pd,type = "profile") + theme_bw()
      print(out)
      TRUE
    })
)
