
# This file is a generated template, your changes will not be overwritten

chisquareClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "chisquareClass",
    inherit = chisquareBase,
    private = list(
      .run = function() {
        
        # `self$data` contains the data
        # `self$options` contains the options
        # `self$results` contains the results object (to populate)
        library(dplyr)
        #library(stringr)
        #library(xml2)
        #library(kableExtra)
        library(ggplot2)
        #library(ggpubr)
        #library(rstatix)
        
        sumyn <- dplyr::if_else(!is.null(self$options$summ_data) & !is.null(self$options$summ_lab),TRUE,FALSE)
        rawyn <- dplyr::if_else(!is.null(self$options$rrr) & !is.null(self$options$ccc),TRUE,FALSE)
        
        #mes <- dplyr::case_when(sumyn~"Analysis done on summarised data",
        #                        !sumyn & rawyn~"Analysis done on raw data",
        #                        !sumyn & !rawyn ~ "No analysis")
 
        
        #######################################
        # Initial check: No variables selected
        #######################################
        #if (is.null(self$options$rrr) || is.null(self$options$ccc)) {
        if (!sumyn & !rawyn) {
          self$results$tablestyle_test_1$setVisible(FALSE)
          self$results$tablestyle_test_2$setVisible(FALSE)
          self$results$tablestyle_ph$setVisible(FALSE)
          self$results$warning_message$setVisible(FALSE)
          
          self$results$data_plot$setVisible(FALSE)
          self$results$ph_interval_plot$setVisible(FALSE)
          return()
          
        }
        
        
        
        #+++++++++++++++++++++
        #+++ Prepare the data
        #+++++++++++++++++++++
        if (!sumyn & rawyn) {
          self$results$warning_message$setVisible(FALSE)
          
          varsName <- self$options$rrr
          byName <- self$options$ccc
          tmpDat <- jmvcore::select(self$data, c(varsName,byName) )
          tmpDat <- as.data.frame(tmpDat)
          
          tmpDat[[varsName]][tmpDat[[varsName]]==""]<-NA
          tmpDat[[varsName]] <- factor(tmpDat[[varsName]])
          tmpDat[[varsName]] <- relevel(tmpDat[[varsName]],ref=self$options$refr)
          
          tmpDat[[byName]][tmpDat[[byName]]==""]<-NA
          tmpDat[[byName]] <- factor(tmpDat[[byName]])
          tmpDat[[byName]] <- relevel(tmpDat[[byName]],ref=self$options$refc)
          
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
          
          varsName <- self$options$summ_lab
          byName <- "Group"
          tmpDat <- as.data.frame(tmpDat)
          
          tmpDat[[varsName]] <- factor(tmpDat[[varsName]])
          tmpDat[[varsName]] <- relevel(tmpDat[[varsName]],ref=self$options$summ_lab_ref)
          
          tmpDat[[byName]] <- factor(tmpDat[[byName]])
          tmpDat[[byName]] <- relevel(tmpDat[[byName]],ref=self$options$summ_data[1])
          
          imis <- FALSE
        }

        
        #tmpDat[[byName]] <- mis_label_function(tmpDat[[byName]],"Unknown")
        #tmpDat[[varsName]] <- mis_label_function(tmpDat[[varsName]],"Unknown")
        
        #if (!self$options$imis){
        #  tmpDat <- tmpDat[!tmpDat[[byName]]=="Unknown",]
        #  tmpDat[[byName]] <- droplevels(tmpDat[[byName]])
        #  
        #  tmpDat <- tmpDat[!tmpDat[[varsName]]=="Unknown",]
        #  tmpDat[[varsName]] <- droplevels(tmpDat[[varsName]])
        #} 
        
        
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
        font_size <- dplyr::case_when(self$options$tasi=="ts"~12,
                                      self$options$tasi=="tm"~16,
                                      self$options$tasi=="tl"~20)
        
        
        
        ###################################
        ### Creates the output (chi square)
        ###################################
        call_test <- chisquare____test(data = tmpDat,
                                   rrr = varsName,
                                   ccc = byName,
                                   nd = nd,
                                   font_size=font_size,
                                   imis = imis)
        
        tbl1 <- as.character(call_test$table_summ)
        tbl2 <- as.character(call_test$table_infe)
        
        #wrapper_div_style <- "max-width: 100%; overflow-x: auto;"
        wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
        final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
        final_html_output2 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl2)
        
        self$results$tablestyle_test_1$setContent(final_html_output1)
        self$results$tablestyle_test_2$setContent(final_html_output2)
        
        #*******************************
        #*** Data plot (if chosen)
        #*******************************
        if (self$options$plotdata_yn) {
          #dat_plot <- call_test$plot_data
          #self$results$data_plot$setState(call_test$plot_data)  
          self$results$data_plot$setState(list(data = tmpDat,variable = varsName, by = byName)) 
          
        } 
        
        
        ########################################
        ### Creates the output (Post-Hoc chisquare)
        ########################################
        if (self$options$ph_yn) {
          if (self$options$transpose_yn) {
            call_ph<- chisquare____posthoc(data = tmpDat,
                                           rrr = byName,
                                           ccc = varsName,
                                           nd = nd,
                                           font_size=font_size,
                                           type = self$options$method)
          } else {
            call_ph<- chisquare____posthoc(data = tmpDat,
                                           rrr = varsName,
                                           ccc = byName,
                                           nd = nd,
                                           font_size=font_size,
                                           type = self$options$method)
          }

          
          tbl1 <- as.character(call_ph)
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, tbl1)
          self$results$tablestyle_ph$setContent(final_html_output1)
        } 
        
        if (self$options$ph_yn & self$options$ph_plot_intervals_yn) {

          if (self$options$transpose_yn) {
            self$results$ph_interval_plot$setState(list(data = tmpDat,variable = byName, by = varsName))
          } else {
            self$results$ph_interval_plot$setState(list(data = tmpDat,variable = varsName, by = byName))
          }
        } 
        

      },
      .ghostplot = function(image,...){
        return(FALSE)
      },
      .data_plot = function(image,...){
        #if (is.null(self$options$rrr) ) return(FALSE)
        if (is.null(image$state)) return(FALSE)
        
        pt <- chisquare____data_plot(data  = image$state$data,
                                 variable = image$state$variable,  #self$options$rrr,
                                 by = image$state$by )  #self$options$ccc)
        print(pt)
        return(TRUE)
      },
      .ph_interval_plot = function(image,...){
        #if (is.null(self$options$rrr)) return(FALSE)
        if (is.null(image$state)) return(FALSE)
        
        #bp <- image$state
        #plot(unserialize(image$state))
        
        pt <- chisquare____ph_interval_plot(data  = image$state$data,
                                            rrr = image$state$variable,  #self$options$rrr,
                                            ccc = image$state$by)
        print(pt)
        return(TRUE)
      })
)
