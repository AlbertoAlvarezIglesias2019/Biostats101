
# This file is a generated template, your changes will not be overwritten

basicstatisticsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "basicstatisticsClass",
    inherit = basicstatisticsBase,
    private = list(
        .run = function() {

          if (is.null(self$options$yyy)) {
            return()
          }
          
            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
          library(dplyr)

          
          #+++++++++++++++++++++
          #+++ Prepare the data
          #+++++++++++++++++++++
          varsName <- self$options$yyy
          groupName <- self$options$xxx
          tmpDat <- jmvcore::select(self$data, c(varsName,groupName))
          tmpDat <- as.data.frame(tmpDat)
          
           #- name: suca
          #title: Numerical
          #type: List
          #options:
          #  - title: n / N (%)
          #name:  nNp
          #- title: n (%)
          #name:  np
          #- title: % (n)
          #name:  pn
          #default: nNp
          #suca <- case_when(self$options$suca=="n"~"{n}",
          #                  self$options$suca=="p"~"{p}%",
          #                  self$options$suca=="nNp"~"{n} / {N} ({p}%)",
          #                  self$options$suca=="np"~"{n} ({p}%)",
          #                  self$options$suca=="pn"~"{p}% ({n})")
          suca <- self$options$suca

          
          #- name: suco
          #title: Numerical
          #type: List
          #options:
          #  - title: Mean (SD)
          #name:  meansd
          #- title: Median (IQR)
          #name:  medianiqr
          #- title: Median (Q1,Q3)
          #name:  medianqs
          #- title: Mean (SD) [Min,Max]
          #name:  meansdmima
          #default: meansd
          #suco <- case_when(self$options$suco=="meansd"~"{mean} ({sd})",
          #                  self$options$suco=="medianiqr"~"{median} ({IQR})",
          #                  self$options$suco=="medianqs"~"{median} ({p25},{p75})")
          suco <- self$options$suco
          if (self$options$allco) suco <- "all"

          
          #- name: mist
          #title: Missing Label
          #type: String
          mist <- "(Unknown)"
          if (!is.null(self$options$mist)) mist <- self$options$mist
          
          
          #+++++++++++++++++++++++
          #+++ Prepare the table
          #+++++++++++++++++++++++
          #byvar <- NULL
          #if (!is.null(self$options$xxx)) byvar <- self$options$xxx
          
          
          
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


          
          #- name: varl
          #title: "Label: Variable/s"
          #type: String
          #default: "Characteristic"
          varl <- self$options$varl
          if (is.null(varl)) varl <- "Characteristic"

          #- name: nude
          #title: Decimals
          #type: Integer
          #min: 0
          #max: 6
          #default: 1  
          nd_num <- as.numeric(self$options$nd_num)
          nd_cat <- as.numeric(self$options$nd_cat)
          
          
          ###########
          ### Check 
          ###########
          imis <- self$options$imis #if_else(self$options$imis,"ifany","no")
          newtable <- basicstatistics____html(tmpDat,variable = varsName,
                                              by=groupName,
                                              imis=imis,
                                              suca = suca,
                                              suco = suco,
                                              mist = mist,
                                              addp = self$options$addp,
                                              addt = self$options$addt,
                                              addn = self$options$addn,
                                              varl = varl,
                                              heal = self$options$heal,
                                              font_size = font_size,
                                              mti = self$options$mti,
                                              nd_num = nd_num,
                                              nd_cat = nd_cat)
          #newtable <- as.character(newtable$table)
          newtable <- as.character(newtable)
          wrapper_div_style <- "width: 140%; max-width: 1200px; overflow-x: auto;"
          final_html_output1 <- sprintf('<div style="%s">%s</div>', wrapper_div_style, newtable)
          
          self$results$tablestyle$setContent(final_html_output1)
          

        })
)
