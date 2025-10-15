#' @title Internal Cross-Tabulation with Percentages and Statistical Test
#'
#' @description
#' An internal utility function to create a cross-tabulation table from two
#' variables, handle missing values, calculate cell contents based on a percentage
#' type, and perform a Chi-squared or Fisher's exact test. It prepares the data
#' in a wide format for subsequent formatting functions like \code{tables____html}.
#'
#' @param data A data frame or tibble.
#' @param row A character string specifying the name of the column for the rows.
#' @param col A character string specifying the name of the column for the columns.
#' @param perc A character string specifying the type of percentage to include
#'   in the cell content. Default is \code{"none"}.
#'   \itemize{
#'     \item \code{"none"}: Counts only.
#'     \item \code{"colu"}: Column percentages.
#'     \item \code{"row"}: Row percentages.
#'     \item \code{"cell"}: Cell percentages.
#'   }
#' @param nd number of decimals for percentages Default is \code{"1"}.
#'
#' @details
#' The function converts missing values (\code{NA}) to a special factor level "MMM"
#' to include them in the initial tabulation. It then calculates the requested
#' percentages based on the non-missing total for the relevant margin (or overall
#' total for cell percentages).
#'
#' It performs a statistical test (Chi-squared or Fisher's Exact) on the table
#' *excluding* missing values, based on the expected cell count rule (5 is the threshold).
#'
#' The output structure includes special rows/columns for Sums and MMM (Missing),
#' as well as the test name and p-value.
#'
#' @return A data frame (tibble) in wide format where:
#' \itemize{
#'   \item The first column is \code{RRR} (Row variable levels).
#'   \item Subsequent columns are the levels of the \code{CCC} (Column variable levels),
#'         containing the count and percentage string (e.g., "10 (20%)").
#'   \item Includes special rows/columns for \code{Sum} and \code{MMM}.
#'   \item Two final columns: \code{test_name} (character string) and \code{p.value} (numeric).
#' }
#' @export
#'
#' @importFrom base table addmargins data.frame names all suppressWarnings
#' @importFrom stats chisq.test fisher.test
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% mutate filter select left_join if_else
#' @importFrom stringr str_remove
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_tbl_cross' is part of an R package.
#' data(iris)
#'
#' # Convert Species to character and add some NA for demonstration
#' iris$Species_char <- as.character(iris$Species)
#' iris$Species_char[1:5] <- NA
#' 
#'
#' # Convert Petal.Length to a factor with NAs
#' iris$PL_grp <- cut(iris$Petal.Length, breaks = c(0, 3, 5, 7),
#'                    labels = c("Short", "Medium", "Long"), include.lowest = TRUE)
#' iris$PL_grp[c(3,51:55)] <- NA
#'
#' # Example 1: Column percentages
#' tab_col <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "cell")
#' # print(tab_col)
#'
#' # Example 2: No percentages (counts only)
#' tab_none <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "none")
#' # print(tab_none)
#'
#' # Example 3: Cell percentages
#' tab_cell <- my_tbl_cross(iris, row = "PL_grp", col = "Species_char", perc = "cell")
#' # print(tab_cell)
#' }
my_tbl_cross <- function(data, row, col, perc = "none",nd=1) {
  
  # 1. Prepare data and handle missing values
  tmpDat <- data.frame(
    CCC = data[[col]], # Column Variable
    RRR = data[[row]]  # Row Variable
  )
  
  ndp <- nd
  
  tmpDat$CCC <- base::addNA(tmpDat$CCC)
  tmpDat$RRR <- base::addNA(tmpDat$RRR)
  
  # Rename NA level to "MMM" (Missing)
  base::levels(tmpDat$CCC)[base::is.na(base::levels(tmpDat$CCC))] <- "MMM"
  base::levels(tmpDat$RRR)[base::is.na(base::levels(tmpDat$RRR))] <- "MMM"
  
  # 2. Initial Cross-tabulation with Margins
  ddd1 <- base::table(tmpDat$CCC, tmpDat$RRR, deparse.level = 2)
  ddd1 <- stats::addmargins(ddd1)
  
  # Convert to data frame and clean up names
  ddd1 <- base::data.frame(ddd1)
  base::names(ddd1) <- stringr::str_remove(base::names(ddd1), "tmpDat.")
  
  # 3. Calculate Cell Content (Counts or Counts + Percentage)
  
  if (perc == "none") {
    out <- ddd1 %>%
      dplyr::mutate(value = Freq)
  }
  
  if (perc == "colu") {
    # Get column totals (N) and missing counts (MIS) for percentage base
    tt1 <- ddd1 %>%dplyr::filter(RRR == "Sum") %>%dplyr::select(CCC, N = Freq)
    tt2 <- ddd1 %>%dplyr::filter(RRR == "MMM") %>%dplyr::select(CCC, MIS = Freq)
    
    out <- ddd1 %>%dplyr::left_join(tt1, by = "CCC") %>%dplyr::left_join(tt2, by = "CCC")
    
    # Recalculate 'Sum' row to be non-missing total
    out <- out %>%dplyr::mutate(Freq = dplyr::if_else(RRR == "Sum" & !CCC == "MMM", N - MIS, Freq))
    
    # Create the display value: Freq (Percentage)
    out <- out %>%dplyr::mutate(value = dplyr::if_else(CCC == "MMM" | RRR == "MMM",
                                                       base::as.character(Freq),
                                                       base::paste(Freq, " (", ndformat(Freq / (N - MIS) * 100, ndp), "%)", sep = "")
    ))
  }
  
  if (perc == "row") {
    # Get row totals (N) and missing counts (MIS) for percentage base
    tt1 <- ddd1 %>% dplyr::filter(CCC == "Sum") %>% dplyr::select(RRR, N = Freq) 
    tt2 <- ddd1 %>% dplyr::filter(CCC == "MMM") %>% dplyr::select(RRR, MIS = Freq)
    
    out <- ddd1 %>% dplyr::left_join(tt1, by = "RRR") %>% dplyr::left_join(tt2, by = "RRR") 
    
    # Recalculate 'Sum' column to be non-missing total
    out <- out %>% dplyr::mutate(Freq = dplyr::if_else(CCC == "Sum"& !RRR == "MMM", N - MIS, Freq))
    
    # Create the display value: Freq (Percentage)
    out <- out %>%
      dplyr::mutate(value = dplyr::if_else(CCC == "MMM" | RRR == "MMM",
                                           base::as.character(Freq),
                                           base::paste(Freq, " (", ndformat(Freq / (N - MIS) * 100, ndp), "%)", sep = "")
      ))
  }
  
  if (perc == "cell") {
    
    # Get column-wise totals
    tt1_ccc <- ddd1 %>% dplyr::filter(RRR == "Sum") %>% dplyr::select(CCC, Nccc = Freq)
    tt2_ccc <- ddd1 %>% dplyr::filter(RRR == "MMM") %>% dplyr::select(CCC, MISccc = Freq)
    outccc <- ddd1 %>% dplyr::left_join(tt1_ccc, by = "CCC") %>% dplyr::left_join(tt2_ccc, by = "CCC")
    
    # Get row-wise totals
    tt1_rrr <- ddd1 %>% dplyr::filter(CCC == "Sum") %>% dplyr::select(RRR, Nrrr = Freq)
    tt2_rrr <- ddd1 %>% dplyr::filter(CCC == "MMM") %>% dplyr::select(RRR, MISrrr = Freq)
    outrrr <- ddd1 %>% dplyr::left_join(tt1_rrr, by = "RRR") %>% dplyr::left_join(tt2_rrr, by = "RRR")
    
    out <- dplyr::left_join(outccc, outrrr, by = c("CCC", "RRR", "Freq"))
    
    # Recalculate 'Sum' margins to exclude missing totals
    misinboth <- out %>% filter(CCC=="MMM" & RRR=="MMM") %>% pull(MISccc)
    out <- out %>%
      dplyr::mutate(newfreq = dplyr::if_else(CCC == "Sum"&!RRR == "MMM" & !RRR == "Sum", Nrrr - MISrrr, Freq)) %>%
      dplyr::mutate(newfreq = dplyr::if_else(RRR == "Sum" & !CCC == "MMM" & !CCC == "Sum", Nccc - MISccc, newfreq)) %>%
      dplyr::mutate(newfreq = dplyr::if_else(RRR == "Sum" & CCC == "Sum", Freq - MISccc - MISrrr +misinboth, newfreq))
    
    # Recalculate the grand total cell (RRR="Sum" and CCC="Sum") to be non-missing total
    NN <- out %>%
      dplyr::filter(RRR == "Sum" & CCC == "Sum") %>%
      dplyr::pull(newfreq)
    
    # Create the display value: Freq (Percentage)
    out <- out %>%
      dplyr::mutate(value = dplyr::if_else(CCC == "MMM" | RRR == "MMM",
                                           base::as.character(Freq),
                                           base::paste(newfreq, " (", ndformat(newfreq / NN * 100, ndp), "%)", sep = "")
      ))
  }
  
  # 4. Pivot to Wide Format
  out <- out %>%
    dplyr::select(CCC, RRR, value) %>%
    tidyr::pivot_wider(names_from = "CCC", values_from = "value")
  
  # 5. Add a placeholder row for the column variable label
  ttt <- out[1, ]
  ttt[1, ] <- NA
  ttt[, 1] <- base::as.character(ttt[, 1])
  ttt[1, "RRR"] <- row # Use the row variable name here
  
  out <- base::rbind(ttt, out)
  
  # 6. Perform Statistical Test (on non-missing data)
  tmpDat <- tmpDat %>%
    dplyr::mutate(CCC = dplyr::if_else(CCC == "MMM", base::as.character(NA), base::as.character(CCC))) %>%
    dplyr::mutate(RRR = dplyr::if_else(RRR == "MMM", base::as.character(NA), base::as.character(RRR)))
  
  # Table excluding NAs
  tabla <- base::table(tmpDat$CCC, tmpDat$RRR)
  
  # Check for Chi-squared assumption (Expected count >= 5)
  chi_test <- base::suppressWarnings(stats::chisq.test(tabla, correct = FALSE))
  if (base::all(chi_test$expected >= 5)) {
    p_val <- chi_test$p.value
    test_used <- "Chi-square test"
  } else {
    # If assumption violated, use Fisher's exact test
    fisher_test <- stats::fisher.test(tabla)
    p_val <- fisher_test$p.value
    test_used <- "Fisher's exact test"
  }
  
  # 7. Add test results and return
  out$test_name <- test_used
  out$p.value <- p_val
  
  return(out)
}