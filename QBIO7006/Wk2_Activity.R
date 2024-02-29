# Activity 1 ------
checkDataframe <- function(df){
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)){
      numeric_value <- suppressWarnings(as.numeric(df[i,j]))
      if (is.na(numeric_value)){
        stop("Data frame contains non-numeric values in rows: ",i, " and columns: ",j)
      }
    }
  }
}

data <- data.frame(Column1 = c(1, 2, 3.5 ,"A"), Column2 = c(4, 5, 6, "B"))
checkDataframe(data)

# Activity 2 -----
# https://github.com/JanEngelstaedter/grow96/blob/main/R/makeSpec.R
# initial checks:
if (length(rows) !=nrowsUsed)
  stop(paste(nrowsUsed, "rows expected."))
if (length(columns) !=ncolsUsed)
  stop(paste(ncolsUsed, "columns expected."))

# Activity 3: 
sessionInfo()
