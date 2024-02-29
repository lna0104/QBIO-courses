a = "CAGCGGC"
b = "CATCGGTC"

aList = strsplit(a, "")[[1]]
bList = strsplit(b, "")[[1]]
H = matrix(0, length(aList)+1, length(bList)+1)

for (i in (2:nrow(H))){
  for (j in (2: ncol(H))){
    if (aList[i-1] == bList[j-1]){
      value_1 = H[i-1,j-1] + 2
    }else {
      value_1 = H[i-1,j-1] - 2
    }
    value_2 = max(H[(i-1):1, j] + 1:(i-1) * (-10))
    value_3 = max(H[i, (j-1):1] + 1:(j-1) * (-10))
    H[i,j] = max(value_1, value_2, value_3, 0L)
  }
}



# Function

algorithmSmithWaterman <- function(a, b, match_score, mismatch_score, gap_score){
  aList = strsplit(a, "")[[1]]
  bList = strsplit(b, "")[[1]]
  H = matrix(0, length(aList)+1, length(bList)+1)
  
  for (i in (2:nrow(H))){
    for (j in (2: ncol(H))){
      if (aList[i-1] == bList[j-1]){
        value_1 = H[i-1,j-1] + match_score
      }else {
        value_1 = H[i-1,j-1] + mismatch_score
      }
      value_2 = max(H[(i-1):1, j] + 1:(i-1) * gap_score)
      value_3 = max(H[i, (j-1):1] + 1:(j-1) * gap_score)
      H[i,j] = max(value_1, value_2, value_3, 0L)
    }
  }

  return (H)
}

result = algorithmSmithWaterman(a, b, 2, -2, -1)
