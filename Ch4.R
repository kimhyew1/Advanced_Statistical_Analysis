selection_sort <- function(list) {
  n <- length(list)
  
  if (n < 2) {
    return(list)
  }
  
  # browser()    # 특정 포인트의 runtime env 확인 가능
  
  for (i in 1:(n-1)) {
    min_idx <- i
    
    for (j in (i+1):n) {
      if (list[j] < list[min_idx]) {
        min_idx <- j
      }
    }
    
    if (min_idx != i) {
      tmp <- list[min_idx]
      list[min_idx] <- c(list[i])
      list[i] <- tmp
    }
  }
  return(list)
}

old_list <- c(6, 9, 2, 12, 7, 4)
new_list <- selection_sort(old_list)
new_list

selection_sort(c(1))


debug(selection_sort)
undebug(selection_sort)

debugonce(selection_sort)
