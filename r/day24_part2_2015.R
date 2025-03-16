
find_ideal_grouping_four <- function(packages) {
  total_weight <- sum(packages)
  group_weight <- total_weight %/% 4
  
  for (group_size in 1:length(packages)) {
    valid_groups <- list()
    first_groups <- combn(packages, group_size, simplify = FALSE)
    
    for (first_group in first_groups) {
      if (sum(first_group) == group_weight) {
        remaining_after_first <- setdiff(packages, first_group)
          
          for (second_group_size in 1:length(remaining_after_first)){
              second_groups <- combn(remaining_after_first, second_group_size, simplify = FALSE)
              
              for(second_group in second_groups){
                  if (sum(second_group) == group_weight){
                      remaining_after_second <- setdiff(remaining_after_first, second_group)
                      
                      if(length(remaining_after_second)>0){
                        for(third_group_size in 1:length(remaining_after_second)){
                            third_groups <- combn(remaining_after_second, third_group_size, simplify = FALSE)
                            for (third_group in third_groups){
                                if(sum(third_group) == group_weight &&
                                   sum(setdiff(remaining_after_second, third_group))== group_weight){
                                  valid_groups <- c(valid_groups, list(first_group))
                                    break
                                }
                            }
                            if(length(valid_groups)>0) break
                        }
                      }
                      
                      
                  }
              }
              if(length(valid_groups) > 0) {
                break
              }
          }
      }
      if (length(valid_groups) > 0) {
        break
      }
    }
    
    if (length(valid_groups) > 0) {
      min_qe <- Inf
      for (group in valid_groups) {
        qe <- prod(group)
        if (qe < min_qe) {
          min_qe <- qe
        }
      }
      return(min_qe)
    }
  }
  return(NULL)
}

main <- function() {
  packages <- as.integer(readLines("input.txt"))
  result <- find_ideal_grouping_four(packages)
  cat(result)
}

main()
