run_duet <- function(file_path) {
  instructions <- readLines(file_path)
  registers <- setNames(rep(0, 26), letters)
  last_sound <- 0
  i <- 1
  
  while (i <= length(instructions)) {
    parts <- strsplit(instructions[i], " ")[[1]]
    cmd <- parts[1]
    x <- parts[2]
    
    get_value <- function(arg) {
      if (grepl("^[a-z]$", arg)) {
        return(registers[[arg]])
      } else {
        return(as.numeric(arg))
      }
    }
    
    switch(cmd,
           snd = { last_sound <- get_value(x) },
           set = { registers[[x]] <- get_value(parts[3]) },
           add = { registers[[x]] <- registers[[x]] + get_value(parts[3]) },
           mul = { registers[[x]] <- registers[[x]] * get_value(parts[3]) },
           mod = { registers[[x]] <- registers[[x]] %% get_value(parts[3]) },
           rcv = {
             if (get_value(x) != 0) {
               return(last_sound)
             }
           },
           jgz = {
             if (get_value(x) > 0) {
               i <- i + get_value(parts[3]) - 1
             }
           }
    )
    
    i <- i + 1
  }
  return(NULL)
}

result <- run_duet("input.txt")
if (!is.null(result)) {
  cat(result, "\n")
}