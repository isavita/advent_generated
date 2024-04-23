mulCount <- 0
registers <- list()
pointer <- 1
instructions <- readLines("input.txt")

# Check if the last line of the file is incomplete
if (nchar(instructions[length(instructions)]) == 0) {
  instructions <- instructions[-length(instructions)]
}

while(pointer >= 1 && pointer <= length(instructions)){
  parts <- strsplit(instructions[pointer], "\\s+")[[1]]
  cmd <- parts[1]
  x <- parts[2]
  y <- if(length(parts) > 2) parts[3] else NA
  
  getValue <- function(s){
    if(grepl("^-?[0-9]+$", s)){
      return(as.integer(s))
    } else {
      if(is.null(registers[[s]])) {
        return(0)
      } else {
        return(registers[[s]])
      }
    }
  }
  
  if(cmd == "set"){
    registers[[x]] <- getValue(y)
  } else if(cmd == "sub"){
    if(is.null(registers[[x]])) registers[[x]] <- 0
    registers[[x]] <- registers[[x]] - getValue(y)
  } else if(cmd == "mul"){
    if(is.null(registers[[x]])) registers[[x]] <- 0
    registers[[x]] <- registers[[x]] * getValue(y)
    mulCount <- mulCount + 1
  } else if(cmd == "jnz"){
    if(!is.null(getValue(x)) && getValue(x) != 0){
      pointer <- pointer + getValue(y) - 1
    }
  }
  pointer <- pointer + 1
}

print(mulCount)