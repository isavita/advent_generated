
input <- as.numeric(readLines("input.txt"))

code <- 20151125
row <- 1
col <- 1

while (TRUE) {
  if (row == 1) {
    row <- col + 1
    col <- 1
  } else {
    row <- row - 1
    col <- col + 1
  }
  
  code <- (code * 252533) %% 33554393
  
  if (row == 2947 && col == 3029) {
    print(code)
    break
  }
}
