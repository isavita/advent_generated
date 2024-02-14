
input <- as.numeric(unlist(strsplit(readLines("input.txt"), "")))

look_and_say <- function(seq) {
  rle_seq <- rle(seq)
  new_seq <- unlist(lapply(1:length(rle_seq$lengths), function(i) c(rle_seq$lengths[i], rle_seq$values[i])))
  return(new_seq)
}

for (i in 1:50) {
  input <- look_and_say(input)
}

length(input)
