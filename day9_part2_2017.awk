
BEGIN {
  score = 0
  in_garbage = 0
  cancel_next = 0
  garbage_count = 0
}

{
  for (i = 1; i <= length($0); i++) {
    char = substr($0, i, 1)

    if (!in_garbage) {
      if (char == "{") {
        score++
      } else if (char == "}") {
        score--
      } else if (char == "<") {
        in_garbage = 1
      }
    } else {
      if (cancel_next) {
        cancel_next = 0
      } else if (char == "!") {
        cancel_next = 1
      } else if (char == ">") {
        in_garbage = 0
      } else {
        garbage_count++
      }
    }
  }
}

END {
  print "Total score: ", score
  print "Garbage characters count: ", garbage_count
}
