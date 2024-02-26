
BEGIN {
  FS = " "
}

{
  opponent = $1
  outcome = $2

  if (outcome == "X") {
    my_choice = (opponent == "A") ? "C" : (opponent == "B") ? "A" : "B"
    score += 0
  } else if (outcome == "Y") {
    my_choice = opponent
    score += 3
  } else {
    my_choice = (opponent == "A") ? "B" : (opponent == "B") ? "C" : "A"
    score += 6
  }

  score += (my_choice == "A") ? 1 : (my_choice == "B") ? 2 : 3
}

END {
  print score
}
