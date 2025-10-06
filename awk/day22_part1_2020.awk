BEGIN {
  head1 = 1; tail1 = 0;
  head2 = 1; tail2 = 0;
  current = 1;

  while ((getline line < "input.txt") > 0) {
    if (line ~ /^P/) { continue }
    if (line == "") { current = 2; continue }
    val = line + 0
    if (current == 1) {
      tail1++; deck1[tail1] = val
    } else {
      tail2++; deck2[tail2] = val
    }
  }

  while ((tail1 - head1 + 1) > 0 && (tail2 - head2 + 1) > 0) {
    card1 = deck1[head1]; head1++
    card2 = deck2[head2]; head2++
    if (card1 > card2) {
      tail1++; deck1[tail1] = card1
      tail1++; deck1[tail1] = card2
    } else {
      tail2++; deck2[tail2] = card2
      tail2++; deck2[tail2] = card1
    }
  }

  size = (tail1 >= head1 ? tail1 - head1 + 1 : 0)
  if (size > 0) {
    start = head1
    winnerDeck = 1
  } else {
    size = (tail2 >= head2 ? tail2 - head2 + 1 : 0)
    start = head2
    winnerDeck = 2
  }

  score = 0
  for (i = 0; i < size; i++) {
    idx = start + i
    if (winnerDeck == 1) {
      score += deck1[idx] * (size - i)
    } else {
      score += deck2[idx] * (size - i)
    }
  }

  printf("%d", score)
  exit
}