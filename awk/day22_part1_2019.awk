#!/usr/bin/awk -f
function dealIntoNewStack() {
  for (i = 0; i < int(Size/2); i++) {
    t = deck[i]
    deck[i] = deck[Size-1 - i]
    deck[Size-1 - i] = t
  }
}
function cutN(n) {
  if (n >= 0) {
    for (i = 0; i < Size - n; i++) newDeck[i] = deck[i + n]
    for (i = 0; i < n; i++) newDeck[Size - n + i] = deck[i]
  } else {
    k = -n
    for (i = 0; i < k; i++) newDeck[i] = deck[Size - k + i]
    for (i = 0; i < Size - k; i++) newDeck[k + i] = deck[i]
  }
  for (i = 0; i < Size; i++) deck[i] = newDeck[i]
}
function dealWithIncrement(n) {
  for (i = 0; i < Size; i++) {
    pos = (i * n) % Size
    newDeck[pos] = deck[i]
  }
  for (i = 0; i < Size; i++) deck[i] = newDeck[i]
}
function find2019() {
  for (i = 0; i < Size; i++) if (deck[i] == 2019) return i
  return -1
}
BEGIN {
  Size = 10007
  for (i = 0; i < Size; i++) deck[i] = i
  while ((getline line < "input.txt") > 0) {
    if (line == "deal into new stack") {
      dealIntoNewStack()
    } else if (substr(line, 1, 3) == "cut") {
      n = substr(line, 5) + 0
      cutN(n)
    } else if (substr(line, 1, 19) == "deal with increment") {
      n = substr(line, 20) + 0
      dealWithIncrement(n)
    }
  }
  print find2019()
  exit
}