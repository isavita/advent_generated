
{
  for (i = 1; i <= length($0); i++) {
    charCount[substr($0, i, 1)]++
  }

  for (char in charCount) {
    if (charCount[char] == 2) {
      hasTwos = 1
    } else if (charCount[char] == 3) {
      hasThrees = 1
    }
  }

  delete charCount
  if (hasTwos) {
    twoCount++
  }
  if (hasThrees) {
    threeCount++
  }

  hasTwos = 0
  hasThrees = 0
}

END {
  checksum = twoCount * threeCount
  print checksum
}

