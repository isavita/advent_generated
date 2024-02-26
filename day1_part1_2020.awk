
BEGIN {
  target = 2020
}

{
  numbers[$0] = 1
}

END {
  for (i in numbers) {
    if (numbers[target - i]) {
      print i, target - i, i * (target - i)
      exit
    }
  }
}
