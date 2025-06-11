
/* REXX */
main:
  numeric digits 20
  totalCups = 1000000
  totalMoves = 10000000

  input = linein('input.txt')
  input_len = length(input)
  cups. = 0
  lastCup = 0

  do i = 1 to input_len
    cup = substr(input, i, 1)
    if i > 1 then
      cups.lastCup = cup
    lastCup = cup
  end

  do i = input_len + 1 to totalCups
    cups.lastCup = i
    lastCup = i
  end

  firstCup = substr(input, 1, 1)
  cups.lastCup = firstCup
  currentCup = firstCup

  do i = 1 to totalMoves
    pickup1 = cups.currentCup
    pickup2 = cups.pickup1
    pickup3 = cups.pickup2

    cups.currentCup = cups.pickup3

    destinationCup = currentCup - 1
    if destinationCup = 0 then
      destinationCup = totalCups

    do while destinationCup = pickup1 | destinationCup = pickup2 | destinationCup = pickup3
      destinationCup = destinationCup - 1
      if destinationCup = 0 then
        destinationCup = totalCups
    end

    cups.pickup3 = cups.destinationCup
    cups.destinationCup = pickup1

    currentCup = cups.currentCup
  end

  cup1 = cups.1
  cup2 = cups.cup1
  say cup1 * cup2
return

call main
