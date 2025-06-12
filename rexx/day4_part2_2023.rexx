
/* Rexx */
main:
  inputFile = 'input.txt'
  num_cards = 0
  cards. = 0

  do i = 1 while lines(inputFile) > 0
    num_cards = i
    line = linein(inputFile)
    parse var line . ':' winning_str '|' given_str
    cards.i.wins = space(winning_str, 1)
    cards.i.haves = space(given_str, 1)
    cards.i.count = 1
  end
  call stream inputFile, 'C', 'CLOSE'

  do i = 1 to num_cards
    matches = 0
    haves_str = cards.i.haves
    wins_str = cards.i.wins
    do k = 1 to words(haves_str)
      num = word(haves_str, k)
      if wordpos(num, wins_str) > 0 then
        matches = matches + 1
    end

    if matches > 0 then do
      do j = 1 to matches
        next_idx = i + j
        if next_idx <= num_cards then
          cards.next_idx.count = cards.next_idx.count + cards.i.count
      end
    end
  end

  total_cards = 0
  do i = 1 to num_cards
    total_cards = total_cards + cards.i.count
  end

  say total_cards
return
