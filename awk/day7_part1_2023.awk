#!/usr/bin/awk -f
function card_value(c,    v){
  if (c ~ /[0-9]/) v = c + 0
  else if (c == "T") v = 10
  else if (c == "J") v = 11
  else if (c == "Q") v = 12
  else if (c == "K") v = 13
  else if (c == "A") v = 14
  else v = 0
  return v
}
function hand_type(cards,    i, ch, val, counts){
  for (i = 2; i <= 14; i++) counts[i] = 0
  for (i = 1; i <= 5; i++){
    ch = substr(cards, i, 1)
    val = card_value(ch)
    counts[val]++
  }
  pairs = 0; three = 0; four = 0; five = 0
  for (i = 2; i <= 14; i++){
    if (counts[i] == 2) pairs++
    else if (counts[i] == 3) three++
    else if (counts[i] == 4) four++
    else if (counts[i] == 5) five++
  }
  if (five)  return 7
  if (four)  return 6
  if (three && pairs) return 5
  if (three) return 4
  if (pairs == 2) return 3
  if (pairs) return 2
  return 1
}
function calculate_rank(cards,    rank, i, c){
  rank = 0
  for (i = 1; i <= 5; i++){
    c = substr(cards, i, 1)
    rank = rank * 16
    if (c ~ /[0-9]/) rank += c + 0
    else if (c == "T") rank += 10
    else if (c == "J") rank += 11
    else if (c == "Q") rank += 12
    else if (c == "K") rank += 13
    else if (c == "A") rank += 14
  }
  return rank
}
function read_input(   line, parts, n, i, cards, bid){
  i = 0
  while ((getline line < "input.txt") > 0){
    gsub(/\r/, "", line)
    if (length(line) <= 1) continue
    n = split(line, parts, /[ \t]+/)
    if (n < 2) continue
    cards = parts[1]
    bid = parts[2] + 0
    i++
    hand_cards[i] = cards
    hand_bid[i] = bid
  }
  num_hands = i
}
BEGIN{
  read_input()
  for (i = 1; i <= num_hands; i++){
    t = hand_type(hand_cards[i])
    rank = calculate_rank(hand_cards[i])
    cnt = (match_counts[t] ? match_counts[t] : 0) + 1
    match_counts[t] = cnt
    matches_rank[t, cnt] = rank
    matches_cards[t, cnt] = hand_cards[i]
    matches_bid[t, cnt] = hand_bid[i]
  }
  total_ranked = 0
  for (t = 7; t >= 1; t--){
    m = match_counts[t]
    if (m > 1){
      for (a = 1; a <= m; a++){
        for (b = a+1; b <= m; b++){
          if (matches_rank[t, a] < matches_rank[t, b]){
            tmp = matches_rank[t, a]; matches_rank[t, a] = matches_rank[t, b]; matches_rank[t, b] = tmp
            tmpc = matches_cards[t, a]; matches_cards[t, a] = matches_cards[t, b]; matches_cards[t, b] = tmpc
            tmpb = matches_bid[t, a]; matches_bid[t, a] = matches_bid[t, b]; matches_bid[t, b] = tmpb
          }
        }
      }
    }
    for (i2 = 1; i2 <= m; i2++){
      total_ranked++
      ranked_cards[total_ranked] = matches_cards[t, i2]
      ranked_bid[total_ranked] = matches_bid[t, i2]
    }
  }
  N = total_ranked
  total = 0
  for (i = 1; i <= N; i++){
    total += ranked_bid[i] * (N - i + 1)
  }
  print total
  exit
}