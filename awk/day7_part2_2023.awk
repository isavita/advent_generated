#!/usr/bin/awk -f
BEGIN {
  # card values (non-joker)
  card_values["J"]=1
  card_values["2"]=2
  card_values["3"]=3
  card_values["4"]=4
  card_values["5"]=5
  card_values["6"]=6
  card_values["7"]=7
  card_values["8"]=8
  card_values["9"]=9
  card_values["T"]=10
  card_values["Q"]=11
  card_values["K"]=12
  card_values["A"]=13

  # hex mapping
  hex_map["A"]="E"
  hex_map["T"]="A"
  hex_map["J"]="1"
  hex_map["Q"]="C"
  hex_map["K"]="D"

  N = 0

  # read input from input.txt
  while ((getline line < "input.txt") > 0) {
    gsub(/\r$/, "", line)
    if (line ~ /^[ \t]*$/) continue
    split(line, parts, /[ \t]+/)
    cards = parts[1]
    bid = (parts[2] + 0)

    # compute hex string
    hex_str = ""
    for (i = 1; i <= length(cards); i++) {
      ch = substr(cards, i, 1)
      if (ch in hex_map) hex_str = hex_str hex_map[ch]
      else hex_str = hex_str ch
    }

    # convert hex string to decimal without strtonum for portability
    hex_val = 0
    if (hex_str != "") {
      for (i = 1; i <= length(hex_str); i++) {
        d = 0
        ch = substr(hex_str, i, 1)
        if (ch == "0") d = 0
        else if (ch == "1") d = 1
        else if (ch == "2") d = 2
        else if (ch == "3") d = 3
        else if (ch == "4") d = 4
        else if (ch == "5") d = 5
        else if (ch == "6") d = 6
        else if (ch == "7") d = 7
        else if (ch == "8") d = 8
        else if (ch == "9") d = 9
        else if (ch == "A") d = 10
        else if (ch == "B") d = 11
        else if (ch == "C") d = 12
        else if (ch == "D") d = 13
        else if (ch == "E") d = 14
        else if (ch == "F") d = 15
        hex_val = hex_val * 16 + d
      }
    }

    type = classify(cards)
    N++
    id = N
    hex_arr[id] = hex_val
    type_arr[id] = type
    bid_arr[id] = bid
  }
  close("input.txt")

  if (N == 0) { print 0; exit }

  for (i = 1; i <= N; i++) idx[i] = i
  merge_sort(1, N)

  total = 0
  for (pos = 1; pos <= N; pos++) {
    id = idx[pos]
    total += bid_arr[id] * (N - pos + 1)
  }
  print total
  exit
}
function hex_to_dec(h,   val, i, ch, d) {
  val = 0
  if (h == "") return 0
  for (i = 1; i <= length(h); i++) {
    ch = substr(h, i, 1)
    d = 0
    if (ch == "0") d = 0
    else if (ch == "1") d = 1
    else if (ch == "2") d = 2
    else if (ch == "3") d = 3
    else if (ch == "4") d = 4
    else if (ch == "5") d = 5
    else if (ch == "6") d = 6
    else if (ch == "7") d = 7
    else if (ch == "8") d = 8
    else if (ch == "9") d = 9
    else if (ch == "A") d = 10
    else if (ch == "B") d = 11
    else if (ch == "C") d = 12
    else if (ch == "D") d = 13
    else if (ch == "E") d = 14
    else if (ch == "F") d = 15
    val = val * 16 + d
  }
  return val
}
function classify(cards,   counts, k, joker_count, empty, high_key, high_v, distinct, value_product) {
  delete counts
  for (k in counts) delete counts[k]
  for (i = 1; i <= length(cards); i++) {
    c = substr(cards, i, 1)
    counts[c]++
  }

  if (("J" in counts) && counts["J"] > 0) {
    joker_count = counts["J"]
    delete counts["J"]
    empty = 1
    for (k in counts) { empty = 0; break }
    if (empty) {
      counts["J"] = joker_count
    } else {
      high_key = ""
      high_v = -1
      for (card in counts) {
        if (counts[card] > high_v) {
          high_key = card
          high_v = counts[card]
        } else if (counts[card] == high_v) {
          if ((card in card_values) && (high_key in card_values)) {
            if (card_values[card] > card_values[high_key]) {
              high_key = card
            }
          }
        }
      }
      counts[high_key] += joker_count
    }
  }

  value_product = 1
  distinct = 0
  for (k in counts) {
    distinct++
    value_product *= counts[k]
  }

  if (value_product == 1 && distinct == 5) return 6
  else if (value_product == 2 && distinct == 4) return 5
  else if (value_product == 3 && distinct == 3) return 3
  else if (value_product == 4) {
    if (distinct == 2) return 1
    else return 4
  } else if (value_product == 5 && distinct == 1) return 0
  else if (value_product == 6 && distinct == 2) return 2
  else return -1
}
function merge_sort(l, r,   m, i, j, k) {
  if (l >= r) return
  m = int((l + r) / 2)
  merge_sort(l, m)
  merge_sort(m + 1, r)
  i = l
  j = m + 1
  k = l
  while (i <= m && j <= r) {
    if (cmp(idx[i], idx[j]) <= 0) { tmp[k] = idx[i]; i++ } else { tmp[k] = idx[j]; j++ }
    k++
  }
  while (i <= m) { tmp[k] = idx[i]; i++; k++ }
  while (j <= r) { tmp[k] = idx[j]; j++; k++ }
  for (p = l; p <= r; p++) idx[p] = tmp[p]
}
function cmp(a, b) {
  if (type_arr[a] != type_arr[b]) return (type_arr[a] < type_arr[b]) ? -1 : 1
  if (hex_arr[a] != hex_arr[b]) return (hex_arr[a] > hex_arr[b]) ? -1 : 1
  return (a < b) ? -1 : (a > b) ? 1 : 0
}