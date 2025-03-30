
import strutils, tables, algorithm, strscans, math

const valueDict: Table[char, int] = {'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13}.toTable

type
  Hand = object
    cards: string
    bid: int

proc main() =
  let inputData = readFile("input.txt")
  let lines = inputData.strip.splitLines

  var hands: seq[Hand] = @[]

  for line in lines:
    if line.len == 0:
      continue

    var cards: string
    var bid: int
    if scanf(line, "$+ $i", cards, bid):
      hands.add(Hand(cards: cards, bid: bid))

  var matches: array[7, seq[Hand]] # 0: Five of a kind ... 6: High card

  for hand in hands:
    var count = hand.cards.toCountTable

    if count.hasKey('J') and count['J'] > 0 and count.len > 1: # Handle Joker rule only if J exists and isn't the only card type
      let jCount = count['J']
      count.del('J') # Temporarily remove J to find the best card to boost

      var highVal = 0
      var highKey = 'J' # Default if only Js were present (covered by count.len > 1 check)

      for key, val in count:
          if val > highVal:
              highVal = val
              highKey = key
          elif val == highVal and valueDict[key] > valueDict[highKey]:
              highKey = key

      # If no other card was found (e.g. hand was JJJJJ), highKey remains 'J',
      # but count is now empty. Add J back. If other cards exist, boost the best one.
      if highKey != 'J':
          count.inc(highKey, jCount)
      else: # Original hand was all Js
          count['J'] = 5 # Put it back correctly


    var value = 1
    for c in count.values:
      value *= c

    # Determine hand type based on product and number of distinct cards
    # Mapping matches Python logic: 0=Five, 1=Four, 2=FullH, 3=Three, 4=TwoP, 5=OneP, 6=HighC
    case value:
      of 5: # Five of a kind (e.g., AAAAA -> count={'A': 5}, product=5)
        matches[0].add(hand)
      of 4: # Four of a kind or Two pair
        if count.len == 2: # Four of a kind (e.g., AAAAK -> count={'A': 4, 'K': 1}, product=4)
          matches[1].add(hand)
        else: # Two pair (e.g., AAKKQ -> count={'A': 2, 'K': 2, 'Q': 1}, product=4)
          matches[4].add(hand)
      of 6: # Full House (e.g., AAAKK -> count={'A': 3, 'K': 2}, product=6)
        matches[2].add(hand)
      of 3: # Three of a kind (e.g., AAAKQ -> count={'A': 3, 'K': 1, 'Q': 1}, product=3)
        matches[3].add(hand)
      of 2: # One Pair (e.g., AAKQT -> count={'A': 2, 'K': 1, 'Q': 1, 'T': 1}, product=2)
        matches[5].add(hand)
      of 1: # High Card (e.g., AKQJT -> count={'A':1..}, product=1)
        matches[6].add(hand)
      else:
        echo "Error: Unexpected hand value product: ", value

  var convertedMatches: seq[(int, int)] = @[]

  # Iterate from weakest type (High Card) to strongest (Five of a Kind)
  # This matches the Python logic's effective order after extend
  for i in countdown(matches.high, 0):
    var temp: seq[(int, int)] = @[]
    for hand in matches[i]:
      # Create sortable hex value: J is lowest (1), A is highest (E)
      let sortableCards = hand.cards.replace("A", "E").replace("T", "A").replace("J", "1").replace("Q", "C").replace("K", "D")
      let sortVal = parseHexInt(sortableCards)
      temp.add((sortVal, hand.bid))

    # Sort within the type by card strength (ascending)
    temp.sort()
    convertedMatches &= temp # Append sorted hands of this type

  var total: int64 = 0
  # Calculate total winnings: rank * bid
  # convertedMatches is now sorted weakest (rank 1) to strongest (rank N)
  for i in 0..convertedMatches.high:
    let rank = i + 1
    total += convertedMatches[i][1].int64 * rank.int64

  echo total

when isMainModule:
  main()
