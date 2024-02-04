
deck = list(range(10007))

with open('input.txt') as f:
    for line in f:
        line = line.strip()
        if line == 'deal into new stack':
            deck.reverse()
        elif line.startswith('cut'):
            n = int(line.split()[-1])
            deck = deck[n:] + deck[:n]
        elif line.startswith('deal with increment'):
            n = int(line.split()[-1])
            new_deck = [-1] * len(deck)
            pos = 0
            for card in deck:
                new_deck[pos] = card
                pos = (pos + n) % len(deck)
            deck = new_deck

print(deck.index(2019))
