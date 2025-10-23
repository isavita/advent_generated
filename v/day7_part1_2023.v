
import os
import arrays

struct Hand {
    cards string
    bid int
    hand_type int
}

fn card_value(c u8) int {
    return match c {
        `A` { 14 }
        `K` { 13 }
        `Q` { 12 }
        `J` { 11 }
        `T` { 10 }
        else { int(c - `0`) }
    }
}

fn classify_hand(cards string) int {
    mut counts := map[u8]int{}
    for c in cards {
        counts[c]++
    }
    
    mut freq := counts.values()
    freq.sort(a > b)
    
    return match freq {
        [5] { 7 } // Five of a kind
        [4, 1] { 6 } // Four of a kind
        [3, 2] { 5 } // Full house
        [3, 1, 1] { 4 } // Three of a kind
        [2, 2, 1] { 3 } // Two pair
        [2, 1, 1, 1] { 2 } // One pair
        else { 1 } // High card
    }
}

fn compare_hands(a &Hand, b &Hand) int {
    if a.hand_type != b.hand_type {
        return a.hand_type - b.hand_type
    }
    
    for i in 0..5 {
        va := card_value(a.cards[i])
        vb := card_value(b.cards[i])
        if va != vb {
            return va - vb
        }
    }
    return 0
}

fn main() {
    content := os.read_file('input.txt') or { panic('Failed to read input.txt') }
    lines := content.trim_space().split_into_lines()
    
    mut hands := []Hand{}
    
    for line in lines {
        parts := line.split(' ')
        cards := parts[0]
        bid := parts[1].int()
        hands << Hand{cards, bid, classify_hand(cards)}
    }
    
    hands.sort_with_compare(compare_hands)
    
    mut total := 0
    for i, hand in hands {
        total += hand.bid * (i + 1)
    }
    
    println(total)
}
