
#!/bin/bash

# main entry point
main() {
    # Check if input file exists
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    # Process hands, calculate type and sort key, sort, and calculate total score
    awk '
    # Map card characters to values for Joker tie-breaking and sorting key generation
    BEGIN {
        # Values for Joker tie-breaking (J=1 is low, A=13 high)
        card_val["J"] = 1; card_val["2"] = 2; card_val["3"] = 3; card_val["4"] = 4; card_val["5"] = 5;
        card_val["6"] = 6; card_val["7"] = 7; card_val["8"] = 8; card_val["9"] = 9; card_val["T"] = 10;
        card_val["Q"] = 11; card_val["K"] = 12; card_val["A"] = 13;

        # Values for generating sortable key (J=1 low, A=E high) - hex-like order
        sort_val["J"] = "1"; sort_val["2"] = "2"; sort_val["3"] = "3"; sort_val["4"] = "4"; sort_val["5"] = "5";
        sort_val["6"] = "6"; sort_val["7"] = "7"; sort_val["8"] = "8"; sort_val["9"] = "9"; sort_val["T"] = "A";
        sort_val["Q"] = "C"; sort_val["K"] = "D"; sort_val["A"] = "E";
    }

    # Process each line (hand and bid)
    {
        cards = $1
        bid = $2
        delete counts # Reset counts for each hand
        joker_count = 0
        sort_key = ""

        # Count card frequencies and build sort key simultaneously
        n = length(cards)
        for (i = 1; i <= n; i++) {
            card = substr(cards, i, 1)
            counts[card]++
            if (card == "J") {
                joker_count++
            }
            sort_key = sort_key sort_val[card] # Build sort key like E1C11
        }

        # Apply Joker rule: add joker count to the best non-joker card
        if (joker_count > 0 && joker_count < 5) { # No change if all Jokers or no Jokers
            max_count = 0
            max_card_val = 0
            best_card = ""
            for (card in counts) {
                if (card == "J") continue
                current_count = counts[card]
                current_val = card_val[card]
                if (current_count > max_count || (current_count == max_count && current_val > max_card_val)) {
                    max_count = current_count
                    max_card_val = current_val
                    best_card = card
                }
            }
            # If there were non-Joker cards, add Jokers count to the best one
            if (best_card != "") {
                 counts[best_card] += joker_count
                 delete counts["J"]
            }
             # If only Jokers existed initially, counts["J"] remains 5
        }

        # Determine hand type based on counts after Joker rule
        # Type scores: 6=FiveKind, 5=FourKind, 4=FullHouse, 3=ThreeKind, 2=TwoPair, 1=OnePair, 0=HighCard
        num_groups = 0
        counts_product = 1
        max_group_count = 0
        for (card in counts) {
            num_groups++
            count = counts[card]
            counts_product *= count
            if (count > max_group_count) {
                 max_group_count = count
            }
        }

        type_score = 0 # Default High Card
        if (max_group_count == 5) { type_score = 6 } # Five of a Kind
        else if (max_group_count == 4) { type_score = 5 } # Four of a Kind
        else if (max_group_count == 3) {
            if (num_groups == 2) { type_score = 4 } # Full House (3 + 2)
            else { type_score = 3 } # Three of a Kind (3 + 1 + 1)
        } else if (max_group_count == 2) {
            if (num_groups == 3) { type_score = 2 } # Two Pair (2 + 2 + 1)
            else { type_score = 1 } # One Pair (2 + 1 + 1 + 1)
        }
        # High Card (1+1+1+1+1) remains 0

        # Print type score, sort key, and bid for sorting
        print type_score, sort_key, bid
    }
    ' input.txt | \
    # Sort: 1st key (type) numeric descending, 2nd key (card values) string descending
    sort -k1,1nr -k2,2r | \
    # Calculate final score
    awk '
    {
        # Store bids in order (strongest first)
        bids[NR] = $3
    }
    END {
        total = 0
        # Rank goes from N down to 1 (N = NR)
        for (i = 1; i <= NR; i++) {
            rank = NR - i + 1
            total += bids[i] * rank
        }
        print total
    }'
}

# Execute the main function
main
