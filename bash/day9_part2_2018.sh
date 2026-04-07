
#!/bin/bash

awk '
{
    players = $1
    last_marble = $7 * 100
    
    left[0] = 0
    right[0] = 0
    current = 0
    
    for (m = 1; m <= last_marble; m++) {
        if (m % 23 == 0) {
            p = m % players
            for (i = 0; i < 7; i++) {
                current = left[current]
            }
            scores[p] += m + current
            
            l = left[current]
            r = right[current]
            right[l] = r
            left[r] = l
            current = r
        } else {
            l = right[current]
            r = right[l]
            
            left[m] = l
            right[m] = r
            right[l] = m
            left[r] = m
            current = m
        }
    }
    
    max_score = 0
    for (p in scores) {
        if (scores[p] > max_score) max_score = scores[p]
    }
    printf "%.0f\n", max_score
}' input.txt
