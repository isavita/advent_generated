
#!/bin/bash

# Function to simulate the fight
# Returns 0 if player wins, 1 if boss wins (player loses)
player_wins() {
    local player_hp=$1
    local player_damage=$2
    local player_armor=$3
    local boss_hp=$4
    local boss_damage=$5
    local boss_armor=$6

    local pd=$(( player_damage - boss_armor ))
    (( pd < 1 )) && pd=1 # max(1, ...)

    local bd=$(( boss_damage - player_armor ))
    (( bd < 1 )) && bd=1 # max(1, ...)

    # Ceiling division: (numerator + denominator - 1) / denominator
    local player_turns=$(( (boss_hp + pd - 1) / pd ))
    local boss_turns=$(( (player_hp + bd - 1) / bd ))

    # Return 0 for true (player wins), 1 for false (player loses)
    if (( player_turns <= boss_turns )); then
        return 0 # Player wins
    else
        return 1 # Player loses
    fi
}

main() {
    # Read boss stats from input.txt
    local boss_hp=$(grep "Hit Points:" input.txt | cut -d':' -f2)
    local boss_damage=$(grep "Damage:" input.txt | cut -d':' -f2)
    local boss_armor=$(grep "Armor:" input.txt | cut -d':' -f2)

    # Define items as "cost;damage;armor"
    local weapons=(
        "8;4;0" "10;5;0" "25;6;0" "40;7;0" "74;8;0"
    )
    local armors=(
        "0;0;0" "13;0;1" "31;0;2" "53;0;3" "75;0;4" "102;0;5" # Include 0-cost armor
    )
    local rings=(
        "0;0;0"  # Represents no ring / first slot empty
        "25;1;0" "50;2;0" "100;3;0" "20;0;1" "40;0;2" "80;0;3"
    )

    local player_hp=100
    local max_cost=0

    local w_idx a_idx ri rj
    local w_cost w_damage w_armor a_cost a_damage a_armor
    local r1_cost r1_damage r1_armor r2_cost r2_damage r2_armor
    local current_cost current_damage current_armor

    # Iterate through all combinations
    for w_item in "${weapons[@]}"; do
        IFS=';' read -r w_cost w_damage w_armor <<< "$w_item"

        for a_item in "${armors[@]}"; do
            IFS=';' read -r a_cost a_damage a_armor <<< "$a_item" # a_damage is always 0

            local num_rings=${#rings[@]}
            # Iterate choosing 2 distinct rings (index ri < rj)
            # This naturally covers 0 rings (both 0;0;0 - handled below)
            # 1 ring (one 0;0;0, one other) and 2 rings.
            # Actually, the loop structure handles selecting two *distinct* items.
            # The 0-cost items ensure combinations represent 0, 1 or 2 pieces of gear.
            for (( ri=0; ri < num_rings; ri++ )); do
                IFS=';' read -r r1_cost r1_damage r1_armor <<< "${rings[$ri]}"

                for (( rj=ri + 1; rj < num_rings; rj++ )); do
                    IFS=';' read -r r2_cost r2_damage r2_armor <<< "${rings[$rj]}"

                    current_cost=$(( w_cost + a_cost + r1_cost + r2_cost ))
                    current_damage=$(( w_damage + r1_damage + r2_damage )) # weapon/rings give damage
                    current_armor=$(( a_armor + r1_armor + r2_armor ))   # armor/rings give armor

                    # Check if player loses
                    if ! player_wins "$player_hp" "$current_damage" "$current_armor" "$boss_hp" "$boss_damage" "$boss_armor"; then
                        # Update max_cost if this losing setup is more expensive
                        if (( current_cost > max_cost )); then
                            max_cost=$current_cost
                        fi
                    fi
                done # rj
            done # ri
        done # a_item
    done # w_item

    echo "$max_cost"
}

# Entry point
main
