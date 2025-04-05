
#!/bin/bash

MIN_MANA=999999999 # Initialize with a large value

# Read input from input.txt
boss_hp=$(grep 'Hit Points:' input.txt | cut -d ' ' -f 3)
boss_damage=$(grep 'Damage:' input.txt | cut -d ' ' -f 2)

# State variables are passed as arguments:
# player_hp, player_mana, boss_hp, shield_timer, poison_timer, recharge_timer, mana_spent, player_turn (1=player, 0=boss)

simulate() {
    local p_hp=$1
    local p_mana=$2
    local b_hp=$3
    local s_timer=$4
    local po_timer=$5
    local r_timer=$6
    local m_spent=$7
    local p_turn=$8

    # --- Pruning ---
    if (( m_spent >= MIN_MANA )); then
        return
    fi

    # --- Apply effects (at start of *both* player and boss turns) ---
    local current_shield_active=0
    if (( s_timer > 0 )); then
        current_shield_active=1
        (( s_timer-- ))
    fi

    if (( po_timer > 0 )); then
        (( b_hp -= 3 ))
        (( po_timer-- ))
    fi

    if (( r_timer > 0 )); then
        (( p_mana += 101 ))
        (( r_timer-- ))
    fi

    # --- Win/Loss Checks ---
    if (( b_hp <= 0 )); then
        if (( m_spent < MIN_MANA )); then
            MIN_MANA=$m_spent
        fi
        return
    fi

    # --- Player's Turn ---
    if (( p_turn == 1 )); then

        # Check loss condition (e.g. if poison killed boss already handled, but player might die)
         if (( p_hp <= 0 )); then
             return
         fi

        # Try Magic Missile (Cost: 53)
        if (( p_mana >= 53 )); then
            simulate "$p_hp" "$((p_mana - 53))" "$((b_hp - 4))" "$s_timer" "$po_timer" "$r_timer" "$((m_spent + 53))" 0
        fi

        # Try Drain (Cost: 73)
        if (( p_mana >= 73 )); then
            simulate "$((p_hp + 2))" "$((p_mana - 73))" "$((b_hp - 2))" "$s_timer" "$po_timer" "$r_timer" "$((m_spent + 73))" 0
        fi

        # Try Shield (Cost: 113, Duration: 6) - Can only cast if not already active
        if (( p_mana >= 113 && s_timer == 0 )); then
            simulate "$p_hp" "$((p_mana - 113))" "$b_hp" 6 "$po_timer" "$r_timer" "$((m_spent + 113))" 0
        fi

        # Try Poison (Cost: 173, Duration: 6) - Can only cast if not already active
        if (( p_mana >= 173 && po_timer == 0 )); then
            simulate "$p_hp" "$((p_mana - 173))" "$b_hp" "$s_timer" 6 "$r_timer" "$((m_spent + 173))" 0
        fi

        # Try Recharge (Cost: 229, Duration: 5) - Can only cast if not already active
        if (( p_mana >= 229 && r_timer == 0 )); then
            simulate "$p_hp" "$((p_mana - 229))" "$b_hp" "$s_timer" "$po_timer" 5 "$((m_spent + 229))" 0
        fi

    # --- Boss's Turn ---
    else
        local damage_dealt=$boss_damage
        if (( current_shield_active == 1 )); then
            (( damage_dealt -= 7 ))
        fi
        if (( damage_dealt < 1 )); then
            damage_dealt=1
        fi
        (( p_hp -= damage_dealt ))

        # Check loss condition
        if (( p_hp <= 0 )); then
            return
        fi

        # Next turn is player's turn
        simulate "$p_hp" "$p_mana" "$b_hp" "$s_timer" "$po_timer" "$r_timer" "$m_spent" 1
    fi
}


main() {
    # Initial state
    local initial_player_hp=50
    local initial_player_mana=500
    local initial_boss_hp=$boss_hp
    local initial_shield_timer=0
    local initial_poison_timer=0
    local initial_recharge_timer=0
    local initial_mana_spent=0

    # Start simulation with player's turn (1)
    simulate "$initial_player_hp" "$initial_player_mana" "$initial_boss_hp" \
             "$initial_shield_timer" "$initial_poison_timer" "$initial_recharge_timer" \
             "$initial_mana_spent" 1

    echo "$MIN_MANA"
}

# Run the main function
main
