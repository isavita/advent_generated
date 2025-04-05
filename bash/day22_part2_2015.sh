
#!/bin/bash

# --- Global state variables ---
# Player stats
sim_playerHP=0
sim_playerMana=0
sim_manaSpent=0

# Boss stats
sim_bossHP=0
sim_bossDamage=0

# Effect timers
sim_shieldTimer=0
sim_poisonTimer=0
sim_rechargeTimer=0

# Minimum mana found so far
min_mana=999999999

# --- Simulation Function ---
simulate() {
    local playerTurn=$1 # 1 for player's turn, 0 for boss's turn

    # --- Pruning ---
    # If we've already spent more mana than the current best, stop exploring this path
    (( sim_manaSpent >= min_mana )) && return

    # --- Boss Win Condition ---
    if (( sim_bossHP <= 0 )); then
        # Found a way to win. Update min_mana if this path is cheaper.
        (( sim_manaSpent < min_mana )) && min_mana=$sim_manaSpent
        return
    fi

    # --- Player Loss Condition ---
    (( sim_playerHP <= 0 )) && return

    # --- Hard Mode: Player loses 1 HP at the start of their turn ---
     if (( playerTurn == 1 )); then
         (( sim_playerHP-- ))
         # Re-check loss condition after HP reduction
         (( sim_playerHP <= 0 )) && return
     fi

    # --- Apply Effects Phase ---
    local current_armor=0
    if (( sim_shieldTimer > 0 )); then
        (( sim_shieldTimer-- ))
        current_armor=7 # Shield effect grants 7 armor
    fi
    if (( sim_poisonTimer > 0 )); then
        (( sim_bossHP -= 3 )) # Poison deals 3 damage
        (( sim_poisonTimer-- ))
    fi
    if (( sim_rechargeTimer > 0 )); then
        (( sim_playerMana += 101 )) # Recharge gives 101 mana
        (( sim_rechargeTimer-- ))
    fi

    # --- Re-check Boss Win Condition after effects ---
    if (( sim_bossHP <= 0 )); then
        (( sim_manaSpent < min_mana )) && min_mana=$sim_manaSpent
        return
    fi

    # --- Boss's Turn ---
    if (( playerTurn == 0 )); then
        local bossAttack=$sim_bossDamage
        (( bossAttack -= current_armor )) # Apply armor reduction
        (( bossAttack < 1 )) && bossAttack=1 # Boss always deals at least 1 damage
        (( sim_playerHP -= bossAttack ))

        # Simulate player's next turn
        simulate 1
        return # End boss's turn
    fi

    # --- Player's Turn (Try Spells) ---

    # Save current state before trying branches
    local saved_playerHP=$sim_playerHP
    local saved_playerMana=$sim_playerMana
    local saved_bossHP=$sim_bossHP
    local saved_shieldTimer=$sim_shieldTimer
    local saved_poisonTimer=$sim_poisonTimer
    local saved_rechargeTimer=$sim_rechargeTimer
    local saved_manaSpent=$sim_manaSpent

    # Try Magic Missile (Cost: 53)
    if (( sim_playerMana >= 53 )); then
        (( sim_playerMana -= 53 ))
        (( sim_manaSpent += 53 ))
        (( sim_bossHP -= 4 ))
        simulate 0 # Simulate boss's turn
        # Restore state for next spell option
        sim_playerHP=$saved_playerHP
        sim_playerMana=$saved_playerMana
        sim_bossHP=$saved_bossHP
        sim_shieldTimer=$saved_shieldTimer
        sim_poisonTimer=$saved_poisonTimer
        sim_rechargeTimer=$saved_rechargeTimer
        sim_manaSpent=$saved_manaSpent
    fi

    # Try Drain (Cost: 73)
    if (( sim_playerMana >= 73 )); then
        (( sim_playerMana -= 73 ))
        (( sim_manaSpent += 73 ))
        (( sim_bossHP -= 2 ))
        (( sim_playerHP += 2 ))
        simulate 0
        sim_playerHP=$saved_playerHP
        sim_playerMana=$saved_playerMana
        sim_bossHP=$saved_bossHP
        sim_shieldTimer=$saved_shieldTimer
        sim_poisonTimer=$saved_poisonTimer
        sim_rechargeTimer=$saved_rechargeTimer
        sim_manaSpent=$saved_manaSpent
    fi

    # Try Shield (Cost: 113) - Only if Shield is not already active
    if (( sim_playerMana >= 113 && sim_shieldTimer == 0 )); then
        (( sim_playerMana -= 113 ))
        (( sim_manaSpent += 113 ))
        sim_shieldTimer=6 # Shield lasts 6 turns
        simulate 0
        sim_playerHP=$saved_playerHP
        sim_playerMana=$saved_playerMana
        sim_bossHP=$saved_bossHP
        sim_shieldTimer=$saved_shieldTimer
        sim_poisonTimer=$saved_poisonTimer
        sim_rechargeTimer=$saved_rechargeTimer
        sim_manaSpent=$saved_manaSpent
    fi

    # Try Poison (Cost: 173) - Only if Poison is not already active
    if (( sim_playerMana >= 173 && sim_poisonTimer == 0 )); then
        (( sim_playerMana -= 173 ))
        (( sim_manaSpent += 173 ))
        sim_poisonTimer=6 # Poison lasts 6 turns
        simulate 0
        sim_playerHP=$saved_playerHP
        sim_playerMana=$saved_playerMana
        sim_bossHP=$saved_bossHP
        sim_shieldTimer=$saved_shieldTimer
        sim_poisonTimer=$saved_poisonTimer
        sim_rechargeTimer=$saved_rechargeTimer
        sim_manaSpent=$saved_manaSpent
    fi

    # Try Recharge (Cost: 229) - Only if Recharge is not already active
    if (( sim_playerMana >= 229 && sim_rechargeTimer == 0 )); then
        (( sim_playerMana -= 229 ))
        (( sim_manaSpent += 229 ))
        sim_rechargeTimer=5 # Recharge lasts 5 turns
        simulate 0
        sim_playerHP=$saved_playerHP
        sim_playerMana=$saved_playerMana
        sim_bossHP=$saved_bossHP
        sim_shieldTimer=$saved_shieldTimer
        sim_poisonTimer=$saved_poisonTimer
        sim_rechargeTimer=$saved_rechargeTimer
        sim_manaSpent=$saved_manaSpent
    fi
}

# --- Main Entry Point ---
main() {
    # Read input from input.txt
    local hp_line=$(grep "Hit Points:" input.txt)
    local dmg_line=$(grep "Damage:" input.txt)

    # Extract values (using parameter expansion for robustness)
    sim_bossHP=${hp_line##*: }
    sim_bossDamage=${dmg_line##*: }

    # Set initial player stats
    sim_playerHP=50
    sim_playerMana=500
    sim_manaSpent=0
    sim_shieldTimer=0
    sim_poisonTimer=0
    sim_rechargeTimer=0

    # Start the simulation (Player's turn starts)
    simulate 1

    # Print the minimum mana required
    echo "$min_mana"
}

# Run the main function
main
