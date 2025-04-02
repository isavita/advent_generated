
#!/usr/bin/awk -f

# --- Day 22: Wizard Simulator 20XX ---
# AWK implementation to find the minimum mana cost to win the fight.

# --- Global Variables ---
BEGIN {
    # Boss stats (will be read from input.txt)
    boss_hp = 0
    boss_damage = 0

    # Player initial stats
    player_start_hp = 50
    player_start_mana = 500

    # Minimum mana spent found so far (initialized to a large value)
    min_mana_spent = 999999

    # Spell data: Cost, Damage, Heal, Effect Duration, Effect Name
    # Use associative arrays for easy lookup by spell name
    spell_cost["MM"] = 53
    spell_damage["MM"] = 4
    spell_heal["MM"] = 0
    spell_duration["MM"] = 0

    spell_cost["D"] = 73
    spell_damage["D"] = 2
    spell_heal["D"] = 2
    spell_duration["D"] = 0

    spell_cost["S"] = 113
    spell_damage["S"] = 0
    spell_heal["S"] = 0
    spell_duration["S"] = 6
    spell_effect["S"] = "Shield"

    spell_cost["P"] = 173
    spell_damage["P"] = 0 # Effect does damage
    spell_heal["P"] = 0
    spell_duration["P"] = 6
    spell_effect["P"] = "Poison"

    spell_cost["R"] = 229
    spell_damage["R"] = 0
    spell_heal["R"] = 0
    spell_duration["R"] = 5
    spell_effect["R"] = "Recharge"

    # Ordered list of spells for iteration
    num_spells = 5
    spells[0] = "MM"
    spells[1] = "D"
    spells[2] = "S"
    spells[3] = "P"
    spells[4] = "R"

    # Read boss stats from input.txt
    if ((getline line < "input.txt") > 0) {
        split(line, parts, ": ");
        boss_hp = parts[2] + 0 # Ensure numeric
    } else {
        print "Error: Could not read boss HP from input.txt" > "/dev/stderr"
        exit 1
    }
    if ((getline line < "input.txt") > 0) {
        split(line, parts, ": ");
        boss_damage = parts[2] + 0 # Ensure numeric
    } else {
        print "Error: Could not read boss Damage from input.txt" > "/dev/stderr"
        exit 1
    }
    close("input.txt")

    # Start the simulation from the main entry point
    main()
}

# --- Helper Functions ---
function min(a, b) {
    return (a < b) ? a : b
}

function max(a, b) {
    return (a > b) ? a : b
}

# --- Core Simulation Function (Recursive Depth-First Search) ---
# Parameters represent the state of the game at the start of a turn.
# is_player_turn: 1 if player's turn, 0 if boss's turn
function simulate_turn(p_hp, p_mana, b_hp, \
                       s_timer, p_timer, r_timer, \
                       mana_spent, is_player_turn, \
                       # Local variables for calculations within the function
                       p_armor, current_boss_damage, spell_idx, spell_name, cost, \
                       next_p_hp, next_p_mana, next_b_hp, \
                       next_s_timer, next_p_timer, next_r_timer, next_mana_spent)
{
    # Pruning: If we've already spent more mana than the current best, stop exploring this path.
    if (mana_spent >= min_mana_spent) {
        return
    }

    # --- Apply Effects (Start of turn) ---
    p_armor = 0
    if (s_timer > 0) {
        p_armor = 7
    }
    if (p_timer > 0) {
        b_hp -= 3
    }
    if (r_timer > 0) {
        p_mana += 101
    }

    # --- Check Win Condition (Boss defeated by effects) ---
    if (b_hp <= 0) {
        min_mana_spent = min(min_mana_spent, mana_spent)
        # print "DEBUG: Win found (effect)! Mana spent:", mana_spent # Optional Debug
        return
    }

    # --- Decrement Effect Timers ---
    # (Do this *after* applying effects for the current turn)
    if (s_timer > 0) s_timer--
    if (p_timer > 0) p_timer--
    if (r_timer > 0) r_timer--

    # --- Player's Turn ---
    if (is_player_turn) {
        # Iterate through possible spells to cast
        for (spell_idx = 0; spell_idx < num_spells; spell_idx++) {
            spell_name = spells[spell_idx]
            cost = spell_cost[spell_name]

            # Check if player can afford the spell
            if (p_mana >= cost) {
                # Check if the spell's effect is already active (if applicable)
                can_cast = 1
                if (spell_effect[spell_name] == "Shield" && s_timer > 0) can_cast = 0
                if (spell_effect[spell_name] == "Poison" && p_timer > 0) can_cast = 0
                if (spell_effect[spell_name] == "Recharge" && r_timer > 0) can_cast = 0

                if (can_cast) {
                    # Calculate the state *after* casting the spell
                    next_p_hp = p_hp
                    next_p_mana = p_mana - cost
                    next_b_hp = b_hp
                    next_s_timer = s_timer
                    next_p_timer = p_timer
                    next_r_timer = r_timer
                    next_mana_spent = mana_spent + cost

                    # Apply instant spell effects
                    next_b_hp -= spell_damage[spell_name]
                    next_p_hp += spell_heal[spell_name]

                    # Activate effect timers if applicable
                    if (spell_effect[spell_name] == "Shield") next_s_timer = spell_duration[spell_name]
                    if (spell_effect[spell_name] == "Poison") next_p_timer = spell_duration[spell_name]
                    if (spell_effect[spell_name] == "Recharge") next_r_timer = spell_duration[spell_name]

                    # Check for immediate win after casting
                    if (next_b_hp <= 0) {
                        min_mana_spent = min(min_mana_spent, next_mana_spent)
                        # print "DEBUG: Win found (cast)! Mana spent:", next_mana_spent # Optional Debug
                        # Continue checking other spells, don't return yet,
                        # as another spell might lead to a win later with less total cost
                        # on this turn's possibilities.
                        # But since we are looking for MINIMUM cost, and this path ends in a win,
                        # we update min_mana_spent and don't need to simulate the boss turn for THIS spell cast.
                        # We DO need to continue the loop to check OTHER spells the player could have cast this turn.
                         continue # Go to the next spell in the loop
                    }

                    # If the player didn't win immediately, simulate the boss's turn
                    simulate_turn(next_p_hp, next_p_mana, next_b_hp, \
                                  next_s_timer, next_p_timer, next_r_timer, \
                                  next_mana_spent, 0) # 0 indicates boss's turn next
                }
            }
        }
        # If the loop finishes without finding any affordable+castable spell,
        # the player implicitly loses on this path. The function simply returns.
    }
    # --- Boss's Turn ---
    else {
        # Calculate boss attack damage (min 1 damage)
        current_boss_damage = max(1, boss_damage - p_armor)
        next_p_hp = p_hp - current_boss_damage

        # Check if player survives
        if (next_p_hp <= 0) {
            # Player lost on this path, do nothing (don't update min_mana_spent)
            # print "DEBUG: Player lost! HP:", next_p_hp # Optional Debug
            return
        }

        # Player survived, simulate the player's next turn
        simulate_turn(next_p_hp, p_mana, b_hp, \
                      s_timer, p_timer, r_timer, \
                      mana_spent, 1) # 1 indicates player's turn next
    }
}

# --- Main Entry Point ---
function main() {
    # Start the simulation from the initial state, player's turn first
    # Initial state: player_hp, player_mana, boss_hp, s_timer, p_timer, r_timer, mana_spent, is_player_turn
    simulate_turn(player_start_hp, player_start_mana, boss_hp, \
                  0, 0, 0, \
                  0, 1) # Player turn starts (is_player_turn = 1)
}

# --- END Block ---
# This block executes after all input processing (or after BEGIN if no input file is processed by the main loop)
END {
    # Print the final result
    print min_mana_spent
}
