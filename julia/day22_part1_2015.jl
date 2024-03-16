mutable struct GameState
    playerHP::Int
    playerMana::Int
    bossHP::Int
    bossDamage::Int
    shieldTimer::Int
    poisonTimer::Int
    rechargeTimer::Int
    manaSpent::Int
end

function min_mana_to_win(initial_state::GameState)
    min_mana = typemax(Int)
    function simulate(state::GameState, player_turn::Bool)
        if state.manaSpent >= min_mana
            return
        end
        if state.bossHP <= 0
            min_mana = state.manaSpent
            return
        end
        if state.playerHP <= 0
            return
        end

        # Apply effects
        if state.shieldTimer > 0
            state.shieldTimer -= 1
        end
        if state.poisonTimer > 0
            state.bossHP -= 3
            state.poisonTimer -= 1
        end
        if state.rechargeTimer > 0
            state.playerMana += 101
            state.rechargeTimer -= 1
        end

        if !player_turn
            damage = state.bossDamage
            if state.shieldTimer > 0
                damage -= 7
            end
            if damage < 1
                damage = 1
            end
            state.playerHP -= damage
            simulate(state, true)
            return
        end

        if state.playerMana >= 53
            new_state = deepcopy(state)
            new_state.playerMana -= 53
            new_state.manaSpent += 53
            new_state.bossHP -= 4
            simulate(new_state, false)
        end
        if state.playerMana >= 73
            new_state = deepcopy(state)
            new_state.playerMana -= 73
            new_state.manaSpent += 73
            new_state.bossHP -= 2
            new_state.playerHP += 2
            simulate(new_state, false)
        end
        if state.playerMana >= 113 && state.shieldTimer == 0
            new_state = deepcopy(state)
            new_state.playerMana -= 113
            new_state.manaSpent += 113
            new_state.shieldTimer = 6
            simulate(new_state, false)
        end
        if state.playerMana >= 173 && state.poisonTimer == 0
            new_state = deepcopy(state)
            new_state.playerMana -= 173
            new_state.manaSpent += 173
            new_state.poisonTimer = 6
            simulate(new_state, false)
        end
        if state.playerMana >= 229 && state.rechargeTimer == 0
            new_state = deepcopy(state)
            new_state.playerMana -= 229
            new_state.manaSpent += 229
            new_state.rechargeTimer = 5
            simulate(new_state, false)
        end
    end

    initial_state.playerHP = 50
    initial_state.playerMana = 500
    simulate(initial_state, true)
    return min_mana
end

# Read input from file
input_file = "input.txt"
lines = readlines(input_file)
boss_hp = parse(Int, split(lines[1], ": ")[2])
boss_damage = parse(Int, split(lines[2], ": ")[2])

initial_state = GameState(0, 0, boss_hp, boss_damage, 0, 0, 0, 0)
println(min_mana_to_win(initial_state))