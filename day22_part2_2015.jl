
using DelimitedFiles

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

function minManaToWin(initialState::GameState)
    minMana = typemax(Int)
    simulate!(state::GameState, playerTurn::Bool) = begin
        if state.manaSpent >= minMana
            return
        end
        if state.bossHP <= 0
            minMana = min(minMana, state.manaSpent)
            return
        end
        if state.playerHP <= 0
            return
        end

        if playerTurn
            state.playerHP -= 1
            if state.playerHP <= 0
                return
            end
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

        if !playerTurn
            damage = state.bossDamage
            if state.shieldTimer > 0
                damage -= 7
            end
            if damage < 1
                damage = 1
            end
            state.playerHP -= damage
            simulate!(state, true)
            return
        end

        # Cast spells
        if state.playerMana >= 53
            newState = deepcopy(state)
            newState.playerMana -= 53
            newState.manaSpent += 53
            newState.bossHP -= 4
            simulate!(newState, false)
        end
        if state.playerMana >= 73
            newState = deepcopy(state)
            newState.playerMana -= 73
            newState.manaSpent += 73
            newState.bossHP -= 2
            newState.playerHP += 2
            simulate!(newState, false)
        end
        if state.playerMana >= 113 && state.shieldTimer == 0
            newState = deepcopy(state)
            newState.playerMana -= 113
            newState.manaSpent += 113
            newState.shieldTimer = 6
            simulate!(newState, false)
        end
        if state.playerMana >= 173 && state.poisonTimer == 0
            newState = deepcopy(state)
            newState.playerMana -= 173
            newState.manaSpent += 173
            newState.poisonTimer = 6
            simulate!(newState, false)
        end
        if state.playerMana >= 229 && state.rechargeTimer == 0
            newState = deepcopy(state)
            newState.playerMana -= 229
            newState.manaSpent += 229
            newState.rechargeTimer = 5
            simulate!(newState, false)
        end
    end

    initialState.playerHP = 50
    initialState.playerMana = 500
    simulate!(initialState, true)
    return minMana
end

function main()
    input = readdlm("input.txt", '\n', String)
    bossHP = parse(Int, split(input[1], ": ")[2])
    bossDamage = parse(Int, split(input[2], ": ")[2])
    initialState = GameState(0, 0, bossHP, bossDamage, 0, 0, 0, 0)
    println(minManaToWin(initialState))
end

main()
