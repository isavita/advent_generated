
function solve()
    global minMana;
    minMana = inf;

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening input.txt');
    end

    line1 = fgetl(fid);
    line2 = fgetl(fid);
    fclose(fid);

    bossHP = sscanf(line1, 'Hit Points: %d');
    bossDamage = sscanf(line2, 'Damage: %d');

    initialState.playerHP = 50;
    initialState.playerMana = 500;
    initialState.bossHP = bossHP;
    initialState.bossDamage = bossDamage;
    initialState.shieldTimer = 0;
    initialState.poisonTimer = 0;
    initialState.rechargeTimer = 0;
    initialState.manaSpent = 0;

    simulate(initialState, true);

    fprintf('%d\n', minMana);
end

function simulate(state, playerTurn)
    global minMana;

    if state.manaSpent >= minMana
        return;
    end

    if state.bossHP <= 0
        minMana = state.manaSpent;
        return;
    end

    if state.playerHP <= 0
        return;
    end

    if playerTurn
        state.playerHP = state.playerHP - 1;
        if state.playerHP <= 0
            return;
        end
    end

    if state.shieldTimer > 0
        state.shieldTimer = state.shieldTimer - 1;
    end
    if state.poisonTimer > 0
        state.bossHP = state.bossHP - 3;
        state.poisonTimer = state.poisonTimer - 1;
    end
    if state.rechargeTimer > 0
        state.playerMana = state.playerMana + 101;
        state.rechargeTimer = state.rechargeTimer - 1;
    end

    if state.bossHP <= 0
        minMana = state.manaSpent;
        return;
    end

    if ~playerTurn
        damage = state.bossDamage;
        if state.shieldTimer > 0
            damage = damage - 7;
        end
        if damage < 1
            damage = 1;
        end
        state.playerHP = state.playerHP - damage;
        simulate(state, true);
        return;
    end

    if state.playerMana >= 53
        newState = state;
        newState.playerMana = newState.playerMana - 53;
        newState.manaSpent = newState.manaSpent + 53;
        newState.bossHP = newState.bossHP - 4;
        simulate(newState, false);
    end

    if state.playerMana >= 73
        newState = state;
        newState.playerMana = newState.playerMana - 73;
        newState.manaSpent = newState.manaSpent + 73;
        newState.bossHP = newState.bossHP - 2;
        newState.playerHP = newState.playerHP + 2;
        simulate(newState, false);
    end

    if state.playerMana >= 113 && state.shieldTimer == 0
        newState = state;
        newState.playerMana = newState.playerMana - 113;
        newState.manaSpent = newState.manaSpent + 113;
        newState.shieldTimer = 6;
        simulate(newState, false);
    end

    if state.playerMana >= 173 && state.poisonTimer == 0
        newState = state;
        newState.playerMana = newState.playerMana - 173;
        newState.manaSpent = newState.manaSpent + 173;
        newState.poisonTimer = 6;
        simulate(newState, false);
    end

    if state.playerMana >= 229 && state.rechargeTimer == 0
        newState = state;
        newState.playerMana = newState.playerMana - 229;
        newState.manaSpent = newState.manaSpent + 229;
        newState.rechargeTimer = 5;
        simulate(newState, false);
    end
end
