
local file = io.open("input.txt", "r")
local bossHP = tonumber(string.match(file:read(), ": (%d+)"))
local bossDamage = tonumber(string.match(file:read(), ": (%d+)"))
file:close()

local function minManaToWin(initialState)
	local minMana = math.huge
	local function simulate(state, playerTurn)
		if state.manaSpent >= minMana then return end
		if state.bossHP <= 0 then
			minMana = state.manaSpent
			return
		end
		if state.playerHP <= 0 then return end

		if playerTurn then
			state.playerHP = state.playerHP - 1
			if state.playerHP <= 0 then return end
		end

		if state.shieldTimer > 0 then state.shieldTimer = state.shieldTimer - 1 end
		if state.poisonTimer > 0 then
			state.bossHP = state.bossHP - 3
			state.poisonTimer = state.poisonTimer - 1
		end
		if state.rechargeTimer > 0 then
			state.playerMana = state.playerMana + 101
			state.rechargeTimer = state.rechargeTimer - 1
		end

		if not playerTurn then
			local damage = state.bossDamage
			if state.shieldTimer > 0 then damage = damage - 7 end
			if damage < 1 then damage = 1 end
			state.playerHP = state.playerHP - damage
			simulate(state, true)
			return
		end

		if state.playerMana >= 53 then
			local newState = {playerHP = state.playerHP, playerMana = state.playerMana - 53, bossHP = state.bossHP - 4, bossDamage = state.bossDamage, shieldTimer = state.shieldTimer, poisonTimer = state.poisonTimer, rechargeTimer = state.rechargeTimer, manaSpent = state.manaSpent + 53}
			simulate(newState, false)
		end
		if state.playerMana >= 73 then
			local newState = {playerHP = state.playerHP + 2, playerMana = state.playerMana - 73, bossHP = state.bossHP - 2, bossDamage = state.bossDamage, shieldTimer = state.shieldTimer, poisonTimer = state.poisonTimer, rechargeTimer = state.rechargeTimer, manaSpent = state.manaSpent + 73}
			simulate(newState, false)
		end
		if state.playerMana >= 113 and state.shieldTimer == 0 then
			local newState = {playerHP = state.playerHP, playerMana = state.playerMana - 113, bossHP = state.bossHP, bossDamage = state.bossDamage, shieldTimer = 6, poisonTimer = state.poisonTimer, rechargeTimer = state.rechargeTimer, manaSpent = state.manaSpent + 113}
			simulate(newState, false)
		end
		if state.playerMana >= 173 and state.poisonTimer == 0 then
			local newState = {playerHP = state.playerHP, playerMana = state.playerMana - 173, bossHP = state.bossHP, bossDamage = state.bossDamage, shieldTimer = state.shieldTimer, poisonTimer = 6, rechargeTimer = state.rechargeTimer, manaSpent = state.manaSpent + 173}
			simulate(newState, false)
		end
		if state.playerMana >= 229 and state.rechargeTimer == 0 then
			local newState = {playerHP = state.playerHP, playerMana = state.playerMana - 229, bossHP = state.bossHP, bossDamage = state.bossDamage, shieldTimer = state.shieldTimer, poisonTimer = state.poisonTimer, rechargeTimer = 5, manaSpent = state.manaSpent + 229}
			simulate(newState, false)
		end
	end

	initialState.playerHP = 50
	initialState.playerMana = 500
	simulate(initialState, true)
	return minMana
end

local initialState = {playerHP = 0, playerMana = 0, bossHP = bossHP, bossDamage = bossDamage, shieldTimer = 0, poisonTimer = 0, rechargeTimer = 0, manaSpent = 0}
print(minManaToWin(initialState))
