
local M = {}

-- Helper function to split a string by a delimiter
local function split(str, sep)
  local parts = {}
  local current_part = ""
  for i = 1, #str do
    local char = str:sub(i, i)
    if char == sep then
      table.insert(parts, current_part)
      current_part = ""
    else
      current_part = current_part .. char
    end
  end
  table.insert(parts, current_part)

  -- Trim spaces from parts
  local trimmed_parts = {}
  for _, part in ipairs(parts) do
      local trimmed = part:match("^%s*(.-)%s*$")
      if trimmed and #trimmed > 0 then
          table.insert(trimmed_parts, trimmed)
      end
  end
  return trimmed_parts
end

-- Helper function to create a set-like table for fast lookups
local function to_set(items)
    local set = {}
    for _, item in ipairs(items) do
        set[item] = true
    end
    return set
end

-- Group 'Class'
local Group = {}
Group.__index = Group

function Group:new(units, hp, ad, at, init, imm, weak, army_id, group_id)
    local o = setmetatable({}, Group)
    o.units = units
    o.hit_points = hp
    o.attack_damage = ad
    o.attack_type = at
    o.initiative = init
    o.immunities = to_set(imm or {})
    o.weaknesses = to_set(weak or {})
    o.target = nil
    o.army_id = army_id
    o.id = group_id -- Unique ID within the simulation for stable sorting/targeting
    return o
end

function Group:effective_power()
    return self.units * self.attack_damage
end

function Group:damage_dealt(enemy)
    if self.units <= 0 then return 0 end -- Optimization: Dead groups deal no damage
    if enemy.immunities[self.attack_type] then
        return 0
    end
    local power = self:effective_power()
    if enemy.weaknesses[self.attack_type] then
        return power * 2
    end
    return power
end

-- Army 'Class'
local Army = {}
Army.__index = Army

function Army:new(id)
    local o = setmetatable({}, Army)
    o.groups = {}
    o.id = id
    return o
end

function Army:add_group(group)
    table.insert(self.groups, group)
end

function Army:alive()
    for _, g in ipairs(self.groups) do
        if g.units > 0 then return true end
    end
    return false
end

function Army:boost(amount)
    for _, g in ipairs(self.groups) do
        g.attack_damage = g.attack_damage + amount
    end
end

function Army:total_units()
    local count = 0
    for _, g in ipairs(self.groups) do
        if g.units > 0 then
            count = count + g.units
        end
    end
    return count
end

-- Battlefield 'Class'
local Battlefield = {}
Battlefield.__index = Battlefield

function Battlefield:new(armies)
    local o = setmetatable({}, Battlefield)
    o.armies = armies -- Expects a table { [army_id] = Army }
    return o
end

function Battlefield:get_all_groups()
    local all_groups = {}
    for _, army in pairs(self.armies) do
        for _, g in ipairs(army.groups) do
             table.insert(all_groups, g)
        end
    end
    return all_groups
end

function Battlefield:find_targets()
    local potential_attackers = {}
    local potential_targets = {}

    for army_id, army in pairs(self.armies) do
        for _, g in ipairs(army.groups) do
            g.target = nil -- Reset target from previous round
            if g.units > 0 then
                table.insert(potential_attackers, g)
                potential_targets[g] = true -- Use group object as key for quick lookup
            end
        end
    end

    -- Sort attackers by effective power, then initiative (descending)
    table.sort(potential_attackers, function(a, b)
        local ep_a = a:effective_power()
        local ep_b = b:effective_power()
        if ep_a ~= ep_b then return ep_a > ep_b end
        return a.initiative > b.initiative
    end)

    for _, attacker in ipairs(potential_attackers) do
        local best_target = nil
        local max_damage = -1 -- Handle 0 damage correctly

        local enemy_army_id = (attacker.army_id == 1) and 2 or 1
        local enemy_army = self.armies[enemy_army_id]

        if enemy_army then
             for _, enemy in ipairs(enemy_army.groups) do
                 if enemy.units > 0 and potential_targets[enemy] then -- Check if alive and not already targeted
                     local damage = attacker:damage_dealt(enemy)

                     if damage > 0 then
                         if damage > max_damage then
                             max_damage = damage
                             best_target = enemy
                         elseif damage == max_damage then
                             local current_best_ep = best_target:effective_power()
                             local enemy_ep = enemy:effective_power()
                             if enemy_ep > current_best_ep then
                                 best_target = enemy
                             elseif enemy_ep == current_best_ep and enemy.initiative > best_target.initiative then
                                 best_target = enemy
                             end
                         end
                     end
                 end
             end
        end

        if best_target then
            attacker.target = best_target
            potential_targets[best_target] = nil -- Mark as targeted for this round
        end
    end
end

function Battlefield:attack_phase()
    local attackers = {}
    for _, army in pairs(self.armies) do
        for _, g in ipairs(army.groups) do
            if g.units > 0 then
                table.insert(attackers, g)
            end
        end
    end

    -- Sort by initiative (descending)
    table.sort(attackers, function(a, b)
        return a.initiative > b.initiative
    end)

    local total_units_killed = 0
    for _, attacker in ipairs(attackers) do
        -- Attacker might have died earlier in the phase
        if attacker.units > 0 and attacker.target then
            local target = attacker.target
            -- Target might have died earlier in the phase
            if target.units > 0 then
                local damage = attacker:damage_dealt(target)
                local units_killed = math.floor(damage / target.hit_points)

                units_killed = math.min(units_killed, target.units) -- Cannot kill more units than exist
                target.units = target.units - units_killed
                total_units_killed = total_units_killed + units_killed
            end
             -- Target reference is implicitly cleared when groups are rebuilt in clean() or re-parsed
             -- No need to explicitly set attacker.target = nil here if clean() is always called after
        end
    end
    return total_units_killed > 0 -- Return true if any attacks happened
end

function Battlefield:clean()
    for army_id, army in pairs(self.armies) do
        local living_groups = {}
        for _, g in ipairs(army.groups) do
            if g.units > 0 then
                table.insert(living_groups, g)
            end
        end
        army.groups = living_groups
    end
end

function Battlefield:active()
    local alive_count = 0
    for _, army in pairs(self.armies) do
        if army:alive() then
            alive_count = alive_count + 1
        end
    end
    return alive_count == 2
end

function Battlefield:result()
    local winner = 0
    local units = 0
    for army_id, army in pairs(self.armies) do
        if army:alive() then
            winner = army_id
            units = army:total_units()
            break -- Only one winner possible
        end
    end
    return winner, units
end

function Battlefield:total_units()
    local count = 0
    for _, army in pairs(self.armies) do
        count = count + army:total_units()
    end
    return count
end

local function parse_input(input_data)
    local armies = { [1] = Army:new(1), [2] = Army:new(2) }
    local current_army_id = 0
    local group_counter = 0 -- For unique group IDs

    for line in input_data:gmatch("[^\r\n]+") do
        line = line:match("^%s*(.-)%s*$") -- Trim line whitespace

        if line == "Immune System:" then
            current_army_id = 1
        elseif line == "Infection:" then
            current_army_id = 2
        elseif #line > 0 and current_army_id > 0 then
            local units, hp, ad, initiative
            local attack_type = ""
            local immunities_str, weaknesses_str

            -- Basic stats
            units, hp, ad, attack_type, initiative = line:match("^(%d+) units each with (%d+) hit points.*with an attack that does (%d+) (%w+) damage at initiative (%d+)$")

            if units then -- Check if basic pattern matched
                units = tonumber(units)
                hp = tonumber(hp)
                ad = tonumber(ad)
                initiative = tonumber(initiative)

                -- Immunities and Weaknesses (optional)
                local modifiers = line:match("%((.*)%)")
                local immunities = {}
                local weaknesses = {}

                if modifiers then
                    local immune_part = modifiers:match("immune to ([^;)]+)")
                    if immune_part then
                         immunities = split(immune_part, ",")
                    end
                    local weak_part = modifiers:match("weak to ([^;)]+)")
                    if weak_part then
                        weaknesses = split(weak_part, ",")
                    end
                end

                group_counter = group_counter + 1
                local group = Group:new(units, hp, ad, attack_type, initiative, immunities, weaknesses, current_army_id, group_counter)
                armies[current_army_id]:add_group(group)
            end
        end
    end
    return Battlefield:new(armies)
end

local function run_simulation(input_data, boost)
    local battle = parse_input(input_data)

    if not battle.armies[1] then
        error("Immune System army not found.")
    end
    battle.armies[1]:boost(boost)

    local round = 0
    local max_rounds = 5000 -- Stalemate detection limit

    while battle:active() and round < max_rounds do
        round = round + 1
        local units_before = battle:total_units()

        battle:find_targets()
        battle:attack_phase()
        battle:clean() -- Remove dead groups after the attack phase

        local units_after = battle:total_units()
        if units_before == units_after then
            return 0, 0, true -- Stalemate detected
        end
    end

    local winner, units = battle:result()
    return winner, units, (round == max_rounds and battle:active()) -- Return stalemate if max rounds reached and still active
end


local function find_min_boost(input_data)
    local boost = 0
    while true do
        local winner, units, stalemate = run_simulation(input_data, boost)
        if not stalemate and winner == 1 then
            return units -- Found the minimum boost for Immune System win
        end
        -- If Infection wins or stalemate, increase boost
        boost = boost + 1
        if boost > 1000 then -- Safety break
             error("Boost exceeded limit, likely impossible or error in logic.")
             break
        end
    end
end

local function main()
    local file = io.open("input.txt", "r")
    if not file then
        io.stderr:write("Error: Cannot open input.txt\n")
        return
    end
    local input_data = file:read("*a")
    file:close()

    -- Basic trim for the whole input data
    input_data = input_data:match("^%s*(.-)%s*$")

    local result = find_min_boost(input_data)
    print(result)
end

main()
