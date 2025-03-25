
local function split(input, sep)
    local t = {}
    for str in string.gmatch(input, "([^"..sep.."]+)") do
        table.insert(t, str)
    end
    return t
end

local Group = {}
Group.__index = Group

function Group.new(units, hit_points, attack_damage, attack_type, initiative, immunities, weaknesses)
    local self = setmetatable({}, Group)
    self.units = units
    self.hit_points = hit_points
    self.attack_damage = attack_damage
    self.attack_type = attack_type
    self.initiative = initiative
    self.immunities = immunities or {}
    self.weaknesses = weaknesses or {}
    self.target = nil
    return self
end

function Group:effective_power()
    return self.units * self.attack_damage
end

function Group:damage_dealt(enemy)
    if self.attack_type and enemy.immunities then
        for _, v in ipairs(enemy.immunities) do
            if v == self.attack_type then return 0 end
        end
    end
    if self.attack_type and enemy.weaknesses then
        for _, v in ipairs(enemy.weaknesses) do
            if v == self.attack_type then return self:effective_power() * 2 end
        end
    end
    return self:effective_power()
end

local Army = {}
Army.__index = Army

function Army.new(groups)
  local self = setmetatable({}, Army)
    self.groups = groups
    return self
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

local Battlefield = {}
Battlefield.__index = Battlefield

function Battlefield.new(armies)
    local self = setmetatable({}, Battlefield)
    self.armies = armies
    return self
end

function Battlefield:find_targets()
    for army_id, army in pairs(self.armies) do
        table.sort(army.groups, function(a, b)
            if a:effective_power() == b:effective_power() then
                return a.initiative > b.initiative
            end
            return a:effective_power() > b:effective_power()
        end)

        for _, group in ipairs(army.groups) do
            local most_damage = 0
            local target_group = nil

            for enemy_army_id, enemy_army in pairs(self.armies) do
                if army_id ~= enemy_army_id and group.units > 0 then
                    for _, enemy_group in ipairs(enemy_army.groups) do
                        if enemy_group.units > 0 and not enemy_group.attacker then
                            local damage = group:damage_dealt(enemy_group)
                            if damage > most_damage then
                                most_damage = damage
                                target_group = enemy_group
                            elseif damage == most_damage and target_group then
                                if enemy_group:effective_power() > target_group:effective_power() then
                                     target_group = enemy_group
                                elseif enemy_group:effective_power() == target_group:effective_power() and enemy_group.initiative > target_group.initiative then
                                    target_group = enemy_group
                                end
                            end
                        end
                    end
                end
            end
            if target_group then
                group.target = target_group
                target_group.attacker = group
            end
        end
    end
end

function Battlefield:clean()
    for _, army in pairs(self.armies) do
        local new_groups = {}
      for _,group in ipairs(army.groups) do
        if group.units > 0 then
          table.insert(new_groups, group)
        end
      end
      army.groups = new_groups
    end
end

function Battlefield:active()
  local num_active = 0
    for _, army in pairs(self.armies) do
        if army:alive() then
            num_active = num_active + 1
        end
    end
    return num_active > 1
end

function Battlefield:result()
    local winner = 0
    local units = 0
    for army_id, army in pairs(self.armies) do
        if army:alive() then
            winner = army_id
            for _, g in ipairs(army.groups) do
                units = units + g.units
            end
        end
    end
    return winner, units
end

local function attack(groups)
    table.sort(groups, function(a, b) return a.initiative > b.initiative end)
    for _, group in ipairs(groups) do
        if group.units > 0 and group.target and group.target.units > 0 then
            local damage = group:damage_dealt(group.target)
            group.target.units = math.max(0, group.target.units - math.floor(damage / group.target.hit_points))
        end
        if group.target then
            group.target.attacker = nil
            group.target = nil
        end
    end
end

local function parse_input(input_data)
    local armies = {}
    local initiative = {}
    local current_army_id = 0

    for line in string.gmatch(input_data, "[^\n]+") do
        if string.match(line, "^Immune System:") then
            current_army_id = 1
            armies[current_army_id] = Army.new({})
        elseif string.match(line, "^Infection:") then
            current_army_id = 2
            armies[current_army_id] = Army.new({})
        elseif current_army_id > 0 then
            local units, hit_points, attack_damage, attack_type, initiative_value = string.match(line, "^(%d+) units each with (%d+) hit points .*with an attack that does (%d+) (%w+) damage at initiative (%d+)")
            if units then
                units = tonumber(units)
                hit_points = tonumber(hit_points)
                attack_damage = tonumber(attack_damage)
                initiative_value = tonumber(initiative_value)

                local immunities_match = string.match(line, "immune to ([^;)]*)")
                local immunities = immunities_match and split(immunities_match, ", ") or {}

                local weaknesses_match = string.match(line, "weak to ([^;)]*)")
                local weaknesses = weaknesses_match and split(weaknesses_match, ", ") or {}

                local group = Group.new(units, hit_points, attack_damage, attack_type, initiative_value, immunities, weaknesses)
                table.insert(armies[current_army_id].groups, group)
                table.insert(initiative, group)
            end
        end
    end

    return Battlefield.new(armies), initiative
end

local function condition_fight(input_data)
    local battle, initiative = parse_input(input_data)
     while battle:active() do
        battle:find_targets()
        attack(initiative)
        battle:clean()
    end
    local _, units = battle:result()
    return units
end

local function main()
    local file = io.open("input.txt", "r")
    local input_data = file:read("*all")
    file:close()
    print(condition_fight(input_data))
end

main()
