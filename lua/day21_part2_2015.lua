
function parseStat(line)
    return tonumber(string.match(line, "%d+"))
end

function playerWins(player, boss)
    playerDamage = math.max(1, player.Damage - boss.Armor)
    bossDamage = math.max(1, boss.Damage - player.Armor)

    playerTurns = math.ceil(boss.HitPoints / playerDamage)
    bossTurns = math.ceil(player.HitPoints / bossDamage)

    return playerTurns <= bossTurns
end

data = io.open("input.txt", "r")
boss = {
    HitPoints = parseStat(data:read("*line")),
    Damage = parseStat(data:read("*line")),
    Armor = parseStat(data:read("*line"))
}

weapons = {
    {Cost = 8, Damage = 4},
    {Cost = 10, Damage = 5},
    {Cost = 25, Damage = 6},
    {Cost = 40, Damage = 7},
    {Cost = 74, Damage = 8}
}

armors = {
    {Cost = 0, Armor = 0},
    {Cost = 13, Armor = 1},
    {Cost = 31, Armor = 2},
    {Cost = 53, Armor = 3},
    {Cost = 75, Armor = 4},
    {Cost = 102, Armor = 5}
}

rings = {
    {Cost = 0},
    {Cost = 25, Damage = 1},
    {Cost = 50, Damage = 2},
    {Cost = 100, Damage = 3},
    {Cost = 20, Armor = 1},
    {Cost = 40, Armor = 2},
    {Cost = 80, Armor = 3}
}

maxCost = 0
for i, w in ipairs(weapons) do
    for j, a in ipairs(armors) do
        for ri = 1, #rings do
            for rj = ri + 1, #rings do
                player = {
                    HitPoints = 100,
                    Damage = w.Damage,
                    Armor = a.Armor
                }
                player.Damage = player.Damage + (rings[ri].Damage or 0) + (rings[rj].Damage or 0)
                player.Armor = player.Armor + (rings[ri].Armor or 0) + (rings[rj].Armor or 0)
                cost = w.Cost + a.Cost + rings[ri].Cost + rings[rj].Cost
                if not playerWins(player, boss) and cost > maxCost then
                    maxCost = cost
                end
            end
        end
    end
end

print(maxCost)
