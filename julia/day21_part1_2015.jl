struct Item
    cost::Int
    damage::Int
    armor::Int
end

struct Character
    hitpoints::Int
    damage::Int
    armor::Int
end

function parse_stat(line)
    parts = split(line, ": ")
    return parse(Int, parts[2])
end

function player_wins(player::Character, boss::Character)
    player_damage = max(1, player.damage - boss.armor)
    boss_damage = max(1, boss.damage - player.armor)

    player_turns = ceil(boss.hitpoints / player_damage)
    boss_turns = ceil(player.hitpoints / boss_damage)

    return player_turns <= boss_turns
end

function main()
    # Read the boss's stats from the file.
    lines = readlines("input.txt")
    boss = Character(parse_stat(lines[1]), parse_stat(lines[2]), parse_stat(lines[3]))

    weapons = [
        Item(8, 4, 0),
        Item(10, 5, 0),
        Item(25, 6, 0),
        Item(40, 7, 0),
        Item(74, 8, 0)
    ]

    armors = [
        Item(0, 0, 0),
        Item(13, 0, 1),
        Item(31, 0, 2),
        Item(53, 0, 3),
        Item(75, 0, 4),
        Item(102, 0, 5)
    ]

    rings = [
        Item(0, 0, 0),
        Item(25, 1, 0),
        Item(50, 2, 0),
        Item(100, 3, 0),
        Item(20, 0, 1),
        Item(40, 0, 2),
        Item(80, 0, 3)
    ]

    min_cost = typemax(Int)
    for w in weapons
        for a in armors
            for ri in 1:length(rings)
                for rj in ri+1:length(rings)
                    player_damage = w.damage + rings[ri].damage + rings[rj].damage
                    player_armor = a.armor + rings[ri].armor + rings[rj].armor
                    player = Character(100, player_damage, player_armor)
                    cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
                    if player_wins(player, boss) && cost < min_cost
                        min_cost = cost
                    end
                end
            end
        end
    end

    println(min_cost)
end

main()