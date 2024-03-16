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
    data = read("input.txt", String)
    lines = split(data, "\n")

    boss = Character(
        parse_stat(lines[1]),
        parse_stat(lines[2]),
        parse_stat(lines[3]),
    )

    weapons = [
        Item(8, 4, 0),
        Item(10, 5, 0),
        Item(25, 6, 0),
        Item(40, 7, 0),
        Item(74, 8, 0),
    ]

    armors = [
        Item(0, 0, 0),
        Item(13, 0, 1),
        Item(31, 0, 2),
        Item(53, 0, 3),
        Item(75, 0, 4),
        Item(102, 0, 5),
    ]

    rings = [
        Item(0, 0, 0),
        Item(25, 1, 0),
        Item(50, 2, 0),
        Item(100, 3, 0),
        Item(20, 0, 1),
        Item(40, 0, 2),
        Item(80, 0, 3),
    ]

    max_cost = 0
    for w in weapons
        for a in armors
            for r1 in 1:length(rings)
                for r2 in r1+1:length(rings)
                    player = Character(100, w.damage, a.armor)
                    player = Character(player.hitpoints, player.damage + rings[r1].damage + rings[r2].damage, player.armor + rings[r1].armor + rings[r2].armor)
                    cost = w.cost + a.cost + rings[r1].cost + rings[r2].cost
                    if !player_wins(player, boss) && cost > max_cost
                        max_cost = cost
                    end
                end
            end
        end
    end

    println(max_cost)
end

main()