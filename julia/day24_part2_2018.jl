
using Printf
using Base.Order

mutable struct Group
    id::Int
    army_id::Int
    units::Int
    hit_points::Int
    attack_damage::Int
    attack_type::String
    initiative::Int
    immunities::Set{String}
    weaknesses::Set{String}
    target::Union{Group, Nothing}
    attacker::Union{Group, Nothing}
    original_attack_damage::Int
end

function effective_power(g::Group)
    return g.units * g.attack_damage
end

function damage_dealt(attacker::Group, defender::Group)::Int
    if attacker.attack_type in defender.immunities
        return 0
    end
    damage = effective_power(attacker)
    if attacker.attack_type in defender.weaknesses
        damage *= 2
    end
    return damage
end

struct Army
    id::Int
    groups::Vector{Group}
end

function is_alive(a::Army)::Bool
    return any(g -> g.units > 0, a.groups)
end

function boost!(a::Army, amount::Int)
    for g in a.groups
        g.attack_damage = g.original_attack_damage + amount
    end
end

struct Battlefield
    armies::Dict{Int, Army}
    all_groups::Vector{Group}
end

function Base.deepcopy_internal(x::Battlefield, dict::IdDict)::Battlefield
    if haskey(dict, x)
        return dict[x]
    end

    new_all_groups = Vector{Group}(undef, length(x.all_groups))
    group_map = IdDict{Group, Group}()
    dict[x] = Battlefield(Dict{Int, Army}(), new_all_groups) # Initialize armies later

    for (i, g) in enumerate(x.all_groups)
         if !haskey(group_map, g)
             new_g = Group(
                 g.id,
                 g.army_id,
                 g.units,
                 g.hit_points,
                 g.attack_damage,
                 g.attack_type,
                 g.initiative,
                 deepcopy(g.immunities),
                 deepcopy(g.weaknesses),
                 nothing, # Reset target/attacker
                 nothing,
                 g.original_attack_damage
             )
             group_map[g] = new_g
             new_all_groups[i] = new_g
         end
    end

     # Populate armies and fix target/attacker references in the copy
    new_armies = dict[x].armies
    for (id, army) in x.armies
        new_army_groups = [group_map[g] for g in army.groups if haskey(group_map, g)]
        new_armies[id] = Army(id, new_army_groups)
    end

    for (i, g) in enumerate(x.all_groups)
        new_g = group_map[g]
        if g.target !== nothing && haskey(group_map, g.target)
             new_g.target = group_map[g.target]
        end
         if g.attacker !== nothing && haskey(group_map, g.attacker)
             new_g.attacker = group_map[g.attacker]
         end
    end

    return dict[x]
end


function parse_input(lines::Vector{String})::Battlefield
    armies = Dict{Int, Army}()
    all_groups = Group[]
    current_army_id = 0
    group_id_counter = 0

    group_regex = r"(\d+) units each with (\d+) hit points (?:\((.*?)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)"
    immune_regex = r"immune to ([\w, ]+)"
    weak_regex = r"weak to ([\w, ]+)"

    for line in lines
        line = strip(line)
        if isempty(line) continue end

        if line == "Immune System:"
            current_army_id = 1
            armies[current_army_id] = Army(current_army_id, Group[])
        elseif line == "Infection:"
            current_army_id = 2
            armies[current_army_id] = Army(current_army_id, Group[])
        else
            m = match(group_regex, line)
            if m !== nothing
                units = parse(Int, m.captures[1])
                hp = parse(Int, m.captures[2])
                details = m.captures[3]
                dmg = parse(Int, m.captures[4])
                atk_type = m.captures[5]
                init = parse(Int, m.captures[6])

                immunities = Set{String}()
                weaknesses = Set{String}()

                if details !== nothing
                    im_m = match(immune_regex, details)
                    if im_m !== nothing
                        union!(immunities, split(im_m.captures[1], ", "))
                    end
                    wk_m = match(weak_regex, details)
                    if wk_m !== nothing
                        union!(weaknesses, split(wk_m.captures[1], ", "))
                    end
                end

                group_id_counter += 1
                group = Group(group_id_counter, current_army_id, units, hp, dmg, atk_type, init, immunities, weaknesses, nothing, nothing, dmg)
                push!(armies[current_army_id].groups, group)
                push!(all_groups, group)
            end
        end
    end
    return Battlefield(armies, all_groups)
end

function select_targets!(battlefield::Battlefield)
    all_groups = filter(g -> g.units > 0, battlefield.all_groups)
    sort!(all_groups, by = g -> (effective_power(g), g.initiative), rev=true)

    targets_chosen = Set{Int}() # Store IDs of groups already targeted

    for attacker in all_groups
        attacker.target = nothing # Reset from previous round
        best_target = nothing
        max_damage = 0

        enemy_army_id = attacker.army_id == 1 ? 2 : 1
        if !haskey(battlefield.armies, enemy_army_id) continue end # Enemy army might be destroyed

        potential_targets = filter(g -> g.units > 0 && !(g.id in targets_chosen), battlefield.armies[enemy_army_id].groups)

        for defender in potential_targets
            dmg = damage_dealt(attacker, defender)
            if dmg == 0 continue end

            if dmg > max_damage
                max_damage = dmg
                best_target = defender
            elseif dmg == max_damage
                if best_target === nothing || effective_power(defender) > effective_power(best_target)
                    best_target = defender
                elseif effective_power(defender) == effective_power(best_target) && defender.initiative > best_target.initiative
                    best_target = defender
                end
            end
        end

        if best_target !== nothing
            attacker.target = best_target
            push!(targets_chosen, best_target.id)
        end
    end
end

function attack_phase!(battlefield::Battlefield)
    all_groups = filter(g -> g.units > 0, battlefield.all_groups)
    sort!(all_groups, by = g -> g.initiative, rev=true)

    total_units_killed = 0

    for attacker in all_groups
         # Check if attacker is still alive and has a valid target
        if attacker.units <= 0 || attacker.target === nothing || attacker.target.units <= 0
            continue
        end

        defender = attacker.target
        damage = damage_dealt(attacker, defender)
        units_killed = min(defender.units, damage ÷ defender.hit_points)

        defender.units -= units_killed
        total_units_killed += units_killed
    end
    return total_units_killed > 0 # Return true if any units were killed (no stalemate)
end

function cleanup!(battlefield::Battlefield)
     filter!(g -> g.units > 0, battlefield.all_groups)
     for army in values(battlefield.armies)
         filter!(g -> g.units > 0, army.groups)
     end
     # Reset attacker references (target reset in select_targets!)
     for g in battlefield.all_groups
         g.attacker = nothing
     end
 end

function simulate_battle(initial_battlefield::Battlefield, boost::Int)::Tuple{Int, Int, Bool}
    battlefield = deepcopy(initial_battlefield)
    immune_system_army = battlefield.armies[1]
    boost!(immune_system_army, boost)

    while is_alive(battlefield.armies[1]) && is_alive(battlefield.armies[2])
        select_targets!(battlefield)
        fight_occurred = attack_phase!(battlefield)
        cleanup!(battlefield)
        if !fight_occurred
            return 0, 0, true # Stalemate
        end
    end

    winner_id = 0
    remaining_units = 0
    if is_alive(battlefield.armies[1])
        winner_id = 1
        remaining_units = sum(g.units for g in battlefield.armies[1].groups)
    elseif is_alive(battlefield.armies[2])
        winner_id = 2
        remaining_units = sum(g.units for g in battlefield.armies[2].groups)
    end

    return winner_id, remaining_units, false # Winner, Units, No Stalemate
end

function find_minimum_boost(initial_battlefield::Battlefield)::Int
    boost = 0
    while true
        winner_id, remaining_units, stalemate = simulate_battle(initial_battlefield, boost)
        if !stalemate && winner_id == 1
            return remaining_units
        end
        boost += 1
        # Optional: Add a safeguard for extremely high boosts if needed
        # if boost > 10000 # Example limit
        #     error("Boost exceeded limit, likely infinite loop or impossible scenario")
        # end
    end
end

function main()
    lines = readlines("input.txt")
    initial_battlefield = parse_input(lines)
    result_units = find_minimum_boost(initial_battlefield)
    println(result_units)
end

main()
