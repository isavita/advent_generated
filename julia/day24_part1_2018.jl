
using Printf

mutable struct Group
    units::Int
    hitPoints::Int
    attackDamage::Int
    attackType::String
    initiative::Int
    immunities::Vector{String}
    weaknesses::Vector{String}
    attacker::Union{Group, Nothing}
    target::Union{Group, Nothing}
end

function effectivePower(g::Group)
    return g.units * g.attackDamage
end

function damageDealt(g::Group, e::Group)
    if g.attackType in e.immunities
        return 0
    end
    if g.attackType in e.weaknesses
        return effectivePower(g) * 2
    end
    return effectivePower(g)
end

function findTargets!(battlefield)
    for (army, groups) in battlefield
        sort!(groups, by=g -> (-effectivePower(g), -g.initiative))
        
        for group in groups
            group.target = nothing
            group.attacker = nothing
            
            if group.units <= 0
                continue
            end
            
            maxDamage = 0
            targetGroup = nothing
            
            for (enemyArmy, enemyGroups) in battlefield
                if army == enemyArmy
                    continue
                end
                
                for enemyGroup in enemyGroups
                    if enemyGroup.units <= 0 || enemyGroup.attacker !== nothing
                        continue
                    end
                    
                    damage = damageDealt(group, enemyGroup)
                    
                    if damage == 0
                        continue
                    end
                    
                    if damage > maxDamage || 
                       (damage == maxDamage && 
                        (targetGroup === nothing || 
                         effectivePower(enemyGroup) > effectivePower(targetGroup) || 
                         (effectivePower(enemyGroup) == effectivePower(targetGroup) && 
                          enemyGroup.initiative > targetGroup.initiative)))
                        maxDamage = damage
                        targetGroup = enemyGroup
                    end
                end
            end
            
            if targetGroup !== nothing
                group.target = targetGroup
                targetGroup.attacker = group
            end
        end
    end
end

function attack!(initiative)
    sort!(initiative, by=g -> -g.initiative)
    
    for group in initiative
        if group.units <= 0 || group.target === nothing
            continue
        end
        
        damage = damageDealt(group, group.target)
        unitsKilled = min(group.target.units, div(damage, group.target.hitPoints))
        group.target.units -= unitsKilled
        
        group.target.attacker = nothing
        group.target = nothing
    end
end

function cleanBattlefield!(battlefield)
    for army in keys(battlefield)
        filter!(g -> g.units > 0, battlefield[army])
    end
end

function conditionFight(input)
    battlefield = Dict{Int, Vector{Group}}()
    initiative = Group[]
    
    currentArmy = 0
    armies = Dict("Immune System" => 1, "Infection" => 2)
    
    for line in input
        if occursin(":", line)
            armyName = split(line, ":")[1]
            currentArmy = get(armies, armyName, 0)
        elseif occursin("units", line)
            m = match(r"(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)", line)
            
            immunities = String[]
            weaknesses = String[]
            
            immMatch = match(r"immune to ([^;)]+)", line)
            if immMatch !== nothing
                immunities = split(immMatch.captures[1], ", ")
            end
            
            weakMatch = match(r"weak to ([^;)]+)", line)
            if weakMatch !== nothing
                weaknesses = split(weakMatch.captures[1], ", ")
            end
            
            group = Group(
                parse(Int, m.captures[1]),
                parse(Int, m.captures[2]),
                parse(Int, m.captures[3]),
                m.captures[4],
                parse(Int, m.captures[5]),
                immunities,
                weaknesses,
                nothing,
                nothing
            )
            
            push!(get!(battlefield, currentArmy, Group[]), group)
            push!(initiative, group)
        end
    end
    
    while true
        findTargets!(battlefield)
        attack!(initiative)
        cleanBattlefield!(battlefield)
        
        armyAlive = 0
        totalUnits = 0
        
        for (army, groups) in battlefield
            if !isempty(groups)
                armyAlive += 1
                totalUnits += sum(g.units for g in groups)
            end
        end
        
        if armyAlive <= 1
            return totalUnits
        end
    end
end

function main()
    input = readlines("input.txt")
    println(conditionFight(input))
end

main()
