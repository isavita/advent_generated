
function process_line(line)
    try
        return parse(Int, chomp(line))
    catch
        println("Error parsing line")
        return 0
    end
end

function calc_fuel_mass(mass)
    fuel = floor(Int, mass / 3) - 2
    if fuel <= 0
        return 0
    else
        return fuel + calc_fuel_mass(fuel)
    end
end

function get_total(masses)
    total = 0
    for mass in masses
        total += calc_fuel_mass(mass)
    end
    return total
end

open("input.txt") do file
    masses = []
    for line in eachline(file)
        push!(masses, process_line(line))
    end
    total = get_total(masses)
    println(total)
end
