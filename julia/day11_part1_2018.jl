using Base.Threads

function power_level(x, y, serial_number)
    rack_id = x + 10
    power = rack_id * y
    power += serial_number
    power *= rack_id
    power = div(power, 100) % 10
    power -= 5
    return power
end

function max_power_square(serial_number)
    max_power = -Inf
    max_x = max_y = 0
    for x in 1:298, y in 1:298
        total_power = 0
        for i in 0:2, j in 0:2
            total_power += power_level(x + i, y + j, serial_number)
        end
        if total_power > max_power
            max_power = total_power
            max_x = x
            max_y = y
        end
    end
    return max_x, max_y
end

open("input.txt", "r") do f
    serial_number = parse(Int, readline(f))
    x, y = max_power_square(serial_number)
    println("$x,$y")
end