
--[[
--- Day 13: Claw Contraption ---

Solves the Claw Contraption problem (Part 2).
Reads machine configurations from input.txt and calculates the minimum total tokens
to spend to win all possible prizes, considering the updated prize coordinates.

Input Format (input.txt):
Button A: X+[ax], Y+[ay]
Button B: X+[bx], Y+[by]
Prize: X=[px], Y=[py]
(repeated for each machine, potentially with blank lines in between)

Output Format (Standard Output):
The minimum total cost (integer).
--]]

-- Function to check if a number is essentially an integer within a small tolerance
local function is_integer(n, tolerance)
    tolerance = tolerance or 1e-9
    -- Check if n is non-nil and finite before proceeding
    if n == nil or n ~= n or n == math.huge or n == -math.huge then
        return false, nil
    end
    local rounded = math.floor(n + 0.5) -- Round to nearest integer
    return math.abs(n - rounded) < tolerance, rounded
end

-- Function to solve for the number of button presses (a, b) for a single machine
-- Returns the minimum cost if a non-negative integer solution exists, otherwise nil.
local function solve_machine(ax, ay, bx, by, px, py)
    -- The system of equations is:
    -- ax * a + bx * b = px
    -- ay * a + by * b = py

    -- Calculate the determinant of the coefficient matrix
    local det = ax * by - ay * bx

    -- If determinant is 0, the system has no unique solution.
    -- In this context, it means the buttons don't offer independent control
    -- required to reach an arbitrary point, or the prize lies on the line/point
    -- reachable, implying infinite solutions if reachable at all.
    -- We assume for this problem that det=0 means no specific solution.
    if det == 0 then
        return nil
    end

    -- Use Cramer's rule (or derivation via substitution/elimination) to find potential solutions for a and b
    -- Note: Calculations involve large numbers (px, py are ~10^14), potentially up to ~10^16 intermediate results.
    -- Lua numbers are typically IEEE 754 doubles, with ~15-16 decimal digits of precision.
    -- Integers up to 2^53 (~9e15) are represented exactly. Calculations might exceed this.
    local num_a = px * by - py * bx
    local num_b = ax * py - ay * px

    local a_float = num_a / det
    local b_float = num_b / det

    -- Check if the calculated solutions are non-negative integers (within tolerance)
    local is_a_int, a_int = is_integer(a_float)
    local is_b_int, b_int = is_integer(b_float)

    if is_a_int and is_b_int and a_int >= 0 and b_int >= 0 then
        -- Verify that the integer solution exactly satisfies the original equations.
        -- This is crucial due to potential floating-point inaccuracies.
        -- Perform checks using the rounded integer values.
        -- Note: Even these multiplications could potentially suffer precision loss if intermediate
        -- products exceed 2^53 significantly, but let's hope standard doubles suffice.
        if ax * a_int + bx * b_int == px and ay * a_int + by * b_int == py then
            -- Solution is valid, calculate the cost
            local cost = 3 * a_int + 1 * b_int
            return cost
        -- else
            -- This case implies floating point precision issues prevented exact verification.
            -- Given the problem constraints usually lead to exact integer solutions,
            -- if the rounded values don't verify, we likely don't have a true integer solution
            -- or intermediate calculations lost too much precision. Treat as non-solvable.
            -- print("WARN: Potential solution failed verification", ax, ay, bx, by, px, py, a_int, b_int)
        end
    end

    -- No valid non-negative integer solution found
    return nil
end

-- Main execution function
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        io.stderr:write("Error: Could not open input.txt\n")
        return
    end

    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local machines = {}
    local i = 1
    while i <= #lines do
        -- Attempt to read 3 consecutive lines defining a machine
        local lineA = lines[i]
        local lineB = lines[i+1]
        local lineP = lines[i+2]

        -- Check if the lines match the expected format
        if lineA and lineB and lineP and
           lineA:match("^Button A: X%+(%d+), Y%+(%d+)$") and
           lineB:match("^Button B: X%+(%d+), Y%+(%d+)$") and
           lineP:match("^Prize: X=(%d+), Y=(%d+)$") then

            -- Extract numbers using string.match
            local ax_str, ay_str = lineA:match("X%+(%d+), Y%+(%d+)")
            local bx_str, by_str = lineB:match("X%+(%d+), Y%+(%d+)")
            local px_str, py_str = lineP:match("X=(%d+), Y=(%d+)")

            -- Convert extracted strings to numbers
            local machine = {
                ax = tonumber(ax_str), ay = tonumber(ay_str),
                bx = tonumber(bx_str), by = tonumber(by_str),
                px = tonumber(px_str), py = tonumber(py_str)
            }

            -- Apply Part 2 offset to prize coordinates
            local offset = 10000000000000
            machine.px = machine.px + offset
            machine.py = machine.py + offset

            table.insert(machines, machine)
            i = i + 3 -- Move past the 3 lines read
        else
            -- If lines don't match, skip the current line (could be blank or malformed)
            i = i + 1
        end
    end

    -- Calculate the total minimum cost for all solvable machines
    local total_cost = 0
    local prizes_won = 0

    for idx, m in ipairs(machines) do
        local cost = solve_machine(m.ax, m.ay, m.bx, m.by, m.px, m.py)
        if cost then
            total_cost = total_cost + cost
            prizes_won = prizes_won + 1
            -- io.write(string.format("Machine %d: Solvable with cost %d\n", idx, cost)) -- Optional debug output
        -- else
            -- io.write(string.format("Machine %d: Not solvable\n", idx)) -- Optional debug output
        end
    end

    -- Print the final result
    print(total_cost)
end

-- Run the main function
main()
