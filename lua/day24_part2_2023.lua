
-- Day 24: Never Tell Me The Odds
-- Author: Your Name/AI Assistant
-- Date:   December 24, 2023

-- Increase max stack size if needed for deep recursion (though not used here)
-- debug.sethook() 

local M = {}

-- Configuration for Part 1 Test Area
-- Use the example values:
-- local MIN_COORD = 7
-- local MAX_COORD = 27
-- Use the actual puzzle values:
local MIN_COORD = 200000000000000
local MAX_COORD = 400000000000000

-- --- Helper Functions ---

-- Parses a line "px, py, pz @ vx, vy, vz" into a table
local function parse_line(line)
    local px, py, pz, vx, vy, vz = line:match("([%d-]+),%s*([%d-]+),%s*([%d-]+)%s*@%s*([%d-]+),%s*([%d-]+),%s*([%d-]+)")
    if px then
        -- Convert matched strings to numbers
        return {
            px = tonumber(px), py = tonumber(py), pz = tonumber(pz),
            vx = tonumber(vx), vy = tonumber(vy), vz = tonumber(vz)
        }
    end
    return nil -- Return nil if line doesn't match format
end

-- Reads hailstones data from the input file
local function read_hailstones(filename)
    local hailstones = {}
    local file = io.open(filename, "r")
    if not file then
        error("Error: Could not open file '" .. filename .. "'")
    end

    for line in file:lines() do
        local h = parse_line(line)
        if h then
            table.insert(hailstones, h)
        else
            -- Optionally print a warning for non-matching lines
            -- print("Warning: Skipping invalid line format: " .. line)
        end
    end

    file:close()
    return hailstones
end

-- --- Part 1 ---

-- Calculates the intersection of two hailstones' paths in the XY plane
-- Returns intersect_x, intersect_y, t1, t2 if they intersect, otherwise nil
local function calculate_xy_intersection(h1, h2)
    -- Equations:
    -- h1.px + h1.vx * t1 = h2.px + h2.vx * t2
    -- h1.py + h1.vy * t1 = h2.py + h2.vy * t2
    --
    -- Rearranged:
    -- h1.vx * t1 - h2.vx * t2 = h2.px - h1.px
    -- h1.vy * t1 - h2.vy * t2 = h2.py - h1.py
    --
    -- Solve using determinant (Cramer's rule or substitution)
    -- Denominator = h1.vx * (-h2.vy) - (-h2.vx) * h1.vy = h2.vx * h1.vy - h1.vx * h2.vy
    local denominator = h2.vx * h1.vy - h1.vx * h2.vy

    -- If denominator is zero, lines are parallel (or coincident) - no single intersection point
    if math.abs(denominator) < 1e-12 then -- Use tolerance for float comparison
        return nil
    end

    -- Calculate t1 and t2
    local t1 = ( (h2.px - h1.px) * (-h2.vy) - (-h2.vx) * (h2.py - h1.py) ) / denominator
    local t2 = ( h1.vx * (h2.py - h1.py) - (h2.px - h1.px) * h1.vy ) / denominator

    -- Calculate intersection point using t1
    local intersect_x = h1.px + h1.vx * t1
    local intersect_y = h1.py + h1.vy * t1

    return intersect_x, intersect_y, t1, t2
end

-- Solves Part 1: Counts future intersections within the test area
function M.solve_part1(hailstones)
    local count = 0
    local n = #hailstones

    for i = 1, n - 1 do
        for j = i + 1, n do
            local h1 = hailstones[i]
            local h2 = hailstones[j]

            local ix, iy, t1, t2 = calculate_xy_intersection(h1, h2)

            -- Check if intersection exists and is in the future for both
            if ix and t1 > 0 and t2 > 0 then
                -- Check if intersection is within the test area bounds
                if ix >= MIN_COORD and ix <= MAX_COORD and
                   iy >= MIN_COORD and iy <= MAX_COORD then
                    count = count + 1
                end
            end
        end
    end
    return count
end

-- --- Part 2 ---

-- Gaussian elimination to solve Ax = b for a system of linear equations.
-- Modifies A and b in place. Returns the solution vector x (which is b after reduction).
-- Handles potential precision issues by assuming the input allows for a near-integer solution.
local function gaussian_elimination(A, b)
    local n = #A
    -- Basic dimension check
    if n == 0 or #A[1] ~= n or #b ~= n then
        error("Invalid matrix or vector dimensions for Gaussian elimination")
    end

    -- Forward elimination to Reduced Row Echelon Form (RREF)
    for j = 1, n do -- Iterate through columns (pivot columns)
        -- Find pivot row (row with max absolute value in current column j, from row j downwards)
        local pivot_row = j
        for i = j + 1, n do
            if math.abs(A[i][j]) > math.abs(A[pivot_row][j]) then
                pivot_row = i
            end
        end

        -- Swap rows if necessary to bring pivot to row j
        if pivot_row ~= j then
            A[j], A[pivot_row] = A[pivot_row], A[j]
            b[j], b[pivot_row] = b[pivot_row], b[j]
        end

        local pivot_val = A[j][j]

        -- Check for near-zero pivot (potential singularity or dependency)
        if math.abs(pivot_val) < 1e-12 then
            -- This indicates the system might not have a unique solution,
            -- or the chosen hailstones lead to dependent equations.
            -- For AoC, often the specific input avoids this, or the dependency cancels out.
            -- We can print a warning but attempt to continue. If it fails, this is why.
            print("Warning: Near-zero pivot encountered at column " .. j .. ". System might be singular/ill-conditioned.")
            -- If we continue, dividing by near-zero will cause issues.
            -- A robust solver would handle this differently (e.g., SVD, detect dependency).
            -- For this context, let's assume the puzzle input is well-behaved.
            -- If pivot_val is truly zero, we cannot normalize this row using it.
            -- We could try to find another non-zero element in the column below or skip.
            -- Let's return nil to indicate failure if pivot is too small.
             return nil
        end

        -- Normalize the pivot row (divide row j by pivot_val)
        for k = j, n do -- Start from j as elements before are already 0
            A[j][k] = A[j][k] / pivot_val
        end
        b[j] = b[j] / pivot_val
        A[j][j] = 1.0 -- Ensure it's exactly 1 after division

        -- Eliminate column j in all other rows (i != j)
        for i = 1, n do
            if i ~= j then
                local factor = A[i][j]
                -- Subtract factor * (pivot row j) from row i
                for k = j, n do -- Start from column j
                    A[i][k] = A[i][k] - factor * A[j][k]
                end
                b[i] = b[i] - factor * b[j]
                A[i][j] = 0.0 -- Ensure it's exactly 0 after elimination
            end
        end
    end

    -- After RREF, A should be the identity matrix, and b contains the solution vector x.
    return b
end


-- Solves Part 2: Finds the initial position of the rock
function M.solve_part2(hailstones)
    -- We need 6 independent linear equations to solve for 6 unknowns:
    -- rx, ry, rz (rock position) and rvx, rvy, rvz (rock velocity).
    -- Each pair of hailstones (i, j) gives equations based on the condition:
    -- (Pi - R) x (Vi - VR) = 0 and (Pj - R) x (Vj - VR) = 0
    -- Which leads to linear equations in R and VR:
    -- rx*(vy_i - vy_j) - ry*(vx_i - vx_j) + rvx*(py_i - py_j) - rvy*(px_i - px_j) = (px_i*vy_i - py_i*vx_i) - (px_j*vy_j - py_j*vx_j)  (XY-plane relation)
    -- rx*(vz_i - vz_j) - rz*(vx_i - vx_j) + rvx*(pz_i - pz_j) - rvz*(px_i - px_j) = (px_i*vz_i - pz_i*vx_i) - (px_j*vz_j - pz_j*vx_j)  (XZ-plane relation)
    -- ry*(vz_i - vz_j) - rz*(vy_i - vy_j) + rvy*(pz_i - pz_j) - rvz*(py_i - py_j) = (py_i*vz_i - pz_i*vy_i) - (py_j*vz_j - pz_j*vy_j)  (YZ-plane relation)

    -- We need at least 4 hailstones to generate enough equations.
    -- Let's use hailstones 1, 2, 3, 4 (using 1-based Lua indexing).
    if #hailstones < 4 then
        error("Need at least 4 hailstones to solve Part 2")
    end

    local h0, h1, h2, h3 = hailstones[1], hailstones[2], hailstones[3], hailstones[4]

    -- Setup the 6x6 matrix A and 6x1 vector b for the system Ax = b
    -- Unknowns vector x = [rx, ry, rz, rvx, rvy, rvz] (indices 1 to 6)
    local A = {}
    local b = {}
    for i = 1, 6 do
        A[i] = {}
        for j = 1, 6 do A[i][j] = 0 end
        b[i] = 0
    end

    -- Helper to fill a row based on two hailstones (hi, hj) and the plane ('xy', 'xz', 'yz')
    local function fill_row(row_idx, hi, hj, plane_type)
        local dvx, dvy, dvz = hi.vx - hj.vx, hi.vy - hj.vy, hi.vz - hj.vz
        local dpx, dpy, dpz = hi.px - hj.px, hi.py - hj.py, hi.pz - hj.pz
        local const -- Constant term on the right side

        if plane_type == 'xy' then
            -- Equation: rx*(-dvy) + ry*(dvx) + rvx*(-dpy) + rvy*(dpx) = const_xy
            A[row_idx][1] = -dvy        -- Coeff rx
            A[row_idx][2] = dvx         -- Coeff ry
            A[row_idx][3] = 0           -- Coeff rz
            A[row_idx][4] = -dpy        -- Coeff rvx
            A[row_idx][5] = dpx         -- Coeff rvy
            A[row_idx][6] = 0           -- Coeff rvz
            const = (hj.px * hj.vy - hj.py * hj.vx) - (hi.px * hi.vy - hi.py * hi.vx)
        elseif plane_type == 'xz' then
             -- Equation: rx*(-dvz) + rz*(dvx) + rvx*(-dpz) + rvz*(dpx) = const_xz
            A[row_idx][1] = -dvz        -- Coeff rx
            A[row_idx][2] = 0           -- Coeff ry
            A[row_idx][3] = dvx         -- Coeff rz
            A[row_idx][4] = -dpz        -- Coeff rvx
            A[row_idx][5] = 0           -- Coeff rvy
            A[row_idx][6] = dpx         -- Coeff rvz
            const = (hj.px * hj.vz - hj.pz * hj.vx) - (hi.px * hi.vz - hi.pz * hi.vx)
         elseif plane_type == 'yz' then
             -- Equation: ry*(-dvz) + rz*(dvy) + rvy*(-dpz) + rvz*(dpy) = const_yz
            A[row_idx][1] = 0           -- Coeff rx
            A[row_idx][2] = -dvz        -- Coeff ry
            A[row_idx][3] = dvy         -- Coeff rz
            A[row_idx][4] = 0           -- Coeff rvx
            A[row_idx][5] = -dpz        -- Coeff rvy
            A[row_idx][6] = dpy         -- Coeff rvz
            const = (hj.py * hj.vz - hj.pz * hj.vy) - (hi.py * hi.vz - hi.pz * hi.vy)
        else
            error("Invalid plane type specified")
        end
        b[row_idx] = const
    end

    -- Generate 6 equations using pairs (h0, h1), (h0, h2), (h0, h3)
    -- Use XY and XZ planes. Could mix YZ too.
    fill_row(1, h0, h1, 'xy')
    fill_row(2, h0, h2, 'xy')
    fill_row(3, h0, h3, 'xy') -- Using h3 for the third xy equation
    fill_row(4, h0, h1, 'xz')
    fill_row(5, h0, h2, 'xz')
    fill_row(6, h0, h3, 'xz') -- Using h3 for the third xz equation

    -- Solve the system Ax = b using Gaussian elimination
    local solution = gaussian_elimination(A, b)

    if not solution then
        error("Failed to solve the linear system for Part 2 (matrix possibly singular).")
    end

    -- The solution vector contains [rx, ry, rz, rvx, rvy, rvz]
    -- We need the sum rx + ry + rz.
    -- Since Gaussian elimination uses floating point, round the results to nearest integer.
    local rx = math.floor(solution[1] + 0.5)
    local ry = math.floor(solution[2] + 0.5)
    local rz = math.floor(solution[3] + 0.5)
    
    -- local rvx = math.floor(solution[4] + 0.5)
    -- local rvy = math.floor(solution[5] + 0.5)
    -- local rvz = math.floor(solution[6] + 0.5)
    -- print(string.format("Debug: Rock Pos=(%.3f, %.3f, %.3f) Vel=(%.3f, %.3f, %.3f)", solution[1], solution[2], solution[3], solution[4], solution[5], solution[6]))
    -- print(string.format("Debug: Rounded Rock Pos=(%d, %d, %d) Vel=(%d, %d, %d)", rx, ry, rz, rvx, rvy, rvz))


    return rx + ry + rz
end


-- --- Main Entry Point ---

function M.main()
    local filename = "input.txt"
    print("Reading input from " .. filename .. "...")
    local hailstones = read_hailstones(filename)
    print("Read " .. #hailstones .. " hailstones.")

    -- Solve Part 1
    print("\n--- Solving Part 1 ---")
    local part1_start = os.clock()
    local part1_result = M.solve_part1(hailstones)
    local part1_time = os.clock() - part1_start
    print(string.format("Part 1 Result: %d (calculated in %.4f seconds)", part1_result, part1_time))

    -- Solve Part 2
    print("\n--- Solving Part 2 ---")
    local part2_start = os.clock()
    local part2_result = M.solve_part2(hailstones)
    local part2_time = os.clock() - part2_start
    -- Use %d format specifier for potentially very large integer result
    print(string.format("Part 2 Result: %d (calculated in %.4f seconds)", part2_result, part2_time))
end

-- Execute the main function when the script is run
M.main()

-- Return the module table (optional, useful if required by another script)
return M
