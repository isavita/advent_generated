
function part1(p1_start, p2_start)
    p1_pos = p1_start
    p2_pos = p2_start
    p1_score = 0
    p2_score = 0
    die_rolls = 0
    current_die = 1

    while true
        # Player 1's turn
        roll_sum = 0
        for _ in 1:3
            roll_sum += current_die
            current_die = current_die % 100 + 1
            die_rolls += 1
        end

        p1_pos = (p1_pos + roll_sum - 1) % 10 + 1
        p1_score += p1_pos

        if p1_score >= 1000
            return p2_score * die_rolls
        end

        # Player 2's turn
        roll_sum = 0
        for _ in 1:3
            roll_sum += current_die
            current_die = current_die % 100 + 1
            die_rolls += 1
        end

        p2_pos = (p2_pos + roll_sum - 1) % 10 + 1
        p2_score += p2_pos

        if p2_score >= 1000
            return p1_score * die_rolls
        end
    end
end

function part2(p1_start, p2_start)
    # Precompute dice roll frequencies
    roll_freq = Dict{Int, Int}()
    for d1 in 1:3, d2 in 1:3, d3 in 1:3
        roll_freq[d1 + d2 + d3] = get(roll_freq, d1 + d2 + d3, 0) + 1
    end

    # Memoization cache
    cache = Dict{Tuple, Tuple{Int, Int}}()

    function play(p1_pos, p2_pos, p1_score, p2_score, p1_turn)
        # Check cache
        key = (p1_pos, p2_pos, p1_score, p2_score, p1_turn)
        if haskey(cache, key)
            return cache[key]
        end

        # Base case: check for win
        if p1_score >= 21
            return (1, 0)
        elseif p2_score >= 21
            return (0, 1)
        end

        # Recursive case
        wins = (0, 0)
        for (roll, freq) in roll_freq
            if p1_turn
                new_pos = (p1_pos + roll - 1) % 10 + 1
                new_score = p1_score + new_pos
                sub_wins = play(new_pos, p2_pos, new_score, p2_score, false)
                wins = (wins[1] + sub_wins[1] * freq, wins[2] + sub_wins[2] * freq)
            else
                new_pos = (p2_pos + roll - 1) % 10 + 1
                new_score = p2_score + new_pos
                sub_wins = play(p1_pos, new_pos, p1_score, new_score, true)
                wins = (wins[1] + sub_wins[1] * freq, wins[2] + sub_wins[2] * freq)
            end
        end

        # Store in cache
        cache[key] = wins
        return wins
    end

    # Return the maximum number of wins
    return maximum(play(p1_start, p2_start, 0, 0, true))
end

# Read input from file
function main()
    input = readlines("input.txt")
    p1_start = parse(Int, split(input[1], ": ")[2])
    p2_start = parse(Int, split(input[2], ": ")[2])

    println("Part 1: ", part1(p1_start, p2_start))
    println("Part 2: ", part2(p1_start, p2_start))
end

main()
