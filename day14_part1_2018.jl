function read_input(filename)
    open(filename, "r") do file
        return parse(Int, readline(file))
    end
end

function generate_recipes(num_recipes)
    scoreboard = [3, 7]
    elf1_idx = 1
    elf2_idx = 2

    while length(scoreboard) < num_recipes + 10
        elf1_score = scoreboard[elf1_idx]
        elf2_score = scoreboard[elf2_idx]

        # Calculate new recipes
        new_recipe_score = elf1_score + elf2_score
        if new_recipe_score >= 10
            push!(scoreboard, div(new_recipe_score, 10))
        end
        push!(scoreboard, new_recipe_score % 10)

        # Update elf positions
        elf1_idx = (elf1_idx + elf1_score + 1) % length(scoreboard)
        elf1_idx = elf1_idx == 0 ? length(scoreboard) : elf1_idx
        elf2_idx = (elf2_idx + elf2_score + 1) % length(scoreboard)
        elf2_idx = elf2_idx == 0 ? length(scoreboard) : elf2_idx
    end

    return scoreboard
end

function main()
    num_recipes = read_input("input.txt")
    scoreboard = generate_recipes(num_recipes)
    result = scoreboard[num_recipes+1:num_recipes+10]
    println(join(result))
end

main()