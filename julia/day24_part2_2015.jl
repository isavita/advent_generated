using Base.Filesystem

function main()
    data = read("input.txt", String)
    lines = split(strip(data), "\n")
    packages = parse.(Int, lines)
    total_weight = sum(packages)
    target_weight = total_weight ÷ 4
    best_qe = typemax(Int)
    best_length = typemax(Int)

    for comb in 1:(2^length(packages) - 1)
        group_weight, qe, group_length = 0, 1, 0
        for i in 1:length(packages)
            if comb & (1 << (i - 1)) != 0
                group_weight += packages[i]
                qe *= packages[i]
                group_length += 1
            end
        end
        if group_weight == target_weight && group_length <= best_length
            if group_length < best_length || qe < best_qe
                if can_split(packages, comb, target_weight)
                    best_length = group_length
                    best_qe = qe
                end
            end
        end
    end

    println(best_qe)
end

function can_split(packages, first_group_comb, target_weight)
    remaining_packages = [packages[i] for i in 1:length(packages) if (first_group_comb & (1 << (i - 1))) == 0]
    for comb1 in 1:(2^length(remaining_packages) - 1)
        group1_weight = 0
        for i in 1:length(remaining_packages)
            if comb1 & (1 << (i - 1)) != 0
                group1_weight += remaining_packages[i]
            end
        end
        if group1_weight == target_weight
            for comb2 in 1:(2^length(remaining_packages) - 1)
                if (comb1 & comb2) == 0
                    group2_weight = 0
                    for i in 1:length(remaining_packages)
                        if comb2 & (1 << (i - 1)) != 0
                            group2_weight += remaining_packages[i]
                        end
                    end
                    if group2_weight == target_weight
                        return true
                    end
                end
            end
        end
    end
    return false
end

main()