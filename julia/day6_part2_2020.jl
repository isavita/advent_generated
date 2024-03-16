open("input.txt", "r") do file
    totalCount = 0
    groupAnswers = Dict{Char,Int}()
    groupSize = 0

    for line in eachline(file)
        if isempty(line)
            for count in values(groupAnswers)
                if count == groupSize
                    totalCount += 1
                end
            end
            empty!(groupAnswers)
            groupSize = 0
        else
            groupSize += 1
            for question in line
                groupAnswers[question] = get(groupAnswers, question, 0) + 1
            end
        end
    end

    for count in values(groupAnswers)
        if count == groupSize
            totalCount += 1
        end
    end

    println(totalCount)
end