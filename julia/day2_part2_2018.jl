
open("input.txt") do file
    lines = readlines(file)
    for i in 1:length(lines)-1
        for j in i+1:length(lines)
            diff = 0
            for k in 1:length(lines[i])
                if lines[i][k] != lines[j][k]
                    diff += 1
                    if diff > 1
                        break
                    end
                end
            end
            if diff == 1
                common = ""
                for k in 1:length(lines[i])
                    if lines[i][k] == lines[j][k]
                        common *= string(lines[i][k])
                    end
                end
                println(common)
                return
            end
        end
    end
end
