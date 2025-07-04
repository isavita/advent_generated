
function main()
    fileID = fopen("input.txt", "r");
    polymer = strtrim(fgets(fileID));
    rules = containers.Map;
    while ~feof(fileID)
        line = strtrim(fgets(fileID));
        if ~isempty(line)
            parts = strsplit(line, " -> ");
            rules(parts{1}) = parts{2};
        end
    end
    fclose(fileID);

    for i = 1:10
        new_polymer = polymer(1);
        for j = 1:(length(polymer) - 1)
            pair = polymer(j:j+1);
            if isKey(rules, pair)
                new_polymer = [new_polymer, rules(pair)];
            end
            new_polymer = [new_polymer, polymer(j+1)];
        end
        polymer = new_polymer;
    end

    counts = containers.Map;
    for char = polymer
        if isKey(counts, char)
            counts(char) = counts(char) + 1;
        else
            counts(char) = 1;
        end
    end

    min_count = inf;
    max_count = 0;
    for count = values(counts)
        min_count = min(min_count, count{1});
        max_count = max(max_count, count{1});
    end

    disp(max_count - min_count);
end

main();
