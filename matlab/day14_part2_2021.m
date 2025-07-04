
function main()
    [template, rules] = read_input("input.txt");
    pair_counts = containers.Map;
    for i = 1:(length(template)-1)
        pair = template(i:i+1);
        if isKey(pair_counts, pair)
            pair_counts(pair) = pair_counts(pair) + 1;
        else
            pair_counts(pair) = 1;
        end
    end

    for step = 1:40
        new_pair_counts = containers.Map;
        keys_to_process = keys(pair_counts);
        for k = 1:length(keys_to_process)
            pair = keys_to_process{k};
            count = pair_counts(pair);
            if isKey(rules, pair)
                insert = rules(pair);
                new_pair1 = [pair(1), insert];
                new_pair2 = [insert, pair(2)];
                if isKey(new_pair_counts, new_pair1)
                    new_pair_counts(new_pair1) = new_pair_counts(new_pair1) + count;
                else
                    new_pair_counts(new_pair1) = count;
                end
                if isKey(new_pair_counts, new_pair2)
                    new_pair_counts(new_pair2) = new_pair_counts(new_pair2) + count;
                else
                    new_pair_counts(new_pair2) = count;
                end
            else
                if isKey(new_pair_counts, pair)
                    new_pair_counts(pair) = new_pair_counts(pair) + count;
                else
                    new_pair_counts(pair) = count;
                end
            end
        end
        pair_counts = new_pair_counts;
    end

    element_counts = containers.Map;
    keys_to_process = keys(pair_counts);
    for k = 1:length(keys_to_process)
        pair = keys_to_process{k};
        count = pair_counts(pair);
        if isKey(element_counts, pair(1))
            element_counts(pair(1)) = element_counts(pair(1)) + count;
        else
            element_counts(pair(1)) = count;
        end
    end
    if isKey(element_counts, template(end))
        element_counts(template(end)) = element_counts(template(end)) + 1;
    else
        element_counts(template(end)) = 1;
    end

    counts = values(element_counts);
    max_count = max(cell2mat(counts));
    min_count = min(cell2mat(counts));

    fprintf('%d\n', max_count - min_count);
end

function [template, rules] = read_input(filename)
    fid = fopen(filename, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    template = strtrim(lines{1});
    rules = containers.Map;
    for i = 2:length(lines)
        line = strtrim(lines{i});
        if ~isempty(line)
            parts = strsplit(line, ' -> ');
            rules(parts{1}) = parts{2};
        end
    end
end
