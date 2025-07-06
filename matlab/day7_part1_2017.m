
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('File reading error');
    end
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    holderMap = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    heldMap = containers.Map('KeyType', 'char', 'ValueType', 'logical');

    for i = 1:length(lines)
        line = lines{i};
        names = regexp(line, '[a-z]+', 'match');
        holder = names{1};
        holderMap(holder) = true;

        if length(names) > 1
            for j = 2:length(names)
                heldMap(names{j}) = true;
            end
        end
    end

    keys = holderMap.keys;
    for i = 1:length(keys)
        key = keys{i};
        if ~isKey(heldMap, key)
            disp(key);
            return;
        end
    end
end
