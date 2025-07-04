
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};

    blackTiles = containers.Map('KeyType', 'char', 'ValueType', 'logical');

    for i = 1:length(instructions)
        instruction = instructions{i};
        x = 0;
        y = 0;
        j = 1;
        while j <= length(instruction)
            if instruction(j) == 'e'
                x = x + 1;
                j = j + 1;
            elseif instruction(j) == 'w'
                x = x - 1;
                j = j + 1;
            elseif j + 1 <= length(instruction) && instruction(j:j+1) == "se"
                y = y - 1;
                j = j + 2;
            elseif j + 1 <= length(instruction) && instruction(j:j+1) == "sw"
                x = x - 1;
                y = y - 1;
                j = j + 2;
            elseif j + 1 <= length(instruction) && instruction(j:j+1) == "ne"
                x = x + 1;
                y = y + 1;
                j = j + 2;
            elseif j + 1 <= length(instruction) && instruction(j:j+1) == "nw"
                y = y + 1;
                j = j + 2;
            end
        end
        
        key = sprintf('%d,%d', x, y);
        if isKey(blackTiles, key)
            remove(blackTiles, key);
        else
            blackTiles(key) = true;
        end
    end

    disp(length(keys(blackTiles)));
end

main();
