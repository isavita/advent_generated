
function main()
    fid = fopen('input.txt', 'r');
    inputLines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    input = inputLines{1};

    h = length(input);
    w = length(input{1});
    antennas = containers.Map('KeyType', 'char', 'ValueType', 'any');

    for y = 1:h
        for x = 1:w
            c = input{y}(x);
            if c ~= '.'
                if ~isKey(antennas, c)
                    antennas(c) = [];
                end
                antennas(c) = [antennas(c); [y, x]];
            end
        end
    end

    antinodes = containers.Map('KeyType', 'char', 'ValueType', 'logical');

    keys = antennas.keys;
    for k = 1:length(keys)
        key = keys{k};
        coords = antennas(key);
        n = size(coords, 1);
        for i = 1:n
            for j = i+1:n
                a0 = coords(i, 1);
                a1 = coords(i, 2);
                b0 = coords(j, 1);
                b1 = coords(j, 2);

                p1_y = 2 * a0 - b0;
                p1_x = 2 * a1 - b1;
                p2_y = 2 * b0 - a0;
                p2_x = 2 * b1 - a1;

                if p1_y >= 1 && p1_y <= h && p1_x >= 1 && p1_x <= w
                    antinodes([num2str(p1_y), ',', num2str(p1_x)]) = true;
                end
                if p2_y >= 1 && p2_y <= h && p2_x >= 1 && p2_x <= w
                    antinodes([num2str(p2_y), ',', num2str(p2_x)]) = true;
                end
            end
        end
    end

    disp(antinodes.Count);
end

main();
