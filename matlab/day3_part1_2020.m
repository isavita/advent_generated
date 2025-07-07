
function main()
    fid = fopen('input.txt', 'r');
    map = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    map = map{1};

    trees = countTrees(map, 3, 1);
    disp(trees);
end

function trees = countTrees(map, right, down)
    trees = 0;
    width = length(map{1});
    row = 1;
    col = 1;
    while row <= length(map)
        if map{row}(col) == '#'
            trees = trees + 1;
        end
        row = row + down;
        col = mod(col + right - 1, width) + 1;
    end
end

main();
