
function main()
    fid = fopen('input.txt', 'r');
    grid = textscan(fid, '%s', 'Delimiter', '\n', 'WhiteSpace', ' \t');
    fclose(fid);
    grid = grid{1};

    rows = length(grid);
    cols = length(grid{1});
    
    directions = [0, 1; 1, 0; 1, 1; -1, 1; 0, -1; -1, 0; -1, -1; 1, -1];
    
    count = 0;
    word = 'XMAS';
    
    for r = 1:rows
        for c = 1:cols
            for d = 1:8
                dx = directions(d, 1);
                dy = directions(d, 2);
                
                valid = true;
                for i = 1:length(word)
                    nr = r + (i - 1) * dx;
                    nc = c + (i - 1) * dy;
                    
                    if nr < 1 || nr > rows || nc < 1 || nc > cols || grid{nr}(nc) ~= word(i)
                        valid = false;
                        break;
                    end
                end
                
                if valid
                    count = count + 1;
                end
            end
        end
    end
    
    fprintf('XMAS appears %d times in the word search\n', count);
end

main();
