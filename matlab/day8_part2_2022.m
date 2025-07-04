
function main()
    fid = fopen("input.txt", "r");
    lines = textscan(fid, "%s", "Delimiter", "\n");
    fclose(fid);
    lines = lines{1};

    grid = zeros(length(lines), length(lines{1}));
    for y = 1:length(lines)
        for x = 1:length(lines{y})
            grid(y, x) = str2double(lines{y}(x));
        end
    end

    [rows, cols] = size(grid);
    max_score = 0;

    for r = 1:rows
        for c = 1:cols
            current_height = grid(r, c);
            score = 1;

            directions = [-1, 1, 0, 0; 0, 0, -1, 1];

            for d = 1:4
                dr = directions(1, d);
                dc = directions(2, d);
                view = 0;
                nr = r + dr;
                nc = c + dc;

                while nr >= 1 && nr <= rows && nc >= 1 && nc <= cols
                    view = view + 1;
                    if grid(nr, nc) >= current_height
                        break;
                    end
                    nr = nr + dr;
                    nc = nc + dc;
                end
                score = score * view;
            end

            if score > max_score
                max_score = score;
            end
        end
    end

    disp(max_score);
end

main();
