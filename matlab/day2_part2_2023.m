
function main()
    total_power = 0;
    file_id = fopen("input.txt", "r");
    
    while ~feof(file_id)
        line = strtrim(fgets(file_id));
        
        game_match = regexp(line, 'Game (\d+): (.+)', 'tokens');
        
        if ~isempty(game_match)
            rounds_str = game_match{1}{2};
            rounds = strsplit(rounds_str, ';');
            
            max_red = 0;
            max_green = 0;
            max_blue = 0;
            
            for i = 1:length(rounds)
                round_str = rounds{i};
                cubes_matches = regexp(round_str, '(\d+) (red|green|blue)', 'tokens');
                
                red = 0;
                green = 0;
                blue = 0;
                
                for j = 1:length(cubes_matches)
                    count = str2double(cubes_matches{j}{1});
                    color = cubes_matches{j}{2};
                    
                    if strcmp(color, "red")
                        red = red + count;
                    elseif strcmp(color, "green")
                        green = green + count;
                    elseif strcmp(color, "blue")
                        blue = blue + count;
                    end
                end
                
                max_red = max(max_red, red);
                max_green = max(max_green, green);
                max_blue = max(max_blue, blue);
            end
            
            power = max_red * max_green * max_blue;
            total_power = total_power + power;
        end
    end
    
    fclose(file_id);
    disp(total_power);
end

main();
