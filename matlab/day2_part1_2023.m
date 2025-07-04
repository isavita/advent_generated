
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    data = data{1};

    target_cubes = containers.Map({'red', 'green', 'blue'}, {12, 13, 14});
    sum_of_ids = 0;

    for i = 1:length(data)
        game_str = data{i};
        game_id_match = regexp(game_str, '\d+', 'match');
        game_id = str2double(game_id_match{1});

        cubes_str = regexp(game_str, '\d+ [a-z]+', 'match');
        
        game_possible = true;
        for j = 1:length(cubes_str)
            cube_parts = strsplit(cubes_str{j});
            count = str2double(cube_parts{1});
            color = cube_parts{2};
            
            if target_cubes(color) < count
                game_possible = false;
                break;
            end
        end
        
        if game_possible
            sum_of_ids = sum_of_ids + game_id;
        end
    end

    disp(sum_of_ids);
end

main();
