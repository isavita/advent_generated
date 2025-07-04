
function main()
    file_id = fopen("input.txt", "r");
    fish_str = fscanf(file_id, "%s");
    fclose(file_id);

    fish_list = strsplit(fish_str, ",");
    fishes = zeros(1, 9);
    for i = 1:length(fish_list)
        fish = str2double(fish_list{i});
        fishes(fish + 1) = fishes(fish + 1) + 1;
    end

    for day = 1:80
        new_fish = fishes(1);
        fishes(1:8) = fishes(2:9);
        fishes(7) = fishes(7) + new_fish;
        fishes(9) = new_fish;
    end

    total_fish = sum(fishes);
    disp(total_fish);
end

main();
