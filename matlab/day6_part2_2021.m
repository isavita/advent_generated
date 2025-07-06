
function main()
    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);

    fishAges = strsplit(line, ',');
    lanternFishCounts = zeros(1, 9);

    for i = 1:length(fishAges)
        age = str2double(fishAges{i});
        lanternFishCounts(age + 1) = lanternFishCounts(age + 1) + 1;
    end

    for day = 1:256
        newLanternFish = lanternFishCounts(1);
        lanternFishCounts(1:8) = lanternFishCounts(2:9);
        lanternFishCounts(7) = lanternFishCounts(7) + newLanternFish;
        lanternFishCounts(9) = newLanternFish;
    end

    fprintf('%d\n', sum(lanternFishCounts));
end

main();
