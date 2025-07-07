
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%d');
    fclose(fid);

    target = floor(data / 11);
    houses = zeros(1, target + 1);

    for elf = 1:target
        for house = elf:elf:min(elf * 50, target)
            houses(house + 1) = houses(house + 1) + elf;
        end
    end

    for houseNumber = 0:target
        if houses(houseNumber + 1) >= target
            disp(houseNumber);
            break;
        end
    end
end

main();
