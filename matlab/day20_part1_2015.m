
function main
    fid = fopen('input.txt', 'r');
    inputStr = fscanf(fid, '%s');
    fclose(fid);
    target = str2double(inputStr) / 10;

    houses = zeros(1, target);
    for elf = 1:target
        houses(elf:elf:target) = houses(elf:elf:target) + elf;
    end

    houseNumber = find(houses >= target, 1);
    fprintf('%d\n', houseNumber);
end
