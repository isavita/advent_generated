
function main()
    fid = fopen('input.txt', 'r');
    line = fgetl(fid);
    fclose(fid);

    positions = str2num(strrep(line, ',', ' '));

    min_fuel = intmax('int32');
    for i = min(positions):max(positions)
        fuel = sum(abs(positions - i));
        if fuel < min_fuel
            min_fuel = fuel;
        end
    end
    disp(min_fuel);
end

main();
