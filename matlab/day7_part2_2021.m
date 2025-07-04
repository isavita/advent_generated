
function main()
    fid = fopen('input.txt', 'r');
    line = fscanf(fid, '%s');
    fclose(fid);
    positions = str2num(line);

    min_pos = min(positions);
    max_pos = max(positions);
    
    fuel_costs = zeros(1, max_pos - min_pos + 1);
    for target = min_pos:max_pos
        diffs = abs(positions - target);
        fuel_costs(target - min_pos + 1) = sum(diffs .* (diffs + 1) / 2);
    end
    
    fprintf('%d\n', min(fuel_costs));
end

main();
