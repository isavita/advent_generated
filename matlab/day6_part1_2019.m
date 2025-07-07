
function main()
    fid = fopen('input.txt', 'r');
    orbits = textscan(fid, '%s%s', 'Delimiter', ')');
    fclose(fid);

    orbitMap = containers.Map('KeyType', 'char', 'ValueType', 'char');
    for i = 1:length(orbits{1})
        orbitMap(orbits{2}{i}) = orbits{1}{i};
    end

    totalOrbits = 0;
    keys = orbitMap.keys;
    for i = 1:length(keys)
        current = keys{i};
        while isKey(orbitMap, current)
            current = orbitMap(current);
            totalOrbits = totalOrbits + 1;
        end
    end

    disp(totalOrbits);
end

main();
