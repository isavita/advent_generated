
function main()
    file = fopen('input.txt', 'r');
    replacements = {};
    molecule = '';
    while ~feof(file)
        line = strtrim(fgets(file));
        if isempty(line)
            continue;
        end
        parts = strsplit(line, ' => ');
        if numel(parts) == 2
            replacements{end+1} = parts;
        else
            molecule = line;
        end
    end
    fclose(file);

    generated_molecules = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    for i = 1:numel(replacements)
        from_atom = replacements{i}{1};
        to_atom = replacements{i}{2};
        for j = 1:length(molecule)
            if strncmp(molecule(j:end), from_atom, length(from_atom))
                new_molecule = [molecule(1:j-1), to_atom, molecule(j+length(from_atom):end)];
                generated_molecules(new_molecule) = true;
            end
        end
    end

    disp(generated_molecules.Count);
end

main();
