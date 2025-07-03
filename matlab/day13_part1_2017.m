
function main
    file_id = fopen('input.txt', 'r');
    firewall = containers.Map('KeyType', 'double', 'ValueType', 'any');
    max_depth = 0;

    while ~feof(file_id)
        line = fgetl(file_id);
        parts = strsplit(line, ': ');
        depth = str2double(parts{1});
        rng = str2double(parts{2});
        firewall(depth) = struct('Range', rng, 'Position', 0, 'Direction', 1);
        if depth > max_depth
            max_depth = depth;
        end
    end
    fclose(file_id);

    severity = 0;
    scanner_keys = keys(firewall);

    for depth = 0:max_depth
        if isKey(firewall, depth)
            if firewall(depth).Position == 0
                severity = severity + depth * firewall(depth).Range;
            end
        end

        for i = 1:length(scanner_keys)
            key = scanner_keys{i};
            scanner = firewall(key);
            if scanner.Position == 0
                scanner.Direction = 1;
            elseif scanner.Position == scanner.Range - 1
                scanner.Direction = -1;
            end
            scanner.Position = scanner.Position + scanner.Direction;
            firewall(key) = scanner;
        end
    end

    fprintf('%d\n', severity);
end
