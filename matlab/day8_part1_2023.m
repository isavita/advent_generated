
function main()
    fileId = fopen("input.txt", "r");
    lines = textscan(fileId, "%s", "Delimiter", "\n");
    fclose(fileId);
    lines = lines{1};

    instructions = lines{1};
    desert_map = containers.Map('KeyType', 'char', 'ValueType', 'any');

    for i = 3:length(lines)
        line = lines{i};
        if ~isempty(line)
            matches = regexp(line, "[A-Z]{3}", "match");
            desert_map(matches{1}) = struct('left', matches{2}, 'right', matches{3});
        end
    end

    current = "AAA";
    target = "ZZZ";
    steps = 0;

    instruction_len = length(instructions);
    instruction_idx = 1;

    while ~strcmp(current, target)
        direction = instructions(instruction_idx);
        if direction == 'R'
            current = desert_map(current).right;
        else
            current = desert_map(current).left;
        end
        steps = steps + 1;
        instruction_idx = mod(instruction_idx, instruction_len) + 1;
    end

    disp(steps);
end
