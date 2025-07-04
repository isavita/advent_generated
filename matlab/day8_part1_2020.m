
function main()
    accumulator = 0;
    index = 0;
    visited = containers.Map('KeyType', 'double', 'ValueType', 'logical');

    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s %d');
    fclose(fid);

    instruction_list = instructions{1};
    argument_list = instructions{2};

    while ~isKey(visited, index)
        visited(index) = true;
        instruction = instruction_list{index + 1};
        argument = argument_list(index + 1);

        switch instruction
            case 'acc'
                accumulator = accumulator + argument;
                index = index + 1;
            case 'jmp'
                index = index + argument;
            case 'nop'
                index = index + 1;
        end
    end

    disp(accumulator);
end
