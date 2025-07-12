
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file input.txt');
    end

    stacks = cell(1, 10);
    for i = 1:10
        stacks{i} = '';
    end
    num_stacks = 0;

    % Parse initial stack state
    while true
        line = fgetl(fid);
        if ~ischar(line)
            break;
        end
        if isempty(line)
            break;
        end

        if num_stacks == 0
            num_stacks = (length(line) + 1) / 4;
            stacks = cell(1, num_stacks);
            for i = 1:num_stacks
                stacks{i} = '';
            end
        end

        for i = 1:num_stacks
            char_idx = (i - 1) * 4 + 2;
            if char_idx <= length(line)
                crate_char = line(char_idx);
                if isletter(crate_char)
                    stacks{i} = [crate_char, stacks{i}];
                end
            end
        end
    end

    % Parse move commands
    while true
        line = fgetl(fid);
        if ~ischar(line)
            break;
        end

        tokens = regexp(line, 'move (\d+) from (\d+) to (\d+)', 'tokens', 'once');
        if ~isempty(tokens)
            n = str2double(tokens{1});
            from_idx = str2double(tokens{2});
            to_idx = str2double(tokens{3});

            for k = 1:n
                if ~isempty(stacks{from_idx})
                    crate = stacks{from_idx}(end);
                    stacks{from_idx} = stacks{from_idx}(1:end-1);
                    stacks{to_idx} = [stacks{to_idx}, crate];
                end
            end
        end
    end

    fclose(fid);

    result = '';
    for i = 1:num_stacks
        if ~isempty(stacks{i})
            result = [result, stacks{i}(end)];
        end
    end
    disp(result);
end
