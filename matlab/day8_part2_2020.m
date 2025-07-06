
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s %d');
    fclose(fid);

    ops = instructions{1};
    args = instructions{2};

    for i = 1:length(ops)
        if strcmp(ops{i}, 'acc')
            continue;
        end

        modifiedOps = ops;
        modifiedArgs = args;

        if strcmp(ops{i}, 'jmp')
            modifiedOps{i} = 'nop';
        else
            modifiedOps{i} = 'jmp';
        end

        [accumulator, terminated] = executeBootCode(modifiedOps, modifiedArgs);
        if terminated
            fprintf('%d\n', accumulator);
            return;
        end
    end
end

function [accumulator, terminated] = executeBootCode(ops, args)
    accumulator = 0;
    visited = false(length(ops), 1);
    currentInstruction = 1;

    while currentInstruction >= 1 && currentInstruction <= length(ops)
        if visited(currentInstruction)
            terminated = false;
            return;
        end

        visited(currentInstruction) = true;

        switch ops{currentInstruction}
            case 'acc'
                accumulator = accumulator + args(currentInstruction);
                currentInstruction = currentInstruction + 1;
            case 'jmp'
                currentInstruction = currentInstruction + args(currentInstruction);
            case 'nop'
                currentInstruction = currentInstruction + 1;
        end
    end

    terminated = true;
end

main();
