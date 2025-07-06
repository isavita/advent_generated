
function main()
    fid = fopen('input.txt', 'r');
    programStr = fscanf(fid, '%s');
    fclose(fid);

    program = str2num(['[', programStr, ']']);

    input = 5;
    output = 0;
    i = 1;

    while true
        opcode = mod(program(i), 100);
        modes = floor(program(i) / 100);
        param1Mode = mod(modes, 10);
        modes = floor(modes / 10);
        param2Mode = mod(modes, 10);

        switch opcode
            case 1
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                p3 = program(i + 3) + 1;
                program(p3) = p1 + p2;
                i = i + 4;
            case 2
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                p3 = program(i + 3) + 1;
                program(p3) = p1 * p2;
                i = i + 4;
            case 3
                program(program(i + 1) + 1) = input;
                i = i + 2;
            case 4
                output = getValue(program, i + 1, param1Mode);
                disp(output);
                i = i + 2;
            case 5
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                if p1 ~= 0
                    i = p2 + 1;
                else
                    i = i + 3;
                end
            case 6
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                if p1 == 0
                    i = p2 + 1;
                else
                    i = i + 3;
                end
            case 7
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                p3 = program(i + 3) + 1;
                if p1 < p2
                    program(p3) = 1;
                else
                    program(p3) = 0;
                end
                i = i + 4;
            case 8
                p1 = getValue(program, i + 1, param1Mode);
                p2 = getValue(program, i + 2, param2Mode);
                p3 = program(i + 3) + 1;
                if p1 == p2
                    program(p3) = 1;
                else
                    program(p3) = 0;
                end
                i = i + 4;
            case 99
                return
            otherwise
                error('Invalid opcode');
        end
    end
end

function val = getValue(program, pos, mode)
    if mode == 0
        val = program(program(pos) + 1);
    else
        val = program(pos);
    end
end

main();
