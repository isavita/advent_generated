
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    instructions = instructions{1};

    registers.a = 1;
    registers.b = 0;

    i = 1;
    while i <= length(instructions)
        parts = strsplit(instructions{i});
        op = parts{1};

        switch op
            case 'hlf'
                registers.(parts{2}) = registers.(parts{2}) / 2;
            case 'tpl'
                registers.(parts{2}) = registers.(parts{2}) * 3;
            case 'inc'
                registers.(parts{2}) = registers.(parts{2}) + 1;
            case 'jmp'
                offset = str2double(parts{2});
                i = i + offset - 1;
            case 'jie'
                reg = parts{2}(1);
                if mod(registers.(reg), 2) == 0
                    offset = str2double(parts{3});
                    i = i + offset - 1;
                end
            case 'jio'
                reg = parts{2}(1);
                if registers.(reg) == 1
                    offset = str2double(parts{3});
                    i = i + offset - 1;
                end
        end
        i = i + 1;
    end

    fprintf('%d\n', registers.b);
end
