
function main()
    fid = fopen('input.txt', 'r');
    instructions = textscan(fid, '%s');
    fclose(fid);
    
    code = getBathroomCode(instructions{1});
    disp(code);
end

function code = getBathroomCode(instructions)
    keypad = [1, 2, 3; 4, 5, 6; 7, 8, 9];
    x = 2;
    y = 2;
    code = '';
    
    for i = 1:length(instructions)
        instruction = instructions{i};
        for j = 1:length(instruction)
            move = instruction(j);
            switch move
                case 'U'
                    if x > 1
                        x = x - 1;
                    end
                case 'D'
                    if x < 3
                        x = x + 1;
                    end
                case 'L'
                    if y > 1
                        y = y - 1;
                    end
                case 'R'
                    if y < 3
                        y = y + 1;
                    end
            end
        end
        code = [code, num2str(keypad(x, y))];
    end
end

main();
