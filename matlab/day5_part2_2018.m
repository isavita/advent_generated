
function main()
    polymer = fileread('input.txt');
    polymer = strtrim(polymer);

    minLength = length(polymer);

    for unit_char_code = double('a'):double('z')
        unit_char = char(unit_char_code);

        tempPolymer = removeUnit(polymer, unit_char);

        currentLength = react(tempPolymer);

        if currentLength < minLength
            minLength = currentLength;
        end
    end

    fprintf('%d\n', minLength);
end

function len = react(polymer)
    stack = char(zeros(1, length(polymer)));
    stack_ptr = 0;

    for i = 1:length(polymer)
        current_char = polymer(i);

        if stack_ptr > 0
            top_char = stack(stack_ptr);
            if top_char ~= current_char && upper(top_char) == upper(current_char)
                stack_ptr = stack_ptr - 1;
            else
                stack_ptr = stack_ptr + 1;
                stack(stack_ptr) = current_char;
            end
        else
            stack_ptr = stack_ptr + 1;
            stack(stack_ptr) = current_char;
        end
    end
    len = stack_ptr;
end

function result = removeUnit(polymer, unit)
    keep_idx = (lower(polymer) ~= lower(unit));
    result = polymer(keep_idx);
end

main();
