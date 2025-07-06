
function main()
    fid = fopen('input.txt', 'r');
    numbers = fscanf(fid, '%d');
    fclose(fid);

    preambleLength = 25;

    for i = preambleLength + 1:length(numbers)
        if ~isValid(numbers(i), numbers(i-preambleLength:i-1))
            fprintf('%d\n', numbers(i));
            break;
        end
    end
end

function valid = isValid(number, previousNumbers)
    valid = false;
    seen = containers.Map('KeyType', 'double', 'ValueType', 'logical');
    for i = 1:length(previousNumbers)
        complement = number - previousNumbers(i);
        if isKey(seen, complement)
            valid = true;
            return;
        end
        seen(previousNumbers(i)) = true;
    end
end

main();
