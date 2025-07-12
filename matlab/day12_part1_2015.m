
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file.');
    end
    inputStr = fscanf(fid, '%c');
    fclose(fid);

    inputStr = strrep(inputStr, '-', ' -');
    inputStr = regexprep(inputStr, '[^0-9 -]', ' ');
    numbers = str2num(inputStr);
    result = sum(numbers);

    fprintf('%d\n', result);
end

main();
