
function main()
    fileId = fopen('input.txt', 'r');
    data = strtrim(fread(fileId, '*char')');
    fclose(fileId);

    for i = 1:40
        data = lookAndSay(data);
    end

    disp(length(data));
end

function result = lookAndSay(inputString)
    result = '';
    i = 1;
    while i <= length(inputString)
        count = 1;
        while i + 1 <= length(inputString) && inputString(i) == inputString(i + 1)
            i = i + 1;
            count = count + 1;
        end
        result = [result, num2str(count), inputString(i)];
        i = i + 1;
    end
end

main();
