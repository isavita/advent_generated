
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end
    
    content = fscanf(fid, '%c');
    fclose(fid);
    
    totalSum = 0;
    enabled = true;
    
    i = 1;
    while i <= length(content)
        if strncmp(content(i:end), 'mul(', 4)
            i = i + 4;
            num1Str = '';
            while isdigit(content(i))
                num1Str = [num1Str, content(i)];
                i = i + 1;
            end
            
            if content(i) ~= ','
                i = i + 1;
                continue;
            end
            i = i + 1;
            
            num2Str = '';
            while isdigit(content(i))
                num2Str = [num2Str, content(i)];
                i = i + 1;
            end
            
            if content(i) ~= ')'
                i = i + 1;
                continue;
            end
            
            if enabled
                num1 = str2double(num1Str);
                num2 = str2double(num2Str);
                totalSum = totalSum + num1 * num2;
            end
            i = i + 1;
            
        elseif strncmp(content(i:end), 'do()', 4)
            enabled = true;
            i = i + 4;
        elseif strncmp(content(i:end), 'don''t()', 7)
            enabled = false;
            i = i + 7;
        else
            i = i + 1;
        end
    end
    
    fprintf('%d\n', totalSum);
end

main();
