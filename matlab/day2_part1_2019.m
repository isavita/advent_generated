
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    
    line = fgetl(fid);
    fclose(fid);
    
    nums = strsplit(line, ',');
    inputData = cellfun(@str2double, nums);
    
    inputData(2) = 12;
    inputData(3) = 2;
    
    result = executeProgram(inputData);
    
    disp(result);
end

function result = executeProgram(data)
    i = 1;
    while i <= length(data) - 3
        opcode = data(i);
        
        if opcode == 99
            result = data(1);
            return;
        end
        
        pos1 = data(i+1) + 1;
        pos2 = data(i+2) + 1;
        pos3 = data(i+3) + 1;
        
        switch opcode
            case 1
                data(pos3) = data(pos1) + data(pos2);
            case 2
                data(pos3) = data(pos1) * data(pos2);
            otherwise
                error('Invalid opcode');
        end
        
        i = i + 4;
    end
    
    result = data(1);
end

main();
