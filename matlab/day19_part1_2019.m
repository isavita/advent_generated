
function main()
    sum = 0;
    for y = 0:49
        for x = 0:49
            if Beam(x, y)
                sum = sum + 1;
            end
        end
    end
    disp(sum);
end

function result = Beam(x, y)
    code = readCode("input.txt");
    ip = 0;
    relativeBase = 0;
    
    inputQueue = [x, y];
    outputValue = -1;
    
    while true
        opcode = mod(code(ip + 1), 100);
        
        switch opcode
            case 1
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 3);
                code(params(3) + 1) = code(params(1) + 1) + code(params(2) + 1);
                ip = ip + 3 + 1;
            case 2
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 3);
                code(params(3) + 1) = code(params(1) + 1) * code(params(2) + 1);
                ip = ip + 3 + 1;
            case 3
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 1);
                code(params(1) + 1) = inputQueue(1);
                inputQueue(1) = [];
                ip = ip + 1 + 1;
            case 4
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 1);
                outputValue = code(params(1) + 1);
                ip = ip + 1 + 1;
            case 5
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 2);
                if code(params(1) + 1) ~= 0
                    ip = code(params(2) + 1);
                else
                    ip = ip + 2 + 1;
                end
            case 6
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 2);
                if code(params(1) + 1) == 0
                    ip = code(params(2) + 1);
                else
                    ip = ip + 2 + 1;
                end
            case 7
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 3);
                if code(params(1) + 1) < code(params(2) + 1)
                    code(params(3) + 1) = 1;
                else
                    code(params(3) + 1) = 0;
                end
                ip = ip + 3 + 1;
            case 8
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 3);
                if code(params(1) + 1) == code(params(2) + 1)
                    code(params(3) + 1) = 1;
                else
                    code(params(3) + 1) = 0;
                end
                ip = ip + 3 + 1;
            case 9
                [params, ip, relativeBase] = getParamsAddresses(code, ip, relativeBase, 1);
                relativeBase = relativeBase + code(params(1) + 1);
                ip = ip + 1 + 1;
            case 99
                break;
            otherwise
                error('Unknown opcode: %d', opcode);
        end
    end
    
    result = (outputValue == 1);
end

function code = readCode(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Cannot open file: %s', filename);
    end
    line = fgetl(fid);
    fclose(fid);
    
    strValues = strsplit(strtrim(line), ',');
    code = zeros(1, length(strValues));
    for i = 1:length(strValues)
        code(i) = str2double(strValues{i});
    end
end

function [params, newIp, newRelativeBase] = getParamsAddresses(code, ip, relativeBase, arity)
    modes = zeros(1, arity);
    modeSection = floor(code(ip + 1) / 100);
    
    params = zeros(1, arity);
    newIp = ip;
    newRelativeBase = relativeBase;
    
    for i = 1:arity
        mode = mod(floor(modeSection / power(10, i-1)), 10);
        modes(i) = mode;
        
        switch mode
            case 0 % Position mode
                params(i) = code(ip + i + 1);
            case 1 % Immediate mode
                params(i) = ip + i;
            case 2 % Relative mode
                params(i) = relativeBase + code(ip + i + 1);
        end
    end
end
