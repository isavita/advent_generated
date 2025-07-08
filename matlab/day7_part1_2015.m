
function main()
    clear get_signal;

    instructionsMap = containers.Map;

    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Cannot open input.txt');
    end

    while ~feof(fid)
        line = fgetl(fid);
        if isempty(line), continue; end

        parts = strsplit(line, ' -> ');
        targetWire = parts{2};
        expr = parts{1};

        instrStruct = struct();
        instrStruct.target = targetWire;

        exprParts = strsplit(expr, ' ');
        numExprParts = length(exprParts);

        if numExprParts == 1
            instrStruct.op = 'ASSIGN';
            instrStruct.op1 = exprParts{1};
        elseif numExprParts == 2
            instrStruct.op = 'NOT';
            instrStruct.op1 = exprParts{2};
        elseif numExprParts == 3
            instrStruct.op1 = exprParts{1};
            instrStruct.op = exprParts{2};
            instrStruct.op2 = exprParts{3};
        else
            error('Unexpected instruction format: %s', line);
        end
        instructionsMap(targetWire) = instrStruct;
    end
    fclose(fid);

    answer = get_signal('a', instructionsMap);
    fprintf('%d\n', answer);
end

function signal = get_signal(wireName, instructionsMap)
    persistent wiresMap;

    if isempty(wiresMap)
        wiresMap = containers.Map;
    end

    numVal = str2double(wireName);
    if ~isnan(numVal)
        signal = numVal;
        return;
    end

    if wiresMap.isKey(wireName)
        signal = wiresMap(wireName);
        return;
    end

    if ~instructionsMap.isKey(wireName)
        error('Instruction for wire %s not found.', wireName);
    end
    foundInstr = instructionsMap(wireName);

    op = foundInstr.op;
    
    switch op
        case 'ASSIGN'
            signal = get_signal(foundInstr.op1, instructionsMap);
        case 'AND'
            sig1 = get_signal(foundInstr.op1, instructionsMap);
            sig2 = get_signal(foundInstr.op2, instructionsMap);
            signal = bitand(sig1, sig2);
        case 'OR'
            sig1 = get_signal(foundInstr.op1, instructionsMap);
            sig2 = get_signal(foundInstr.op2, instructionsMap);
            signal = bitor(sig1, sig2);
        case 'LSHIFT'
            sig1 = get_signal(foundInstr.op1, instructionsMap);
            shiftVal = str2double(foundInstr.op2);
            signal = bitshift(sig1, shiftVal);
        case 'RSHIFT'
            sig1 = get_signal(foundInstr.op1, instructionsMap);
            shiftVal = str2double(foundInstr.op2);
            signal = bitshift(sig1, -shiftVal);
        case 'NOT'
            sig1 = get_signal(foundInstr.op1, instructionsMap);
            signal = bitcmp(sig1, 16);
        otherwise
            error('Unknown operation: %s', op);
    end

    wiresMap(wireName) = signal;
end
