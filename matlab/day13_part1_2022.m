
function main()
    % Read input from input.txt
    packets = readPackets('input.txt');

    % Calculate the sum of indices of pairs in the right order
    sumOfIndices = calculateSumOfOrderedIndices(packets);

    % Print the result to standard output
    fprintf('Sum of indices of pairs in the right order: %d\n', sumOfIndices);
end

function packets = readPackets(filename)
    % Reads packet pairs from a file.
    % Each pair is separated by a blank line.
    % Returns a cell array where each cell contains a pair of packets.
    
    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end

    packets = {};
    currentPair = {};
    line = fgetl(fid);

    while ischar(line)
        if isempty(line)
            % Blank line signifies the end of a pair
            if ~isempty(currentPair)
                packets{end+1} = currentPair;
                currentPair = {};
            end
        else
            % Parse the packet string into a MATLAB data structure
            currentPair{end+1} = parsePacket(line);
        end
        line = fgetl(fid);
    end

    % Add the last pair if the file doesn't end with a blank line
    if ~isempty(currentPair)
        packets{end+1} = currentPair;
    end

    fclose(fid);
end

function packetData = parsePacket(packetString)
    % Parses a packet string into a MATLAB data structure (nested cells and numbers).
    % This is a recursive function.
    
    % Remove outer brackets if they exist and are not part of nested structures
    if startsWith(packetString, '[') && endsWith(packetString, ']')
        packetString = packetString(2:end-1);
    end

    packetData = {};
    currentElement = '';
    bracketDepth = 0;
    
    for i = 1:length(packetString)
        char = packetString(i);
        
        if char == '['
            bracketDepth = bracketDepth + 1;
            currentElement = [currentElement, char];
        elseif char == ']'
            bracketDepth = bracketDepth - 1;
            currentElement = [currentElement, char];
            if bracketDepth == 0
                % End of a nested list
                packetData{end+1} = parsePacket(currentElement);
                currentElement = '';
            end
        elseif char == ',' && bracketDepth == 0
            % Comma at the top level, signifies separation of elements
            if ~isempty(currentElement)
                if isstrprop(currentElement(1), 'digit')
                    packetData{end+1} = str2double(currentElement);
                else
                    % This case should ideally be handled by the bracket depth logic
                    % but as a fallback, parse if it's a string that looks like a packet
                    packetData{end+1} = parsePacket(currentElement);
                end
                currentElement = '';
            end
        else
            currentElement = [currentElement, char];
        end
    end
    
    % Add the last element if it exists
    if ~isempty(currentElement)
        if isstrprop(currentElement(1), 'digit')
            packetData{end+1} = str2double(currentElement);
        else
            packetData{end+1} = parsePacket(currentElement);
        end
    end
end

function sumOfIndices = calculateSumOfOrderedIndices(packets)
    % Calculates the sum of indices of packet pairs that are in the right order.
    sumOfIndices = 0;
    for i = 1:length(packets)
        pair = packets{i};
        left = pair{1};
        right = pair{2};
        
        if isPacketInOrder(left, right)
            sumOfIndices = sumOfIndices + i;
        end
    end
end

function result = isPacketInOrder(left, right)
    % Compares two packets (which can be integers or nested lists)
    % and returns true if they are in the right order, false otherwise.
    % This is the core comparison logic.
    
    % Case 1: Both are integers
    if isnumeric(left) && isnumeric(right)
        if left < right
            result = true;
        elseif left > right
            result = false;
        else
            result = -1; % Indicate they are equal, continue comparison
        end
        return;
    end
    
    % Case 2: Both are lists (MATLAB cells)
    if iscell(left) && iscell(right)
        lenLeft = length(left);
        lenRight = length(right);
        minLen = min(lenLeft, lenRight);
        
        for i = 1:minLen
            subResult = isPacketInOrder(left{i}, right{i});
            if subResult ~= -1 % If a decision was made
                result = subResult;
                return;
            end
        end
        
        % If all common elements were equal
        if lenLeft < lenRight
            result = true; % Left list ran out first
        elseif lenLeft > lenRight
            result = false; % Right list ran out first
        else
            result = -1; % Lists are identical, continue comparison
        end
        return;
    end
    
    % Case 3: One is an integer, the other is a list
    if isnumeric(left) && iscell(right)
        % Convert integer to a list
        left = {left};
        result = isPacketInOrder(left, right);
        return;
    end
    
    if iscell(left) && isnumeric(right)
        % Convert integer to a list
        right = {right};
        result = isPacketInOrder(left, right);
        return;
    end
    
    % If we reach here, it means the comparison is inconclusive (-1)
    % or an unexpected type was encountered.
    % For the purpose of this problem, if it's not explicitly ordered,
    % we assume it's not in order if the lengths differ and no other
    % comparison made a decision. However, the logic above should cover all cases.
    % If both are empty lists, they are equal.
    if isempty(left) && isempty(right)
        result = -1;
        return;
    end
    
    % Fallback for unexpected scenarios, though the logic should be exhaustive.
    % If no decision is made, it implies equality for the current level.
    result = -1; 
end

% Entry point for the program
if nargin == 0
    main();
end
