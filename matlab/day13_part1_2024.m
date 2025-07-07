
function main()
    machines = readInput('input.txt');
    results = [];
    for i = 1:length(machines)
        cost = solveMachine(machines(i));
        if cost >= 0
            results = [results, cost];
        end
    end

    if isempty(results)
        fprintf('0 0\n');
        return;
    end

    count = length(results);
    totalSum = sum(results);
    fprintf('%d %d\n', count, totalSum);
end

function machines = readInput(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Cannot open file: %s', filename);
    end
    
    machines = struct('ax', {}, 'ay', {}, 'bx', {}, 'by', {}, 'px', {}, 'py', {});
    currentMachineLines = {};
    
    tline = fgetl(fid);
    while ischar(tline)
        tline = strtrim(tline);
        if isempty(tline)
            if ~isempty(currentMachineLines)
                machines = [machines, parseMachine(currentMachineLines)];
                currentMachineLines = {};
            end
        else
            currentMachineLines{end+1} = tline;
        end
        tline = fgetl(fid);
    end
    
    if ~isempty(currentMachineLines)
        machines = [machines, parseMachine(currentMachineLines)];
    end
    
    fclose(fid);
end

function m = parseMachine(lines)
    m = struct('ax', 0, 'ay', 0, 'bx', 0, 'by', 0, 'px', 0, 'py', 0);
    for i = 1:length(lines)
        line = lines{i};
        line = strrep(line, 'Button A:', 'A:');
        line = strrep(line, 'Button B:', 'B:');
        line = strrep(line, 'Prize:', 'P:');
        
        if startsWith(line, 'A:')
            [m.ax, m.ay] = parseLine(line(3:end));
        elseif startsWith(line, 'B:')
            [m.bx, m.by] = parseLine(line(3:end));
        elseif startsWith(line, 'P:')
            [m.px, m.py] = parsePrize(line(3:end));
        end
    end
end

function [x, y] = parseLine(s)
    parts = strsplit(strtrim(s), ',');
    xp = strtrim(parts{1});
    yp = strtrim(parts{2});
    x = parseVal(xp);
    y = parseVal(yp);
end

function [x, y] = parsePrize(s)
    parts = strsplit(strtrim(s), ',');
    xp = strtrim(parts{1});
    yp = strtrim(parts{2});
    x = parseValPrize(xp);
    y = parseValPrize(yp);
end

function val = parseVal(s)
    s = strtrim(s);
    s = strrep(s, 'X+', '');
    s = strrep(s, 'Y+', '');
    s = strrep(s, 'X=', '');
    s = strrep(s, 'Y=', '');
    val = str2double(s);
end

function val = parseValPrize(s)
    s = strtrim(s);
    s = strrep(s, 'X=', '');
    s = strrep(s, 'Y=', '');
    val = str2double(s);
end

function minCost = solveMachine(m)
    minCost = -1;
    
    for aCount = 0:100
        for bCount = 0:100
            x = m.ax * aCount + m.bx * bCount;
            y = m.ay * aCount + m.by * bCount;
            
            if x == m.px && y == m.py
                cost = aCount * 3 + bCount;
                if minCost == -1 || cost < minCost
                    minCost = cost;
                end
            end
        end
    end
end

% Call the main function to start execution
main();
