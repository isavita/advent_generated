
function main()
    offset = 1e13;
    machines = readInput('input.txt');
    for i = 1:length(machines)
        machines(i).px = machines(i).px + offset;
        machines(i).py = machines(i).py + offset;
    end

    results = [];
    for i = 1:length(machines)
        cost = solveMachine(machines(i));
        if cost >= 0
            results = [results, cost];
        end
    end

    if isempty(results)
        fprintf('0 0\n');
    else
        count = length(results);
        sum_costs = sum(results);
        fprintf('%d %d\n', count, sum_costs);
    end
end

function machines = readInput(filename)
    fID = fopen(filename, 'r');
    if fID == -1
        error('Cannot open file: %s', filename);
    end
    
    lines = textscan(fID, '%s', 'Delimiter', '\n');
    fclose(fID);
    
    lines = lines{1};
    
    machines = struct('ax', {}, 'ay', {}, 'bx', {}, 'by', {}, 'px', {}, 'py', {});
    current_machine_lines = {};
    
    for i = 1:length(lines)
        line = strtrim(lines{i});
        if isempty(line)
            if ~isempty(current_machine_lines)
                machines(end+1) = parseMachine(current_machine_lines);
                current_machine_lines = {};
            end
        else
            current_machine_lines{end+1} = line;
        end
    end
    
    if ~isempty(current_machine_lines)
        machines(end+1) = parseMachine(current_machine_lines);
    end
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
    x = parseVal(parts{1});
    y = parseVal(parts{2});
end

function [x, y] = parsePrize(s)
    parts = strsplit(strtrim(s), ',');
    x = parseValPrize(parts{1});
    y = parseValPrize(parts{2});
end

function v = parseVal(s)
    s = strtrim(s);
    s = strrep(s, 'X+', '');
    s = strrep(s, 'Y+', '');
    s = strrep(s, 'X=', '');
    s = strrep(s, 'Y=', '');
    v = str2double(s);
end

function v = parseValPrize(s)
    s = strtrim(s);
    s = strrep(s, 'X=', '');
    s = strrep(s, 'Y=', '');
    v = str2double(s);
end

function cost = solveMachine(m)
    D = m.ax * m.by - m.ay * m.bx;
    if D == 0
        cost = -1;
        return;
    end
    
    numA = m.px * m.by - m.py * m.bx;
    numB = -m.px * m.ay + m.py * m.ax;
    
    if mod(numA, D) ~= 0 || mod(numB, D) ~= 0
        cost = -1;
        return;
    end
    
    a = numA / D;
    b = numB / D;
    
    if a < 0 || b < 0
        cost = -1;
        return;
    end
    
    cost = 3 * a + b;
end

main();
