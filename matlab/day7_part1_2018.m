
function main()
    [deps, allSteps] = parseInput("input.txt");
    order = topologicalSort(deps, allSteps);
    disp(order);
end

function [deps, allSteps] = parseInput(filename)
    fid = fopen(filename, 'r');
    if fid == -1
        error('Cannot open file: %s', filename);
    end
    
    deps = containers.Map('KeyType', 'char', 'ValueType', 'char');
    allSteps = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    
    tline = fgetl(fid);
    while ischar(tline)
        matches = regexp(tline, 'Step (\w) must be finished before step (\w) can begin.', 'tokens');
        if ~isempty(matches)
            a = matches{1}{1};
            b = matches{1}{2};
            
            if isKey(deps, b)
                deps(b) = [deps(b), a];
            else
                deps(b) = a;
            end
            
            allSteps(a) = true;
            allSteps(b) = true;
        end
        tline = fgetl(fid);
    end
    fclose(fid);
end

function order = topologicalSort(deps, allSteps)
    order = '';
    available = {};
    
    stepKeys = keys(allSteps);
    for i = 1:length(stepKeys)
        step = stepKeys{i};
        if ~isKey(deps, step) || isempty(deps(step))
            available{end+1} = step;
        end
    end
    available = sort(available);
    
    while ~isempty(available)
        next = available{1};
        available(1) = [];
        order = [order, next];
        
        stepKeys = keys(allSteps);
        for i = 1:length(stepKeys)
            step = stepKeys{i};
            if isKey(deps, step)
                currentDeps = deps(step);
                if ~isempty(currentDeps) && any(currentDeps == next)
                    deps(step) = currentDeps(currentDeps ~= next);
                    if isempty(deps(step))
                        available{end+1} = step;
                    end
                end
            end
        end
        available = sort(available);
    end
end

main();
