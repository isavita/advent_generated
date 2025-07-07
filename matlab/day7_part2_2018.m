
function main()
    deps = containers.Map('KeyType', 'char', 'ValueType', 'any');
    allSteps = containers.Map('KeyType', 'char', 'ValueType', 'any');
    
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    lines = lines{1};
    
    for i = 1:length(lines)
        line = lines{i};
        if isempty(line)
            continue;
        end
        
        match = regexp(line, 'Step (\w) must be finished before step (\w) can begin.', 'tokens');
        if ~isempty(match)
            a = match{1}{1};
            b = match{1}{2};
            
            if ~isKey(deps, b)
                deps(b) = {};
            end
            deps(b) = [deps(b), {a}];
            
            if ~isKey(allSteps, a)
                allSteps(a) = struct('id', a, 'duration', a - 'A' + 61);
            end
            if ~isKey(allSteps, b)
                allSteps(b) = struct('id', b, 'duration', b - 'A' + 61);
            end
        end
    end
    
    numWorkers = 5;
    baseDuration = 60;
    
    workers = zeros(1, numWorkers);
    tasks = repmat({''}, 1, numWorkers);
    time = 0;
    
    remainingSteps = keys(allSteps);
    
    while ~isempty(remainingSteps)
        available = {};
        
        for i = 1:length(remainingSteps)
            step = remainingSteps{i};
            
            isReady = true;
            if isKey(deps, step)
                for j = 1:length(deps(step))
                    prereq = deps(step){j};
                    if ismember(prereq, remainingSteps)
                        isReady = false;
                        break;
                    end
                end
            end
            
            if isReady && ~ismember(step, tasks)
                available{end+1} = step;
            end
        end
        
        available = sort(available);
        
        for i = 1:numWorkers
            if workers(i) == 0 && ~isempty(available)
                tasks{i} = available{1};
                workers(i) = allSteps(available{1}).duration;
                available(1) = [];
            end
        end
        
        minDuration = inf;
        for i = 1:numWorkers
            if workers(i) > 0 && workers(i) < minDuration
                minDuration = workers(i);
            end
        end
        
        if isinf(minDuration)
            break;
        end
        
        for i = 1:numWorkers
            if workers(i) > 0
                workers(i) = workers(i) - minDuration;
                if workers(i) == 0
                    finishedStep = tasks{i};
                    
                    remainingSteps = remainingSteps(~ismember(remainingSteps, {finishedStep}));
                    
                    
                    keysDeps = keys(deps);
                    for k = 1:length(keysDeps)
                        currentStep = keysDeps{k};
                        if ismember(finishedStep, deps(currentStep))
                            deps(currentStep) = deps(currentStep)(~ismember(deps(currentStep), {finishedStep}));
                        end
                    end
                    
                    tasks{i} = '';
                end
            end
        end
        time = time + minDuration;
    end
    
    fprintf('%d\n', time);
end

