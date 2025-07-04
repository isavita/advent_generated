
function main()
    reports = parseInput('input.txt');
    count = countSafeReports(reports);
    disp(count);
end

function reports = parseInput(filePath)
    fid = fopen(filePath, 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    reports = cell(length(lines{1}), 1);
    for i = 1:length(lines{1})
        line = strsplit(lines{1}{i});
        reports{i} = cellfun(@str2double, line);
    end
end

function isSafe = isSafe(report)
    if isSafeSequence(report)
        isSafe = true;
        return;
    end
    
    for i = 1:length(report)
        modifiedReport = [report(1:i-1), report(i+1:end)];
        if isSafeSequence(modifiedReport)
            isSafe = true;
            return;
        end
    end
    
    isSafe = false;
end

function isSafeSeq = isSafeSequence(report)
    if isempty(report) || length(report) == 1
        isSafeSeq = true;
        return;
    end
    
    diffs = diff(report);
    
    isIncreasing = all(diffs > 0);
    isDecreasing = all(diffs < 0);
    
    if ~(isIncreasing || isDecreasing)
        isSafeSeq = false;
        return;
    end
    
    isSafeSeq = all(abs(diffs) >= 1 & abs(diffs) <= 3);
end

function count = countSafeReports(reports)
    count = 0;
    for i = 1:length(reports)
        if isSafe(reports{i})
            count = count + 1;
        end
    end
end

main();
