
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Failed to open input file.');
    end
    
    safeReportCount = 0;
    
    tline = fgetl(fid);
    while ischar(tline)
        levels = sscanf(tline, '%d');
        if ~isempty(levels) && isSafeReport(levels)
            safeReportCount = safeReportCount + 1;
        end
        tline = fgetl(fid);
    end
    
    fclose(fid);
    
    fprintf('%d\n', safeReportCount);
end

function safe = isSafeReport(levels)
    n = length(levels);
    if n < 2
        safe = false;
        return;
    end
    
    diffs = diff(levels);
    
    if any(diffs == 0)
        safe = false;
        return;
    end
    
    isIncreasing = diffs(1) > 0;
    
    for i = 1:length(diffs)
        if (isIncreasing && diffs(i) <= 0) || (~isIncreasing && diffs(i) >= 0)
            safe = false;
            return;
        end
        
        absDiff = abs(diffs(i));
        if absDiff < 1 || absDiff > 3
            safe = false;
            return;
        end
    end
    
    safe = true;
end

main();
