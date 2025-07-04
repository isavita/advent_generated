
function main()
    fid = fopen('input.txt', 'r');
    patterns_str = strsplit(fgetl(fid), ',');
    patterns = cellfun(@(x) strtrim(x), patterns_str, 'UniformOutput', false);
    fgetl(fid);
    
    count = 0;
    while ~feof(fid)
        design = strtrim(fgetl(fid));
        if canMake(design, patterns)
            count = count + 1;
        end
    end
    fclose(fid);
    disp(count);
end

function result = canMake(design, patterns)
    n = length(design);
    dp = false(1, n + 1);
    dp(1) = true;
    
    for i = 1:n
        for p = patterns
            pattern_str = p{1};
            lp = length(pattern_str);
            if i >= lp && dp(i - lp + 1) && strcmp(design(i - lp + 1:i), pattern_str)
                dp(i + 1) = true;
                break;
            end
        end
    end
    result = dp(n + 1);
end

main();
