
function solve()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end

    line1 = fgetl(fid);
    patterns_str = strsplit(line1, ',');
    available_patterns = cell(1, numel(patterns_str));
    for k = 1:numel(patterns_str)
        available_patterns{k} = strtrim(patterns_str{k});
    end

    fgetl(fid); % Discard the second line

    total_ways = 0;

    while ~feof(fid)
        design = fgetl(fid);
        if ischar(design)
            design = strtrim(design);
            if ~isempty(design)
                total_ways = total_ways + count_ways(design, available_patterns);
            end
        end
    end

    fclose(fid);
    fprintf('%d\n', total_ways);
end

function ways = count_ways(design, patterns)
    n = length(design);
    dp = zeros(1, n + 1);
    dp(1) = 1;

    pattern_lengths = zeros(1, numel(patterns));
    for k = 1:numel(patterns)
        pattern_lengths(k) = length(patterns{k});
    end

    for i = 1:n
        current_dp_idx = i + 1;
        for p_idx = 1:numel(patterns)
            p = patterns{p_idx};
            lp = pattern_lengths(p_idx);

            if i >= lp
                sub_design = design((i - lp) + 1 : i);
                if strcmp(sub_design, p)
                    dp(current_dp_idx) = dp(current_dp_idx) + dp((i - lp) + 1);
                end
            end
        end
    end
    ways = dp(n + 1);
end

solve();
