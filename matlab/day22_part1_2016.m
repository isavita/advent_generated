
function main
    nodes = readNodes("input.txt");
    count = countViablePairs(nodes);
    disp(count);
end

function nodes = readNodes(filename)
    fid = fopen(filename, 'r');
    nodes = struct('used', {}, 'avail', {});
    line = fgetl(fid);
    while ischar(line)
        matches = regexp(line, 'node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%', 'tokens');
        if ~isempty(matches)
            used = str2double(matches{1}{1});
            avail = str2double(matches{1}{2});
            nodes(end+1) = struct('used', used, 'avail', avail);
        end
        line = fgetl(fid);
    end
    fclose(fid);
end

function count = countViablePairs(nodes)
    n = numel(nodes);
    used_values = [nodes.used];
    avail_values = [nodes.avail];
    
    valid_indices = find(used_values > 0);
    
    count = 0;
    for i = valid_indices
        for j = 1:n
            if i ~= j && used_values(i) <= avail_values(j)
                count = count + 1;
            end
        end
    end
end

main;
