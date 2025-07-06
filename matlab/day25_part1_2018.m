
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    
    points = [];
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        coords = strsplit(line, ',');
        points = [points; cellfun(@str2double, coords)];
    end
    fclose(fid);
    
    n = size(points, 1);
    parent = 1:n;
    
    for i = 1:n
        for j = i+1:n
            dist = sum(abs(points(i,:) - points(j,:)));
            if dist <= 3
                root_i = find_root(parent, i);
                root_j = find_root(parent, j);
                if root_i ~= root_j
                    parent(root_i) = root_j;
                end
            end
        end
    end
    
    constellation_count = 0;
    for i = 1:n
        if parent(i) == i
            constellation_count = constellation_count + 1;
        end
    end
    
    fprintf('%d\n', constellation_count);
end

function root = find_root(parent, i)
    if parent(i) ~= i
        parent(i) = find_root(parent, parent(i));
    end
    root = parent(i);
end

main();
