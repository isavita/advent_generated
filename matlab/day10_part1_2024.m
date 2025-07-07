
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    lines = lines{1};

    nr = length(lines);
    nc = length(lines{1});
    grid = zeros(nr, nc);
    for i = 1:nr
        for j = 1:nc
            grid(i, j) = str2double(lines{i}(j));
        end
    end

    trailheads = [];
    for r = 1:nr
        for c = 1:nc
            if grid(r, c) == 0
                trailheads = [trailheads; r, c];
            end
        end
    end

    sumScores = 0;
    dirs = [1, 0; -1, 0; 0, 1; 0, -1];

    for k = 1:size(trailheads, 1)
        th = trailheads(k, :);
        reached = containers.Map('KeyType', 'char', 'ValueType', 'logical');
        front = struct('p', struct('r', th(1), 'c', th(2)), 'h', 0);
        visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');

        while ~isempty(front)
            cur = front(end);
            front = front(1:end-1);

            if cur.h == 9
                key = sprintf('%d,%d', cur.p.r, cur.p.c);
                if ~isKey(reached, key)
                    reached(key) = true;
                end
                continue;
            end

            for i = 1:size(dirs, 1)
                nr2 = cur.p.r + dirs(i, 1);
                nc2 = cur.p.c + dirs(i, 2);

                if nr2 >= 1 && nr2 <= nr && nc2 >= 1 && nc2 <= nc
                    if grid(nr2, nc2) == cur.h + 1
                        key = sprintf('%d,%d,%d', nr2, nc2, cur.h + 1);
                        if ~isKey(visited, key)
                            visited(key) = true;
                            front = [front, struct('p', struct('r', nr2, 'c', nc2), 'h', cur.h + 1)];
                        end
                    end
                end
            end
        end
        sumScores = sumScores + length(reached);
    end

    fprintf('%d\n', sumScores);
end
