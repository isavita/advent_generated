
function main()
    fid = fopen('input.txt', 'r');
    grid = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    grid = grid{1};
    n = length(grid);
    m = length(grid{1});
    
    sx = 0; sy = 0; ex = 0; ey = 0;
    for i = 1:n
        for j = 1:m
            if grid{i}(j) == 'S'
                sx = i; sy = j;
            elseif grid{i}(j) == 'E'
                ex = i; ey = j;
            end
        end
    end
    
    dx = [-1, 0, 1, 0];
    dy = [0, 1, 0, -1];
    dist = inf(n, m, 4);
    dist(sx, sy, 2) = 0;
    
    h = struct('x', {}, 'y', {}, 'd', {}, 'cost', {});
    
    function push(v)
        h(end+1) = v;
        i = length(h);
        while i > 1
            p = floor((i - 1) / 2) + 1;
            if h(p).cost <= h(i).cost
                break;
            end
            h([p, i]) = h([i, p]);
            i = p;
        end
    end
    
    function v = pop()
        v = h(1);
        h(1) = h(end);
        h(end) = [];
        i = 1;
        while true
            l = 2 * i - 1;
            r = 2 * i;
            small = i;
            if l <= length(h) && h(l).cost < h(small).cost
                small = l;
            end
            if r <= length(h) && h(r).cost < h(small).cost
                small = r;
            end
            if small == i
                break;
            end
            h([i, small]) = h([small, i]);
            i = small;
        end
    end
    
    push(struct('x', sx, 'y', sy, 'd', 2, 'cost', 0));
    
    while ~isempty(h)
        u = pop();
        if dist(u.x, u.y, u.d) < u.cost
            continue;
        end
        if u.x == ex && u.y == ey
            fprintf('%d\n', u.cost);
            break;
        end
        
        for ndir_offset = [-1, 1]
            ndir = mod(u.d + ndir_offset - 1, 4) + 1;
            nc = u.cost + 1000;
            if nc < dist(u.x, u.y, ndir)
                dist(u.x, u.y, ndir) = nc;
                push(struct('x', u.x, 'y', u.y, 'd', ndir, 'cost', nc));
            end
        end
        
        nx = u.x + dx(u.d);
        ny = u.y + dy(u.d);
        
        if nx >= 1 && nx <= n && ny >= 1 && ny <= m && grid{nx}(ny) ~= '#'
            nc = u.cost + 1;
            if nc < dist(nx, ny, u.d)
                dist(nx, ny, u.d) = nc;
                push(struct('x', nx, 'y', ny, 'd', u.d, 'cost', nc));
            end
        end
    end
end

main();
