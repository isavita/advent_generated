
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        fprintf('0\n');
        return;
    end
    
    data = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    
    lines = data{1};
    if isempty(lines)
        fprintf('0\n');
        return;
    end
    
    grid = char(lines);
    [n, m] = size(grid);
    
    [sr, sc] = find(grid == 'S', 1);
    [er, ec] = find(grid == 'E', 1);
    
    if isempty(sr) || isempty(er)
        fprintf('0\n');
        return;
    end
    
    dr = [-1, 0, 1, 0];
    dc = [0, 1, 0, -1];
    
    dist = ones(n, m, 4) * inf;
    
    heap = zeros(n * m * 4, 4);
    heap_size = 0;
    
    start_dir = 2;
    dist(sr, sc, start_dir) = 0;
    
    heap_size = 1;
    heap(1, :) = [0, sr, sc, start_dir];
    
    while heap_size > 0
        current = heap(1, :);
        heap(1, :) = heap(heap_size, :);
        heap_size = heap_size - 1;
        
        idx = 1;
        while true
            child1 = 2 * idx;
            child2 = 2 * idx + 1;
            smallest = idx;
            
            if child1 <= heap_size && heap(child1, 1) < heap(smallest, 1)
                smallest = child1;
            end
            if child2 <= heap_size && heap(child2, 1) < heap(smallest, 1)
                smallest = child2;
            end
            
            if smallest ~= idx
                heap([idx, smallest], :) = heap([smallest, idx], :);
                idx = smallest;
            else
                break;
            end
        end
        
        cost = current(1);
        r = current(2);
        c = current(3);
        d = current(4);
        
        if cost > dist(r, c, d)
            continue;
        end
        
        nd_r = mod(d, 4) + 1;
        nd_l = mod(d + 2, 4) + 1;
        
        for nd = [nd_r, nd_l]
            nc = cost + 1000;
            if nc < dist(r, c, nd)
                dist(r, c, nd) = nc;
                heap_size = heap_size + 1;
                heap(heap_size, :) = [nc, r, c, nd];
                idx = heap_size;
                while idx > 1
                    parent_idx = floor(idx / 2);
                    if heap(idx, 1) < heap(parent_idx, 1)
                        heap([idx, parent_idx], :) = heap([parent_idx, idx], :);
                        idx = parent_idx;
                    else
                        break;
                    end
                end
            end
        end
        
        nr = r + dr(d);
        nc = c + dc(d);
        
        if nr >= 1 && nr <= n && nc >= 1 && nc <= m && grid(nr, nc) ~= '#'
            new_cost = cost + 1;
            if new_cost < dist(nr, nc, d)
                dist(nr, nc, d) = new_cost;
                heap_size = heap_size + 1;
                heap(heap_size, :) = [new_cost, nr, nc, d];
                idx = heap_size;
                while idx > 1
                    parent_idx = floor(idx / 2);
                    if heap(idx, 1) < heap(parent_idx, 1)
                        heap([idx, parent_idx], :) = heap([parent_idx, idx], :);
                        idx = parent_idx;
                    else
                        break;
                    end
                end
            end
        end
    end
    
    best = min(dist(er, ec, :));
    
    if isinf(best)
        fprintf('0\n');
        return;
    end
    
    vis = false(n, m, 4);
    used = false(n, m);
    
    rev_stack = zeros(n * m * 4, 3);
    rev_top = 0;
    
    for d = 1:4
        if dist(er, ec, d) == best && ~vis(er, ec, d)
            vis(er, ec, d) = true;
            rev_top = rev_top + 1;
            rev_stack(rev_top, :) = [er, ec, d];
        end
    end
    
    while rev_top > 0
        current = rev_stack(rev_top, :);
        rev_top = rev_top - 1;
        
        r = current(1);
        c = current(2);
        d = current(3);
        
        used(r, c) = true;
        costU = dist(r, c, d);
        
        pd_r = mod(d, 4) + 1;
        pd_l = mod(d + 2, 4) + 1;
        
        for pd = [pd_r, pd_l]
            if costU >= 1000 && dist(r, c, pd) == costU - 1000 && ~vis(r, c, pd)
                vis(r, c, pd) = true;
                rev_top = rev_top + 1;
                rev_stack(rev_top, :) = [r, c, pd];
            end
        end
        
        pr = r - dr(d);
        pc = c - dc(d);
        
        if pr >= 1 && pr <= n && pc >= 1 && pc <= m && grid(pr, pc) ~= '#'
            if costU > 0 && dist(pr, pc, d) == costU - 1 && ~vis(pr, pc, d)
                vis(pr, pc, d) = true;
                rev_top = rev_top + 1;
                rev_stack(rev_top, :) = [pr, pc, d];
            end
        end
    end
    
    count = sum(used(:));
    fprintf('%d\n', count);
end
