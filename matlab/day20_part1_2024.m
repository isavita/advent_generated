
function main()
    fid = fopen('input.txt', 'r');
    grid_lines = textscan(fid, '%s', 'Delimiter', '\n', 'Whitespace', '');
    fclose(fid);
    grid_lines = grid_lines{1};
    
    H = numel(grid_lines);
    W = numel(grid_lines{1});
    grid = char(grid_lines);
    
    [Sr, Sc] = find(grid == 'S');
    [Er, Ec] = find(grid == 'E');
    
    walls = (grid == '#');
    [track_r, track_c] = find(~walls);
    track_cells = [track_r, track_c];
    
    dirs = [1 0; -1 0; 0 1; 0 -1];
    
    dist_from_s = bfs_matlab(Sr, Sc, H, W, walls, false, dirs);
    dist_from_e = bfs_matlab(Er, Ec, H, W, walls, false, dirs);
    
    if dist_from_s(Er, Ec) == -1
        fprintf('%d\n', 0);
        return;
    end
    
    normal_cost = dist_from_s(Er, Ec);
    
    possible_cheats = 0;
    
    for i = 1:size(track_cells, 1)
        start_pos_r = track_cells(i, 1);
        start_pos_c = track_cells(i, 2);
        
        sd = dist_from_s(start_pos_r, start_pos_c);
        if sd == -1
            continue;
        end
        
        for k1 = 1:size(dirs, 1)
            dr1 = dirs(k1, 1);
            dc1 = dirs(k1, 2);
            m1r = start_pos_r + dr1;
            m1c = start_pos_c + dc1;
            
            if m1r < 1 || m1r > H || m1c < 1 || m1c > W
                continue;
            end
            
            for k2 = 1:size(dirs, 1)
                dr2 = dirs(k2, 1);
                dc2 = dirs(k2, 2);
                m2r = m1r + dr2;
                m2c = m1c + dc2;
                
                if m2r < 1 || m2r > H || m2c < 1 || m2c > W
                    continue;
                end
                
                if walls(m2r, m2c)
                    continue;
                end
                
                ed = dist_from_e(m2r, m2c);
                if ed == -1
                    continue;
                end
                
                new_cost = sd + 2 + ed;
                saving = normal_cost - new_cost;
                
                if saving >= 100
                    possible_cheats = possible_cheats + 1;
                end
            end
        end
    end
    
    fprintf('%d\n', possible_cheats);
end

function dist = bfs_matlab(start_r, start_c, H, W, walls, ignore_walls, dirs)
    dist = -ones(H, W);
    dist(start_r, start_c) = 0;
    
    queue_max_size = H * W;
    q_r = zeros(1, queue_max_size);
    q_c = zeros(1, queue_max_size);
    
    head = 1;
    tail = 1;
    
    q_r(tail) = start_r;
    q_c(tail) = start_c;
    tail = tail + 1;
    
    while head < tail
        r = q_r(head);
        c = q_c(head);
        head = head + 1;
        
        for k = 1:size(dirs, 1)
            dr = dirs(k, 1);
            dc = dirs(k, 2);
            nr = r + dr;
            nc = c + dc;
            
            if nr >= 1 && nr <= H && nc >= 1 && nc <= W
                if ~ignore_walls && walls(nr, nc)
                    continue;
                end
                if dist(nr, nc) == -1
                    dist(nr, nc) = dist(r, c) + 1;
                    q_r(tail) = nr;
                    q_c(tail) = nc;
                    tail = tail + 1;
                end
            end
        end
    end
end
