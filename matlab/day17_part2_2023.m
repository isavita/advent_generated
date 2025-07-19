
function main
    % Read grid
    fid = fopen('input.txt');
    if fid < 0, error('Cannot open input.txt'); end
    lines = cellfun(@strtrim, textscan(fid,'%s','Delimiter',''), 'UniformOutput', false);
    fclose(fid);
    grid = char(lines{:});
    height = size(grid,1);
    width  = size(grid,2);

    % Directions: N=1, S=2, E=3, W=4
    dr = [-1 1 0 0];
    dc = [0 0 1 -1];

    % Part 1
    fprintf('%d\n', solve(1, 3, grid, height, width, dr, dc));

    % Part 2
    fprintf('%d\n', solve(4, 10, grid, height, width, dr, dc));
end

function cost = solve(minTurn, maxStraight, grid, h, w, dr, dc)
    infVal = intmax('int32');
    dist = repmat(infVal, h, w, 4, maxStraight+1);
    heap = [];
    % Initial moves
    if w > 1
        c = grid(1,2)-'0';
        dist(1,2,3,1) = c;
        heap(end+1,:) = [c 1 2 3 1];
    end
    if h > 1
        c = grid(2,1)-'0';
        dist(2,1,2,1) = c;
        heap(end+1,:) = [c 2 1 2 1];
    end

    while ~isempty(heap)
        [~, idx] = min(heap(:,1));
        state = heap(idx,:);
        heap(idx,:) = [];
        cost = state(1);
        r = state(2); c = state(3); dir = state(4); steps = state(5);
        if cost > dist(r,c,dir,steps), continue; end
        if r==h && c==w && steps>=minTurn
            return
        end
        for nextDir = 1:4
            if (dir==1 && nextDir==2) || (dir==2 && nextDir==1) || ...
               (dir==3 && nextDir==4) || (dir==4 && nextDir==3), continue; end
            nr = r + dr(nextDir);
            nc = c + dc(nextDir);
            if nr<1 || nr>h || nc<1 || nc>w, continue; end
            newCost = cost + (grid(nr,nc)-'0');
            if nextDir==dir
                if steps>=maxStraight, continue; end
                newSteps = steps+1;
            else
                if steps<minTurn, continue; end
                newSteps = 1;
            end
            if newCost < dist(nr,nc,nextDir,newSteps)
                dist(nr,nc,nextDir,newSteps) = newCost;
                heap(end+1,:) = [newCost nr nc nextDir newSteps];
            end
        end
    end
    cost = -1;
end
