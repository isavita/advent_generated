
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    wire1 = getPoints(lines{1}{1});
    wire2 = getPoints(lines{1}{2});
    
    intersections = intersect(wire1, wire2, 'rows');
    
    distances = sum(abs(intersections), 2);
    minDistance = min(distances);
    
    fprintf('%d\n', minDistance);
end

function points = getPoints(path)
    moves = strsplit(path, ',');
    current = [0, 0];
    points = zeros(1, 2);
    
    for i = 1:length(moves)
        move = moves{i};
        dir = move(1);
        steps = str2double(move(2:end));
        
        for j = 1:steps
            switch dir
                case 'U'
                    current(2) = current(2) + 1;
                case 'D'
                    current(2) = current(2) - 1;
                case 'L'
                    current(1) = current(1) - 1;
                case 'R'
                    current(1) = current(1) + 1;
            end
            points(end+1, :) = current;
        end
    end
    points(1,:) = [];
end

main();
