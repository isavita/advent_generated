
function main()
    fid = fopen('input.txt', 'r');
    path1 = fgetl(fid);
    path2 = fgetl(fid);
    fclose(fid);

    wire1 = getPointsWithSteps(path1);
    wire2 = getPointsWithSteps(path2);

    minSteps = inf;
    keys1 = fieldnames(wire1);
    for i = 1:length(keys1)
        key = keys1{i};
        if isfield(wire2, key)
            totalSteps = wire1.(key) + wire2.(key);
            if totalSteps < minSteps
                minSteps = totalSteps;
            end
        end
    end

    disp(minSteps);
end

function points = getPointsWithSteps(path)
    points = struct();
    current = [0, 0];
    steps = 0;
    moves = strsplit(path, ',');

    for i = 1:length(moves)
        move = moves{i};
        dir = move(1);
        dist = str2double(move(2:end));

        for j = 1:dist
            steps = steps + 1;
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
            key = sprintf('%d,%d', current(1), current(2));
            if ~isfield(points, key)
                points.(key) = steps;
            end
        end
    end
end
