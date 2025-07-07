
function main()
    fileId = fopen('input.txt', 'r');
    data = textscan(fileId, '<x=%d, y=%d, z=%d>');
    fclose(fileId);

    pos = [data{1}, data{2}, data{3}];
    vel = zeros(size(pos));

    initialPos = pos;
    initialVel = vel;

    cycleX = findCycle(pos, vel, initialPos, initialVel, 1);
    cycleY = findCycle(pos, vel, initialPos, initialVel, 2);
    cycleZ = findCycle(pos, vel, initialPos, initialVel, 3);

    result = lcm(lcm(cycleX, cycleY), cycleZ);
    fprintf('%d\n', result);
end

function cycle = findCycle(pos, vel, initialPos, initialVel, axis)
    steps = 0;
    while true
        steps = steps + 1;
        for i = 1:size(pos, 1)
            for j = i+1:size(pos, 1)
                if pos(i, axis) > pos(j, axis)
                    vel(i, axis) = vel(i, axis) - 1;
                    vel(j, axis) = vel(j, axis) + 1;
                elseif pos(i, axis) < pos(j, axis)
                    vel(i, axis) = vel(i, axis) + 1;
                    vel(j, axis) = vel(j, axis) - 1;
                end
            end
        end

        pos = pos + vel;

        if all(pos(:, axis) == initialPos(:, axis)) && all(vel(:, axis) == initialVel(:, axis))
            cycle = steps;
            return;
        end
    end
end

function result = lcm(a, b)
    result = abs(a*b) / gcd(a, b);
end

main();
