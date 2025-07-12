
fid = fopen('input.txt', 'r');
if fid == -1
    error('Error opening file input.txt');
end

line1 = fgetl(fid);
depth = sscanf(line1, 'depth: %d');

line2 = fgetl(fid);
target_coords = sscanf(line2, 'target: %d,%d');
target_x = target_coords(1);
target_y = target_coords(2);

fclose(fid);

cave = zeros(target_x + 1, target_y + 1);
risk_level = 0;

for x = 0:target_x
    for y = 0:target_y
        x_idx = x + 1;
        y_idx = y + 1;

        if (x == 0 && y == 0) || (x == target_x && y == target_y)
            geologic_index = 0;
        elseif y == 0
            geologic_index = x * 16807;
        elseif x == 0
            geologic_index = y * 48271;
        else
            geologic_index = cave(x_idx - 1, y_idx) * cave(x_idx, y_idx - 1);
        end

        cave(x_idx, y_idx) = mod(geologic_index + depth, 20183);
        risk_level = risk_level + mod(cave(x_idx, y_idx), 3);
    end
end

fprintf('%d\n', risk_level);
