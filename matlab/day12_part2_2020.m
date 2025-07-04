
function main()
    fid = fopen('input.txt', 'r');
    x = 0;
    y = 0;
    waypoint_x = 10;
    waypoint_y = 1;

    while ~feof(fid)
        line = strtrim(fgets(fid));
        action = line(1);
        value = str2double(line(2:end));

        switch action
            case 'N'
                waypoint_y = waypoint_y + value;
            case 'S'
                waypoint_y = waypoint_y - value;
            case 'E'
                waypoint_x = waypoint_x + value;
            case 'W'
                waypoint_x = waypoint_x - value;
            case 'L'
                rotations = value / 90;
                for i = 1:rotations
                    temp_x = waypoint_x;
                    waypoint_x = -waypoint_y;
                    waypoint_y = temp_x;
                end
            case 'R'
                rotations = value / 90;
                for i = 1:rotations
                    temp_x = waypoint_x;
                    waypoint_x = waypoint_y;
                    waypoint_y = -temp_x;
                end
            case 'F'
                x = x + waypoint_x * value;
                y = y + waypoint_y * value;
        end
    end
    fclose(fid);
    fprintf('%d\n', abs(x) + abs(y));
end
