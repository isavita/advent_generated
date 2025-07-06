
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file: input.txt');
    end
    
    ship = struct('x', 0, 'y', 0, 'facing', 0);
    
    tline = fgetl(fid);
    while ischar(tline)
        action = tline(1);
        value = str2double(tline(2:end));
        
        switch action
            case 'N'
                ship.y = ship.y + value;
            case 'S'
                ship.y = ship.y - value;
            case 'E'
                ship.x = ship.x + value;
            case 'W'
                ship.x = ship.x - value;
            case 'L'
                ship.facing = mod(ship.facing - value, 360);
            case 'R'
                ship.facing = mod(ship.facing + value, 360);
            case 'F'
                switch ship.facing
                    case 0
                        ship.x = ship.x + value;
                    case 90
                        ship.y = ship.y - value;
                    case 180
                        ship.x = ship.x - value;
                    case 270
                        ship.y = ship.y + value;
                end
        end
        
        tline = fgetl(fid);
    end
    
    fclose(fid);
    
    manhattanDistance = abs(ship.x) + abs(ship.y);
    disp(manhattanDistance);
end

main();
