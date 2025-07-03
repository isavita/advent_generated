
function main()
    fid = fopen('input.txt', 'r');
    directions_str = fscanf(fid, '%s');
    fclose(fid);
    
    directions = strsplit(directions_str, ',');
    
    x = 0;
    y = 0;
    z = 0;
    max_distance = 0;
    
    for i = 1:length(directions)
        direction = directions{i};
        
        switch direction
            case 'n'
                y = y + 1;
                z = z - 1;
            case 'ne'
                x = x + 1;
                z = z - 1;
            case 'se'
                x = x + 1;
                y = y - 1;
            case 's'
                y = y - 1;
                z = z + 1;
            case 'sw'
                x = x - 1;
                z = z + 1;
            case 'nw'
                x = x - 1;
                y = y + 1;
        end
        
        current_max = max([abs(x), abs(y), abs(z)]);
        max_distance = max(current_max, max_distance);
    end
    
    result = max([abs(x), abs(y), abs(z)]);
    fprintf('%d\n', result);
end

main();
