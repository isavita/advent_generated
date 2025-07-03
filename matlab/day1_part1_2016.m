
function main()
    dirs = [0, 1; 1, 0; 0, -1; -1, 0];
    current_dir = 1;
    x = 0;
    y = 0;

    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s', 'Delimiter', ', ');
    fclose(fid);
    
    data = data{1};

    for i = 1:length(data)
        d = data{i};
        if d(1) == 'R'
            current_dir = mod(current_dir, 4) + 1;
        else
            current_dir = mod(current_dir - 2, 4) + 1;
        end
        
        dist = str2double(d(2:end));
        x = x + dirs(current_dir, 1) * dist;
        y = y + dirs(current_dir, 1) * dist;
    end
    
    disp(abs(x) + abs(y));
end

main();
