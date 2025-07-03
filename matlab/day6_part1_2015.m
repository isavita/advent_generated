
function main()
    lights = zeros(1000, 1000);
    file = fopen('input.txt', 'r');
    while ~feof(file)
        line = strtrim(fgets(file));
        parts = strsplit(line);
        if strcmp(parts{1}, 'turn')
            coords = strsplit(parts{3}, ',');
            x1 = str2double(coords{1}) + 1;
            y1 = str2double(coords{2}) + 1;
            coords = strsplit(parts{5}, ',');
            x2 = str2double(coords{1}) + 1;
            y2 = str2double(coords{2}) + 1;
            if strcmp(parts{2}, 'on')
                lights(x1:x2, y1:y2) = 1;
            else
                lights(x1:x2, y1:y2) = 0;
            end
        else
            coords = strsplit(parts{2}, ',');
            x1 = str2double(coords{1}) + 1;
            y1 = str2double(coords{2}) + 1;
            coords = strsplit(parts{4}, ',');
            x2 = str2double(coords{1}) + 1;
            y2 = str2double(coords{2}) + 1;
            lights(x1:x2, y1:y2) = 1 - lights(x1:x2, y1:y2);
        end
    end
    fclose(file);
    totalLightsOn = sum(lights(:));
    disp(totalLightsOn);
end

main();
