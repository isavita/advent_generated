
function main()
    lights = zeros(1000, 1000, 'int32');
    fileId = fopen('input.txt', 'r');
    
    while ~feof(fileId)
        line = strtrim(fgets(fileId));
        words = strsplit(line);
        
        if strcmp(words{1}, 'turn')
            startCoords = str2double(strsplit(words{3}, ','));
            endCoords = str2double(strsplit(words{5}, ','));
            
            if strcmp(words{2}, 'on')
                lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) = ...
                    lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) + 1;
            elseif strcmp(words{2}, 'off')
                lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) = ...
                    lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) - 1;
                lights(lights < 0) = 0;
            end
        elseif strcmp(words{1}, 'toggle')
            startCoords = str2double(strsplit(words{2}, ','));
            endCoords = str2double(strsplit(words{4}, ','));
            
            lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) = ...
                lights(startCoords(1)+1:endCoords(1)+1, startCoords(2)+1:endCoords(2)+1) + 2;
        end
    end
    
    fclose(fileId);
    fprintf('%d\n', sum(lights(:)));
end

main();
