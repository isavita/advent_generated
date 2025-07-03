
function main()
    screen = repmat('.', 6, 50);
    file = fopen('input.txt', 'r');
    while ~feof(file)
        instruction = strtrim(fgets(file));
        parts = strsplit(instruction);
        if strcmp(parts{1}, 'rect')
            dims = strsplit(parts{2}, 'x');
            width = str2double(dims{1});
            height = str2double(dims{2});
            screen(1:height, 1:width) = '#';
        elseif strcmp(parts{1}, 'rotate')
            idx = str2double(strsplit(parts{3}, '='){2});
            by = str2double(parts{5});
            if strcmp(parts{2}, 'row')
                screen(idx+1, :) = circshift(screen(idx+1, :), by);
            elseif strcmp(parts{2}, 'column')
                screen(:, idx+1) = circshift(screen(:, idx+1), by);
            end
        end
    end
    fclose(file);
    for i = 1:size(screen, 1)
        disp(screen(i, :));
    end
end

main();
