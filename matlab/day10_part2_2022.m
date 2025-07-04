
function main()
    x = [1];
    fileId = fopen("input.txt", "r");
    lines = textscan(fileId, "%s", "Delimiter", "\n");
    fclose(fileId);
    lines = lines{1};

    for i = 1:length(lines)
        line = lines{i};
        if strcmp(line, "noop")
            x(end+1) = x(end);
        else
            parts = strsplit(line);
            n = str2double(parts{2});
            x(end+1) = x(end);
            x(end+1) = x(end) + n;
        end
    end

    grid = repmat('.', [6, 40]);
    for i = 1:length(x)
        crtx = mod(i-1, 40);
        crty = floor((i-1) / 40);
        if abs(crtx - x(i)) <= 1
            grid(crty+1, crtx+1) = '#';
        end
    end

    for y = 1:6
        disp(grid(y, :));
    end
end
