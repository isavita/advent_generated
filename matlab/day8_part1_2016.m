
screen = zeros(6, 50);

fid = fopen('input.txt', 'r');
while ~feof(fid)
    line = fgetl(fid);
    if ~ischar(line)
        continue;
    end

    if startsWith(line, 'rect')
        parts = strsplit(line, {' ', 'x'});
        a = str2double(parts{2});
        b = str2double(parts{3});
        screen(1:b, 1:a) = 1;
    elseif startsWith(line, 'rotate row')
        C = sscanf(line, 'rotate row y=%d by %d');
        y = C(1);
        b = C(2);
        screen(y+1, :) = circshift(screen(y+1, :), [0, b]);
    elseif startsWith(line, 'rotate column')
        C = sscanf(line, 'rotate column x=%d by %d');
        x = C(1);
        b = C(2);
        screen(:, x+1) = circshift(screen(:, x+1), [b, 0]);
    end
end
fclose(fid);

lit_pixels = sum(screen(:));
fprintf('%d\n', lit_pixels);
