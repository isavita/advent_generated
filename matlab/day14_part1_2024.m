
function main()
    file = fopen('input.txt', 'r');
    robots = zeros(0, 4);
    while ~feof(file)
        line = strtrim(fgets(file));
        parts = strsplit(line, ' ');
        pPart = strrep(parts{1}, 'p=', '');
        vPart = strrep(parts{2}, 'v=', '');
        pos = str2double(strsplit(pPart, ','));
        vel = str2double(strsplit(vPart, ','));
        robots(end+1, :) = [pos(1), pos(2), vel(1), vel(2)];
    end
    fclose(file);

    width = 101;
    height = 103;

    for i = 1:100
        robots(:, 1) = mod(robots(:, 1) + robots(:, 3), width);
        robots(:, 2) = mod(robots(:, 2) + robots(:, 4), height);
        robots(robots(:, 1) < 0, 1) = robots(robots(:, 1) < 0, 1) + width;
        robots(robots(:, 2) < 0, 2) = robots(robots(:, 2) < 0, 2) + height;
    end

    q1 = sum(robots(:, 1) < 50 & robots(:, 2) < 51);
    q2 = sum(robots(:, 1) > 50 & robots(:, 2) < 51);
    q3 = sum(robots(:, 1) < 50 & robots(:, 2) > 51);
    q4 = sum(robots(:, 1) > 50 & robots(:, 2) > 51);

    fprintf('%d\n', q1 * q2 * q3 * q4);
end

main();
