
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%d:%d', 'Delimiter', ':');
    fclose(fid);

    depths = data{1};
    ranges = data{2};

    caught = @(delay) any(mod(depths + delay, 2 * (ranges - 1)) == 0);

    delay = 0;
    while caught(delay)
        delay = delay + 1;
    end

    disp(delay);
end
