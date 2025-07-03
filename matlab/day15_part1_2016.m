
function main
    fid = fopen('input.txt', 'r');
    discs = [];
    while ~feof(fid)
        line = fgetl(fid);
        [~, ~, ~, ~, matches] = regexp(line, 'Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+).');
        total_positions = str2double(matches{1}{1});
        start_position = str2double(matches{1}{2});
        discs = [discs; struct('total_positions', total_positions, 'start_position', start_position)];
    end
    fclose(fid);

    time = 0;
    while true
        valid = true;
        for i = 1:length(discs)
            if mod(discs(i).start_position + time + i, discs(i).total_positions) ~= 0
                valid = false;
                break;
            end
        end
        if valid
            disp(time);
            break;
        end
        time = time + 1;
    end
end
