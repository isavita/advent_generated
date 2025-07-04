
function main()
    fid = fopen('input.txt', 'r');
    passes = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    passes = passes{1};

    seat_ids = zeros(length(passes), 1);
    for i = 1:length(passes)
        boarding_pass = passes{i};
        row_str = strrep(boarding_pass(1:7), 'F', '0');
        row_str = strrep(row_str, 'B', '1');
        row = bin2dec(row_str);

        col_str = strrep(boarding_pass(8:end), 'L', '0');
        col_str = strrep(col_str, 'R', '1');
        col = bin2dec(col_str);

        seat_ids(i) = row * 8 + col;
    end

    fprintf('%d\n', max(seat_ids));

    min_seat = min(seat_ids);
    max_seat = max(seat_ids);
    all_possible_seats = min_seat:max_seat;
    
    seat_ids_sorted = sort(seat_ids);
    
    missing_seat_idx = find(diff(seat_ids_sorted) > 1);
    if ~isempty(missing_seat_idx)
        fprintf('%d\n', seat_ids_sorted(missing_seat_idx(1)) + 1);
    end
end

main();
