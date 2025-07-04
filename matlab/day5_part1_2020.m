
function main()
    file_id = fopen("input.txt", "r");
    data = textscan(file_id, "%s", "Delimiter", "\n");
    fclose(file_id);
    
    boarding_passes = data{1};
    
    num_passes = length(boarding_passes);
    seat_ids = zeros(num_passes, 1);
    
    for i = 1:num_passes
        bp = boarding_passes{i};
        row_str = strrep(bp(1:7), "F", "0");
        row_str = strrep(row_str, "B", "1");
        row = bin2dec(row_str);
        
        col_str = strrep(bp(8:end), "L", "0");
        col_str = strrep(col_str, "R", "1");
        col = bin2dec(col_str);
        
        seat_ids(i) = row * 8 + col;
    end
    
    fprintf("%d\n", max(seat_ids));
end

main();
