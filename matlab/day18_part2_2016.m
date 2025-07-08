
function main()
    filename = "input.txt";
    first_row_str = readFirstRow(filename);
    total_rows = 400000;

    current_row_logical = (first_row_str == '^');
    safe_count = sum(~current_row_logical);
    row_length = length(current_row_logical);

    for i = 1:(total_rows - 1)
        padded_row = [false, current_row_logical, false];
        L = padded_row(1:end-2);
        R = padded_row(3:end);
        next_row_logical = xor(L, R);
        current_row_logical = next_row_logical;
        safe_count = safe_count + sum(~current_row_logical);
    end

    fprintf('%d\n', safe_count);
end

function row_str = readFirstRow(filename)
    fid = fopen(filename, 'r');
    row_str = fgetl(fid);
    fclose(fid);
end
