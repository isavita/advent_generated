
function main()
    fid = fopen('input.txt', 'r');
    steps = fscanf(fid, '%d');
    fclose(fid);

    buffer = zeros(1, 1);
    current_pos = 0;

    for i = 1:2017
        current_pos = mod(current_pos + steps, length(buffer)) + 1;
        buffer = [buffer(1:current_pos-1), i, buffer(current_pos:end)];
    end

    final_pos = mod(current_pos + 1, length(buffer));
    if final_pos == 0
        final_pos = length(buffer);
    end
    disp(buffer(final_pos));
end

main();
