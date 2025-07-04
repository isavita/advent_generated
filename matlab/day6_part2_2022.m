
function main()
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%c');
    fclose(fid);
    data = strtrim(data');

    marker_length = 14;
    for i = marker_length:length(data)
        if numel(unique(data(i-marker_length+1:i))) == marker_length
            disp(i);
            return;
        end
    end
end

main();
