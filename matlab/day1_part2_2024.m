
function main()
    fid = fopen('input.txt', 'r');
    input_text = fscanf(fid, '%c');
    fclose(fid);

    pairs = strsplit(strtrim(input_text), '\n');
    
    num_pairs = length(pairs);
    locations_ids = zeros(1, num_pairs);
    locations_similarities = zeros(1, num_pairs);

    for i = 1:num_pairs
        parts = strsplit(pairs{i});
        locations_ids(i) = str2double(parts{1});
        locations_similarities(i) = str2double(parts{2});
    end

    unique_ids = unique(locations_ids);
    total = 0;
    for id = unique_ids
        count = sum(locations_similarities == id);
        total = total + count * id;
    end

    fprintf('%d\n', total);
end

main();
