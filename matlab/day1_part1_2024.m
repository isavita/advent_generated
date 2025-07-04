
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%d %d');
    fclose(fid);

    locations_ids1 = sort(data{1});
    locations_ids2 = sort(data{2});

    distances = abs(locations_ids1 - locations_ids2);

    fprintf('%d\n', sum(distances));
end

main();
