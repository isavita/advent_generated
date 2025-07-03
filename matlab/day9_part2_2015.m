
function main()
    fid = fopen('input.txt', 'r');
    distances = containers.Map;
    locations = {};

    while ~feof(fid)
        line = strsplit(strtrim(fgets(fid)));
        loc1 = line{1};
        loc2 = line{3};
        dist = str2double(line{5});
        distances(strjoin({loc1, loc2}, '-')) = dist;
        distances(strjoin({loc2, loc1}, '-')) = dist;
        locations = [locations, loc1, loc2];
    end
    fclose(fid);

    locations = unique(locations);
    n = length(locations);
    shortest_distance = inf;
    longest_distance = 0;

    p = perms(locations);

    for i = 1:size(p, 1)
        current_distance = 0;
        for j = 1:(n - 1)
            key = strjoin({p{i, j}, p{i, j+1}}, '-');
            current_distance = current_distance + distances(key);
        end
        shortest_distance = min(shortest_distance, current_distance);
        longest_distance = max(longest_distance, current_distance);
    end

    fprintf('%d\n', shortest_distance);
    fprintf('%d\n', longest_distance);
end

main();
