
function main()
    file = fopen("input.txt", "r");
    timestamp = str2double(fgetl(file));
    buses_str = strsplit(fgetl(file), ",");
    buses = buses_str(~strcmp(buses_str, "x"));
    buses = cellfun(@str2double, buses);
    fclose(file);

    wait_times = buses - mod(timestamp, buses);
    [min_wait, idx] = min(wait_times);
    selected_bus = buses(idx);

    disp(selected_bus * min_wait);
end

main();
