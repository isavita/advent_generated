
function main
    fid = fopen('input.txt', 'r');
    fgetl(fid);
    line2 = fgetl(fid);
    fclose(fid);

    bus_data = strsplit(line2, ',');
    buses = [];
    offsets = [];
    for i = 1:length(bus_data)
        if ~strcmp(bus_data{i}, 'x')
            bus_id = str2double(bus_data{i});
            buses = [buses, bus_id];
            offsets = [offsets, i - 1];
        end
    end

    t = 1;
    step = 1;
    for i = 1:length(buses)
        bus = buses(i);
        offset = offsets(i);
        while mod(t + offset, bus) ~= 0
            t = t + step;
        end
        step = step * bus;
    end

    fprintf('%d\n', t);
end
