
function main()
    fileId = fopen('input.txt', 'r');
    data = textscan(fileId, '%s', 'Delimiter', '\n');
    fclose(fileId);
    data = data{1};

    count = 0;
    for i = 1:length(data)
        ranges = strsplit(data{i}, ',');
        range1 = ranges{1};
        range2 = ranges{2};

        parts1 = strsplit(range1, '-');
        start1 = str2double(parts1{1});
        end1 = str2double(parts1{2});

        parts2 = strsplit(range2, '-');
        start2 = str2double(parts2{1});
        end2 = str2double(parts2{2});

        if (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)
            count = count + 1;
        end
    end
    disp(count);
end

main();
