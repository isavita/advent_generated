
function main()
    fid = fopen('input.txt', 'r');
    box_ids = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    box_ids = box_ids{1};

    num_twos = 0;
    num_threes = 0;

    for i = 1:length(box_ids)
        counts = countchars(box_ids{i});
        if any(counts == 2)
            num_twos = num_twos + 1;
        end
        if any(counts == 3)
            num_threes = num_threes + 1;
        end
    end

    fprintf('%d\n', num_twos * num_threes);
end

function counts = countchars(str)
    unique_chars = unique(str);
    counts = zeros(size(unique_chars));
    for i = 1:length(unique_chars)
        counts(i) = sum(str == unique_chars(i));
    end
end
