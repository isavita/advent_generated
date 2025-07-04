
function main
    fid = fopen('input.txt', 'r');
    box_ids = textscan(fid, '%s');
    fclose(fid);
    box_ids = box_ids{1};

    count_two = 0;
    count_three = 0;

    for i = 1:length(box_ids)
        counts = countchars(box_ids{i});
        if any(counts == 2)
            count_two = count_two + 1;
        end
        if any(counts == 3)
            count_three = count_three + 1;
        end
    end

    fprintf('%d\n', count_two * count_three);

    for i = 1:length(box_ids)
        for j = i + 1:length(box_ids)
            diff_count = sum(box_ids{i} ~= box_ids{j});
            if diff_count == 1
                common_chars = box_ids{i}(box_ids{i} == box_ids{j});
                fprintf('%s\n', common_chars);
                return;
            end
        end
    end
end

function counts = countchars(str)
    unique_chars = unique(str);
    counts = zeros(size(unique_chars));
    for i = 1:length(unique_chars)
        counts(i) = sum(str == unique_chars(i));
    end
end
