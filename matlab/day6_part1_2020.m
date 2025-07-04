
function main()
    fid = fopen('input.txt', 'r');
    content = fread(fid, '*char')';
    fclose(fid);

    groups = strsplit(strtrim(content), '\n\n');

    total_count = 0;
    for i = 1:length(groups)
        group_answers = strsplit(groups{i}, '\n');
        combined_answers = '';
        for j = 1:length(group_answers)
            combined_answers = [combined_answers, group_answers{j}];
        end
        unique_chars = unique(combined_answers);
        total_count = total_count + length(unique_chars);
    end

    disp(total_count);
end
