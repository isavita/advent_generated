
function main
    fid = fopen('input.txt', 'r');
    raw_data = fread(fid, '*char')';
    fclose(fid);

    groups = strsplit(raw_data, '\n\n');
    
    calories = zeros(length(groups), 1);
    for i = 1:length(groups)
        items = str2double(strsplit(groups{i}));
        calories(i) = sum(items);
    end
    
    result = max(calories);
    disp(result);
end
