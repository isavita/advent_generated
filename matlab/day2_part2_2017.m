
function main
    data = dlmread('input.txt');

    diffs = max(data, [], 2) - min(data, [], 2);
    disp(sum(diffs));

    result = 0;
    for i = 1:size(data, 1)
        row = data(i, :);
        for j = 1:length(row)
            for k = j+1:length(row)
                if mod(row(j), row(k)) == 0
                    result = result + floor(row(j) / row(k));
                elseif mod(row(k), row(j)) == 0
                    result = result + floor(row(k) / row(j));
                end
            end
        end
    end
    disp(result);
end
