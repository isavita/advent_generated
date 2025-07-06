
function main()
    fid = fopen('input.txt', 'r');
    expenses = fscanf(fid, '%d');
    fclose(fid);

    n = length(expenses);
    for i = 1:n
        for j = i+1:n
            for k = j+1:n
                if expenses(i) + expenses(j) + expenses(k) == 2020
                    fprintf('%d\n', expenses(i) * expenses(j) * expenses(k));
                    return;
                end
            end
        end
    end
end
