
function main()
    fid = fopen('input.txt', 'r');
    expenses = fscanf(fid, '%d');
    fclose(fid);

    n = length(expenses);
    for i = 1:n
        for j = i+1:n
            if expenses(i) + expenses(j) == 2020
                fprintf('%d\n', expenses(i) * expenses(j));
                return;
            end
        end
    end
end

main();
