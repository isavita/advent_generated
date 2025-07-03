
function main
    fid = fopen('input.txt', 'r');
    containers = fscanf(fid, '%d');
    fclose(fid);

    target = 150;
    count = 0;
    n = length(containers);

    for i = 1:n
        combos = nchoosek(containers, i);
        for j = 1:size(combos, 1)
            if sum(combos(j, :)) == target
                count = count + 1;
            end
        end
    end

    fprintf('%d\n', count);
end

main;
