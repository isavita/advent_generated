
function main()
    fileID = fopen('input.txt', 'r');
    packages = cell2mat(textscan(fileID, '%d'));
    fclose(fileID);

    targetWeight = sum(packages) / 4;

    for groupLength = 1:numel(packages)
        firstGroupCombs = nchoosek(packages, groupLength);
        validMask = sum(firstGroupCombs, 2) == targetWeight;
        validFirstGroups = firstGroupCombs(validMask, :);

        if ~isempty(validFirstGroups)
            qes = prod(int64(validFirstGroups), 2);
            [sorted_qes, sort_idx] = sort(qes);
            sorted_groups = validFirstGroups(sort_idx, :);

            for i = 1:size(sorted_groups, 1)
                currentGroup = sorted_groups(i, :);
                remainingPackages = multisetdiff(packages, currentGroup);

                if can_split_into_three(remainingPackages, targetWeight)
                    fprintf('%d\n', sorted_qes(i));
                    return;
                end
            end
        end
    end
end

function can = can_split_into_three(packages, targetWeight)
    can = false;
    n = numel(packages);
    for k1 = 1:(n - 1)
        combs1 = nchoosek(packages, k1);
        validMask1 = sum(combs1, 2) == targetWeight;
        validGroups1 = combs1(validMask1, :);

        if ~isempty(validGroups1)
            for i = 1:size(validGroups1, 1)
                group2 = validGroups1(i, :);
                remaining = multisetdiff(packages, group2);
                if can_form_one_group(remaining, targetWeight)
                    can = true;
                    return;
                end
            end
        end
    end
end

function can = can_form_one_group(packages, targetWeight)
    can = false;
    n = numel(packages);
    if n == 0, return; end
    for k = 1:ceil(n / 2)
        combs = nchoosek(packages, k);
        if any(sum(combs, 2) == targetWeight)
            can = true;
            return;
        end
    end
end

function result = multisetdiff(A, B)
    result = A;
    for val = B(:)'
        idx = find(result == val, 1);
        if ~isempty(idx)
            result(idx) = [];
        end
    end
end
