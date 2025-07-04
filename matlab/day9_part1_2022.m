
function main()
    fid = fopen("input.txt", "r");
    lines = textscan(fid, "%s %d");
    fclose(fid);

    head = [0, 0];
    tail = [0, 0];
    visited = containers.Map('KeyType', 'char', 'ValueType', 'logical');
    visited('0,0') = true;

    directions = lines{1};
    steps = lines{2};

    for i = 1:length(directions)
        dir = directions{i};
        numSteps = steps(i);

        for j = 1:numSteps
            if dir == "R"
                head(1) = head(1) + 1;
            elseif dir == "L"
                head(1) = head(1) - 1;
            elseif dir == "U"
                head(2) = head(2) + 1;
            elseif dir == "D"
                head(2) = head(2) - 1;
            end

            if abs(head(1) - tail(1)) > 1 || abs(head(2) - tail(2)) > 1
                if head(1) ~= tail(1) && head(2) ~= tail(2)
                    tail(1) = tail(1) + sign(head(1) - tail(1));
                    tail(2) = tail(2) + sign(head(2) - tail(2));
                else
                    tail(1) = tail(1) + sign(head(1) - tail(1));
                    tail(2) = tail(2) + sign(head(2) - tail(2));
                end
            end
            visited(sprintf('%d,%d', tail(1), tail(2))) = true;
        end
    end

    disp(length(visited));
end

main();
