
function main()
    file = fopen('input.txt', 'r');
    input = fscanf(file, '%s');
    fclose(file);

    n = length(input);
    cups = zeros(1, n + 1);
    currentCup = str2double(input(1));

    for i = 1:n
        cup = str2double(input(i));
        if i < n
            nextCup = str2double(input(i + 1));
            cups(cup) = nextCup;
        end
    end
    cups(str2double(input(n))) = str2double(input(1));

    for move = 1:100
        pickup1 = cups(currentCup);
        pickup2 = cups(pickup1);
        pickup3 = cups(pickup2);

        cups(currentCup) = cups(pickup3);

        destinationCup = currentCup - 1;
        if destinationCup < 1
            destinationCup = n;
        end

        while destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3
            destinationCup = destinationCup - 1;
            if destinationCup < 1
                destinationCup = n;
            end
        end

        cups(pickup3) = cups(destinationCup);
        cups(destinationCup) = pickup1;

        currentCup = cups(currentCup);
    end

    cup = cups(1);
    while cup ~= 1
        fprintf('%d', cup);
        cup = cups(cup);
    end
    fprintf('\n');
end

main();
