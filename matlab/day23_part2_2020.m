
function main()
    totalCups = 1000000;
    totalMoves = 10000000;

    file = fopen("input.txt", "r");
    inputStr = strtrim(fgets(file));
    fclose(file);

    cups = zeros(1, totalCups);
    lastCup = 0;

    for i = 1:length(inputStr)
        cup = str2double(inputStr(i));
        if i > 1
            cups(lastCup) = cup;
        end
        lastCup = cup;
    end

    for i = length(inputStr) + 1:totalCups
        cups(lastCup) = i;
        lastCup = i;
    end
    cups(lastCup) = str2double(inputStr(1));

    currentCup = str2double(inputStr(1));
    for i = 1:totalMoves
        pickup1 = cups(currentCup);
        pickup2 = cups(pickup1);
        pickup3 = cups(pickup2);

        cups(currentCup) = cups(pickup3);

        destinationCup = currentCup - 1;
        if destinationCup < 1
            destinationCup = totalCups;
        end

        while destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3
            destinationCup = destinationCup - 1;
            if destinationCup < 1
                destinationCup = totalCups;
            end
        end

        cups(pickup3) = cups(destinationCup);
        cups(destinationCup) = pickup1;

        currentCup = cups(currentCup);
    end

    cup1 = cups(1);
    cup2 = cups(cup1);
    fprintf("%d\n", cup1 * cup2);
end

main();
