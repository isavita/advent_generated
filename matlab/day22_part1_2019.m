
function main()
    Size = 10007;
    deck = 0:(Size-1);

    file = fopen('input.txt', 'r');
    while ~feof(file)
        line = strtrim(fgets(file));
        if strcmp(line, 'deal into new stack')
            deck = dealIntoNewStack(deck);
        elseif startsWith(line, 'cut')
            parts = strsplit(line);
            n = str2double(parts{2});
            deck = cutN(deck, n);
        elseif startsWith(line, 'deal with increment')
            parts = strsplit(line);
            n = str2double(parts{end});
            deck = dealWithIncrement(deck, n);
        end
    end
    fclose(file);

    disp(find2019(deck));
end

function deck = dealIntoNewStack(deck)
    deck = deck(end:-1:1);
end

function deck = cutN(deck, n)
    Size = length(deck);
    if n < 0
        n = Size + n;
    end
    deck = [deck(n+1:end), deck(1:n)];
end

function deck = dealWithIncrement(deck, n)
    Size = length(deck);
    newDeck = zeros(1, Size);
    for i = 0:(Size-1)
        newDeck(mod(i*n, Size) + 1) = deck(i+1);
    end
    deck = newDeck;
end

function index = find2019(deck)
    index = find(deck == 2019) - 1;
end

main();
