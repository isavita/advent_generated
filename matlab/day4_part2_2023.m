
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Error opening file');
    end

    cards = struct('winnings', {}, 'givens', {}, 'totalCount', {});
    cardIndex = 1;

    while ~feof(fid)
        line = strtrim(fgets(fid));
        if isempty(line)
            continue;
        end

        parts = strsplit(line, ':');
        cardDataStr = strtrim(parts{2});
        
        dataParts = strsplit(cardDataStr, '|');
        winningNumsStr = strtrim(dataParts{1});
        givenNumsStr = strtrim(dataParts{2});

        winningNums = regexp(winningNumsStr, '\d+', 'match');
        givenNums = regexp(givenNumsStr, '\d+', 'match');

        cards(cardIndex).winnings = cellfun(@str2double, winningNums);
        cards(cardIndex).givens = cellfun(@str2double, givenNums);
        cards(cardIndex).totalCount = 1;
        
        cardIndex = cardIndex + 1;
    end
    fclose(fid);

    numCards = length(cards);
    for i = 1:numCards
        matches = sum(ismember(cards(i).givens, cards(i).winnings));
        for j = 1:matches
            if i + j <= numCards
                cards(i+j).totalCount = cards(i+j).totalCount + cards(i).totalCount;
            end
        end
    end

    totalCards = sum(arrayfun(@(x) x.totalCount, cards));
    fprintf('%d\n', totalCards);
end

main();
