
function totalWinnings = camelCards()
    % camelCards reads hands and bids from input.txt, calculates total winnings.
    %
    % The function determines the strength of each hand, ranks them, and
    % calculates the total winnings by multiplying each hand's bid by its rank.
    %
    % Returns:
    %   totalWinnings: The sum of (bid * rank) for all hands.

    % Define card strengths for comparison.
    cardStrengths = containers.Map('KeyType', 'char', 'ValueType', 'double');
    cardStrengths('A') = 14;
    cardStrengths('K') = 13;
    cardStrengths('Q') = 12;
    cardStrengths('J') = 11;
    cardStrengths('T') = 10;
    cardStrengths('9') = 9;
    cardStrengths('8') = 8;
    cardStrengths('7') = 7;
    cardStrengths('6') = 6;
    cardStrengths('5') = 5;
    cardStrengths('4') = 4;
    cardStrengths('3') = 3;
    cardStrengths('2') = 2;

    % Read hands and bids from input.txt.
    try
        fileId = fopen('input.txt', 'r');
        if fileId == -1
            error('Unable to open input.txt for reading.');
        end
        handsData = textscan(fileId, '%s %d', 'Delimiter', ' ');
        fclose(fileId);
    catch ME
        fprintf('Error reading file: %s\n', ME.message);
        return;
    end

    hands = handsData{1};
    bids = handsData{2};

    numHands = length(hands);
    handTypes = zeros(numHands, 1);
    handValues = zeros(numHands, 5);

    % Process each hand to determine its type and numerical value.
    for i = 1:numHands
        hand = hands{i};
        handTypes(i) = determineHandType(hand);
        handValues(i, :) = getHandValue(hand, cardStrengths);
    end

    % Combine hand type, hand value, and bid for sorting.
    % Each row: [handType, card1Value, card2Value, card3Value, card4Value, card5Value, bid]
    allHandsInfo = [handTypes, handValues, bids];

    % Sort hands based on type and then card values.
    % Sorting is done in ascending order of type (weakest to strongest).
    % For ties in type, sort by card values (weakest to strongest).
    sortedHandsInfo = sortrows(allHandsInfo, [1, 2, 3, 4, 5, 6]);

    % Calculate total winnings.
    totalWinnings = 0;
    for rank = 1:numHands
        bid = sortedHandsInfo(rank, 7);
        totalWinnings = totalWinnings + (rank * bid);
    end

    % Print the result to standard output.
    fprintf('Total Winnings: %d\n', totalWinnings);
end

function handType = determineHandType(hand)
    % determineHandType classifies a five-card hand into one of seven types.
    %
    % Args:
    %   hand: A string representing the five-card hand (e.g., "AAAAA").
    %
    % Returns:
    %   handType: A numerical representation of the hand type, from weakest (1)
    %             to strongest (7).
    %             1: High card
    %             2: One pair
    %             3: Two pair
    %             4: Three of a kind
    %             5: Full house
    %             6: Four of a kind
    %             7: Five of a kind

    counts = containers.Map('KeyType', 'char', 'ValueType', 'double');
    for card = hand
        if isKey(counts, card)
            counts(card) = counts(card) + 1;
        else
            counts(card) = 1;
        end
    end

    numUniqueCards = length(counts);
    cardCounts = sort(cell2mat(values(counts)), 'descend'); % Sort counts descending

    if numUniqueCards == 1 % Five of a kind
        handType = 7;
    elseif numUniqueCards == 2
        if cardCounts(1) == 4 % Four of a kind
            handType = 6;
        else % Full house
            handType = 5;
        end
    elseif numUniqueCards == 3
        if cardCounts(1) == 3 % Three of a kind
            handType = 4;
        else % Two pair
            handType = 3;
        end
    elseif numUniqueCards == 4 % One pair
        handType = 2;
    else % High card
        handType = 1;
    end
end

function handValue = getHandValue(hand, cardStrengths)
    % getHandValue converts a hand string into a numerical representation
    % for comparison.
    %
    % Args:
    %   hand: A string representing the five-card hand.
    %   cardStrengths: A containers.Map object mapping card characters to their strengths.
    %
    % Returns:
    %   handValue: A row vector of numerical strengths for each card in the hand.

    handValue = zeros(1, 5);
    for i = 1:5
        handValue(i) = cardStrengths(hand(i));
    end
end

% --- Main Entry Point ---
% This block ensures the script runs the main function when executed directly.
if isempty(ver('matlab')) % Check if running in MATLAB environment
    % If not in MATLAB, this part might not be directly executable without
    % a MATLAB runtime. For a typical MATLAB script, the function call
    % at the end is sufficient.
else
    camelCards();
end
