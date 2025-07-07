
function crabCombat()
    % CRABCOMBAT Plays a game of Crab Combat and calculates the winning score.
    % Reads player decks from 'input.txt' and prints the winning score to
    % standard output.

    % --- Configuration ---
    INPUT_FILENAME = 'input.txt';

    % --- Main Execution ---
    try
        [player1Deck, player2Deck] = readDecks(INPUT_FILENAME);
    catch ME
        fprintf(2, 'Error reading input file: %s\n', ME.message);
        return;
    end

    [winner, winningDeck] = playGame(player1Deck, player2Deck);

    score = calculateScore(winningDeck);

    fprintf('Player %d wins!\n', winner);
    fprintf('Winning score: %d\n', score);

end

function [player1Deck, player2Deck] = readDecks(filename)
    % READDECKS Reads player decks from the specified file.
    %
    % Args:
    %   filename: The name of the input file.
    %
    % Returns:
    %   player1Deck: A cell array representing Player 1's deck.
    %   player2Deck: A cell array representing Player 2's deck.

    fid = fopen(filename, 'r');
    if fid == -1
        error('Could not open file: %s', filename);
    end
    
    player1Deck = {};
    player2Deck = {};
    currentPlayer = 1;
    
    while ~feof(fid)
        line = strtrim(fgetl(fid));
        
        if isempty(line)
            continue; % Skip empty lines
        elseif strcmp(line, 'Player 1:')
            currentPlayer = 1;
        elseif strcmp(line, 'Player 2:')
            currentPlayer = 2;
        else
            card = str2double(line);
            if isnan(card)
                error('Invalid card value found in file: %s', line);
            end
            
            if currentPlayer == 1
                player1Deck{end+1} = card;
            else
                player2Deck{end+1} = card;
            end
        end
    end
    
    fclose(fid);
    
    % Convert cell arrays to numeric arrays for easier manipulation
    player1Deck = cell2mat(player1Deck);
    player2Deck = cell2mat(player2Deck);
end

function [winner, winningDeck] = playGame(player1Deck, player2Deck)
    % PLAYGAME Simulates a game of Crab Combat.
    %
    % Args:
    %   player1Deck: Numeric array representing Player 1's deck.
    %   player2Deck: Numeric array representing Player 2's deck.
    %
    % Returns:
    %   winner: The winning player (1 or 2).
    %   winningDeck: The deck of the winning player.

    while ~isempty(player1Deck) && ~isempty(player2Deck)
        card1 = player1Deck(1);
        card2 = player2Deck(1);
        
        player1Deck = player1Deck(2:end); % Remove played card
        player2Deck = player2Deck(2:end); % Remove played card
        
        if card1 > card2
            % Player 1 wins the round
            player1Deck = [player1Deck, card1, card2];
        else
            % Player 2 wins the round
            player2Deck = [player2Deck, card2, card1];
        end
    end
    
    if ~isempty(player1Deck)
        winner = 1;
        winningDeck = player1Deck;
    else
        winner = 2;
        winningDeck = player2Deck;
    end
end

function score = calculateScore(deck)
    % CALCULATESCORE Calculates the winning score for a given deck.
    %
    % Args:
    %   deck: A numeric array representing the winning player's deck.
    %
    % Returns:
    %   score: The calculated winning score.

    n = length(deck);
    score = 0;
    for i = 1:n
        score = score + deck(i) * (n - i + 1);
    end
end

% --- Entry Point ---
% To run this program, save it as 'crabCombat.m' and then call
% 'crabCombat' from the MATLAB command window.
%
% Example:
% >> crabCombat
