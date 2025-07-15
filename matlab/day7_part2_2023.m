
function main()
    fid = fopen('input.txt', 'r');
    data = textscan(fid, '%s %d');
    fclose(fid);
    
    hands = data{1};
    bids = data{2};
    num_hands = numel(hands);
    
    scores = zeros(num_hands, 2);

    for i = 1:num_hands
        hand = hands{i};
        
        jokers = sum(hand == 'J');
        non_joker_hand = hand(hand ~= 'J');
        
        if isempty(non_joker_hand)
            counts = 5;
        else
            [~, ~, ic] = unique(non_joker_hand);
            counts = accumarray(ic, 1);
            [~, max_idx] = max(counts);
            counts(max_idx) = counts(max_idx) + jokers;
        end
        
        type_val = sort(counts, 'descend')';
        type = 1;
        if type_val(1) == 5, type = 7;
        elseif type_val(1) == 4, type = 6;
        elseif type_val(1) == 3 && type_val(2) == 2, type = 5;
        elseif type_val(1) == 3, type = 4;
        elseif type_val(1) == 2 && type_val(2) == 2, type = 3;
        elseif type_val(1) == 2, type = 2;
        end
        
        value_str = hand;
        value_str(hand == 'A') = 'E';
        value_str(hand == 'K') = 'D';
        value_str(hand == 'Q') = 'C';
        value_str(hand == 'J') = '1';
        value_str(hand == 'T') = 'A';
        
        scores(i, 1) = type;
        scores(i, 2) = hex2dec(value_str);
    end
    
    all_data = [scores, bids];
    sorted_data = sortrows(all_data, [1, 2]);
    
    ranks = (1:num_hands)';
    total = sum(int64(sorted_data(:, 3)) .* int64(ranks));
    
    fprintf('%d\n', total);
end
