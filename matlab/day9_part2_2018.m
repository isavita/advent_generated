
function main()
    fileID = fopen('input.txt', 'r');
    line = fgetl(fileID);
    fclose(fileID);

    C = sscanf(line, '%d players; last marble is worth %d points');
    players = C(1);
    lastMarble = C(2) * 100;

    scores = zeros(1, players, 'uint64');
    
    next_ptr = zeros(1, lastMarble + 1, 'uint32');
    prev_ptr = zeros(1, lastMarble + 1, 'uint32');

    next_ptr(1) = 1;
    prev_ptr(1) = 1;
    current_idx = 1;

    for marble = 1:lastMarble
        if mod(marble, 23) == 0
            player_idx = mod(marble - 1, players) + 1;
            
            marble_to_remove_idx = current_idx;
            for i = 1:7
                marble_to_remove_idx = prev_ptr(marble_to_remove_idx);
            end
            
            scores(player_idx) = scores(player_idx) + uint64(marble) + uint64(marble_to_remove_idx - 1);
            
            prev_of_removed = prev_ptr(marble_to_remove_idx);
            next_of_removed = next_ptr(marble_to_remove_idx);
            
            next_ptr(prev_of_removed) = next_of_removed;
            prev_ptr(next_of_removed) = prev_of_removed;
            
            current_idx = next_of_removed;
        else
            insert_after_idx = next_ptr(current_idx);
            insert_before_idx = next_ptr(insert_after_idx);
            
            new_marble_idx = marble + 1;
            
            next_ptr(insert_after_idx) = new_marble_idx;
            prev_ptr(new_marble_idx) = insert_after_idx;
            next_ptr(new_marble_idx) = insert_before_idx;
            prev_ptr(insert_before_idx) = new_marble_idx;
            
            current_idx = new_marble_idx;
        end
    end

    fprintf('%d\n', max(scores));
end
