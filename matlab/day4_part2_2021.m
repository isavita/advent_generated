
function main()
    fileID = fopen('input.txt', 'r');
    
    drawn_numbers_str = fgetl(fileID);
    drawn_numbers = str2double(strsplit(drawn_numbers_str, ','));
    
    board_data = fscanf(fileID, '%f');
    fclose(fileID);
    
    BOARD_SIZE = 5;
    num_boards = numel(board_data) / (BOARD_SIZE * BOARD_SIZE);
    boards = permute(reshape(board_data, BOARD_SIZE, BOARD_SIZE, num_boards), [2, 1, 3]);

    picked = false(size(boards));
    already_won = false(1, num_boards);
    last_winning_score = 0;

    for num = drawn_numbers
        picked(boards == num) = true;

        for k = 1:num_boards
            if ~already_won(k)
                board_k_picked = picked(:, :, k);
                is_winner = any(all(board_k_picked, 1)) || any(all(board_k_picked, 2));
                
                if is_winner
                    already_won(k) = true;
                    unmarked_sum = sum(boards(:, :, k)(~board_k_picked));
                    last_winning_score = unmarked_sum * num;
                end
            end
        end
    end

    fprintf('%d\n', last_winning_score);
end
