
function main()

fid = fopen('input.txt', 'r');
if fid == -1
    error('Error opening input.txt.');
end

line1 = fgetl(fid);
called_numbers_str = strsplit(line1, ',');
called_numbers = str2double(called_numbers_str);

boards_numbers_prealloc = zeros(5, 5, 100); 
board_count = 0;

fgetl(fid); 

while ~feof(fid)
    current_board_numbers = zeros(5, 5);
    is_valid_board = true;
    
    for i = 1:5
        line = fgetl(fid);
        
        if ~ischar(line) || isempty(line)
            is_valid_board = false;
            break; 
        end
        
        data = textscan(line, '%f', 'Delimiter', ' ', 'MultipleDelimsAsOne', true);
        row_numbers = data{1};
        
        if length(row_numbers) ~= 5
            is_valid_board = false;
            break;
        end
        current_board_numbers(i, :) = row_numbers;
    end
    
    if is_valid_board
        board_count = board_count + 1;
        boards_numbers_prealloc(:,:,board_count) = current_board_numbers;
        
        fgetl(fid); 
    else
        break; 
    end
end
fclose(fid);

boards_numbers = boards_numbers_prealloc(:,:,1:board_count);
num_boards = board_count;

boards_marked = false(5, 5, num_boards); 

winning_board_idx = -1;
winning_number = -1;

for k = 1:length(called_numbers)
    current_number = called_numbers(k);

    boards_marked(boards_numbers == current_number) = true;

    row_sums = sum(boards_marked, 2); 
    col_sums = sum(boards_marked, 1); 

    board_wins_row = any(row_sums == 5, 1); 
    board_wins_col = any(col_sums == 5, 2); 
    
    board_has_won = squeeze(board_wins_row | board_wins_col); 

    first_winner_idx = find(board_has_won, 1, 'first');

    if ~isempty(first_winner_idx)
        winning_board_idx = first_winner_idx;
        winning_number = current_number;
        break; 
    end
end

if winning_board_idx ~= -1
    winner_numbers = boards_numbers(:,:,winning_board_idx);
    winner_marked = boards_marked(:,:,winning_board_idx);
    
    unmarked_sum = sum(winner_numbers(~winner_marked));
    
    fprintf('%d\n', unmarked_sum * winning_number);
end

end
