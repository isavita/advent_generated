
function main()

MAX_SIZE = 30;
CYCLES = 6;

grid = false(MAX_SIZE, MAX_SIZE, MAX_SIZE, MAX_SIZE);

mid = MAX_SIZE / 2;
offset = mid - 3;

fid = fopen('input.txt', 'r');
file_content = fread(fid, '*char')';
fclose(fid);

file_content = file_content(file_content ~= 10 & file_content ~= 13);
input_grid_2d = reshape(file_content, 8, 8)';
input_grid_2d = (input_grid_2d == '#');

grid(mid+1, mid+1, (offset+1):(offset+8), (offset+1):(offset+8)) = input_grid_2d;

kernel = ones(3,3,3,3);

for cycle = 1:CYCLES
    active_neighbors_count = convn(double(grid), kernel, 'same') - double(grid);

    new_grid = false(MAX_SIZE, MAX_SIZE, MAX_SIZE, MAX_SIZE);

    idx_active_stay_active = (grid == true) & (active_neighbors_count == 2 | active_neighbors_count == 3);
    new_grid(idx_active_stay_active) = true;

    idx_inactive_become_active = (grid == false) & (active_neighbors_count == 3);
    new_grid(idx_inactive_become_active) = true;

    grid = new_grid;
end

total_active_cells = sum(grid(:));

fprintf('%d\n', total_active_cells);

end
