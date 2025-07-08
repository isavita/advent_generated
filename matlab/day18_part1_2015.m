
function main()
    fid = fopen('input.txt', 'r');
    lines = textscan(fid, '%s', 'Delimiter', '\n');
    fclose(fid);
    
    grid_char = char(lines{1});
    grid = (grid_char == '#');
    
    kernel = [1 1 1; 1 0 1; 1 1 1];
    
    for step = 1:100
        neighbor_counts = conv2(double(grid), kernel, 'same');
        
        new_grid = grid; 
        
        idx_dies = grid & ~(neighbor_counts == 2 | neighbor_counts == 3);
        new_grid(idx_dies) = false;
        
        idx_born = ~grid & (neighbor_counts == 3);
        new_grid(idx_born) = true;
        
        grid = new_grid;
    end
    
    result = sum(grid(:));
    fprintf('%d\n', result);
end
