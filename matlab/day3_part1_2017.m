
function main
    fid = fopen('input.txt', 'r');
    num = fscanf(fid, '%d');
    fclose(fid);
    
    root = ceil(sqrt(num));
    if mod(root, 2) == 0
        root = root + 1;
    end
    
    side_length = root - 1;
    steps_to_center = floor(side_length / 2);
    max_num = root^2;
    steps_to_max_num = max_num - num;
    steps_along_side = mod(steps_to_max_num, side_length);
    
    result = steps_to_center + abs(steps_along_side - steps_to_center);
    fprintf('%d\n', result);
end

main;
