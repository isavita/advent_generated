
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Unable to open input.txt');
    end
    
    data = textscan(fid, '%d %d %d');
    fclose(fid);
    
    numbers = cell2mat(data);
    
    validTriangles = 0;
    [rows, cols] = size(numbers);
    
    for j = 1:3:rows
        if j + 2 <= rows
            a = numbers(j, :);
            b = numbers(j+1, :);
            c = numbers(j+2, :);
            
            validTriangles = validTriangles + sum(a + b > c & a + c > b & b + c > a);
        end
    end
    
    disp(validTriangles);
end

main();
