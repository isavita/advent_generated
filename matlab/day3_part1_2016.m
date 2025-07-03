
function main()
    fid = fopen('input.txt', 'r');
    triangles = textscan(fid, '%d %d %d');
    fclose(fid);

    triangles = cell2mat(triangles);
    
    sorted_triangles = sort(triangles, 2);
    
    possible_triangles = sum(sorted_triangles(:, 1) + sorted_triangles(:, 2) > sorted_triangles(:, 3));
    
    disp(possible_triangles);
end

main();
