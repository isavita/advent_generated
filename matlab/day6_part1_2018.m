
function main()
    points = readPoints('input.txt');
    
    min_x = min(points(:,1));
    max_x = max(points(:,1));
    min_y = min(points(:,2));
    max_y = max(points(:,2));
    
    area_counts = zeros(size(points, 1), 1);
    infinite_areas = false(size(points, 1), 1);
    
    for x = min_x:max_x
        for y = min_y:max_y
            distances = sum(abs(points - [x, y]), 2);
            [min_dist, closest_point_idx] = min(distances);
            
            if sum(distances == min_dist) == 1
                area_counts(closest_point_idx) = area_counts(closest_point_idx) + 1;
                
                if x == min_x || x == max_x || y == min_y || y == max_y
                    infinite_areas(closest_point_idx) = true;
                end
            end
        end
    end
    
    finite_areas = area_counts(~infinite_areas);
    largest_area = max(finite_areas);
    
    fprintf('%d\n', largest_area);
end

function points = readPoints(filename)
    fid = fopen(filename, 'r');
    data = textscan(fid, '%d,%d');
    fclose(fid);
    points = cell2mat(data);
end

main();
