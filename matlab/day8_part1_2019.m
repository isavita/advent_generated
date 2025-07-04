
function main
    fid = fopen('input.txt', 'r');
    data = fscanf(fid, '%c');
    fclose(fid);
    data = strtrim(data');

    width = 25;
    height = 6;
    pixels_per_layer = width * height;
    num_layers = floor(length(data) / pixels_per_layer);

    min_zeros = inf;
    result = 0;

    for i = 0:(num_layers - 1)
        layer = data(i*pixels_per_layer + 1 : (i+1)*pixels_per_layer);
        zeros = sum(layer == '0');
        if zeros < min_zeros
            min_zeros = zeros;
            result = sum(layer == '1') * sum(layer == '2');
        end
    end

    disp(result);
end
