
function main()
    fid = fopen('input.txt', 'r');
    image_data = fscanf(fid, '%c');
    fclose(fid);
    image_data = strtrim(image_data);

    width = 25;
    height = 6;
    layer_size = width * height;
    num_layers = numel(image_data) / layer_size;

    layers = cell(1, num_layers);
    for i = 1:num_layers
        start = (i - 1) * layer_size + 1;
        layers{i} = image_data(start : start + layer_size - 1);
    end

    final_image = repmat(' ', 1, layer_size);
    for i = 1:layer_size
        for j = 1:num_layers
            pixel = layers{j}(i);
            if pixel ~= '2'
                final_image(i) = ifelse(pixel == '1', '#', ' ');
                break;
            end
        end
    end

    for i = 1:height
        start = (i - 1) * width + 1;
        disp(final_image(start : start + width - 1));
    end
end

function result = ifelse(condition, true_val, false_val)
    if condition
        result = true_val;
    else
        result = false_val;
    end
end

main();
