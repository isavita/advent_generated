
#!/usr/bin/awk -f

BEGIN {
    width = 25
    height = 6
    layer_size = width * height

    getline image_data < "input.txt"
    close("input.txt")

    data_len = length(image_data)

    for (i = 1; i <= layer_size; i++) {
        pixel_set[i] = 0
        final_image[i] = " "
    }

    for (i = 1; i <= data_len; i++) {
        pixel_pos = (i - 1) % layer_size + 1
        pixel_val = substr(image_data, i, 1)

        if (!pixel_set[pixel_pos] && pixel_val != "2") {
            final_image[pixel_pos] = (pixel_val == "1" ? "#" : " ")
            pixel_set[pixel_pos] = 1
        }
    }

    for (h = 0; h < height; h++) {
        row = ""
        for (w = 0; w < width; w++) {
            idx = h * width + w + 1
            row = row final_image[idx]
        }
        print row
    }
    exit
}
