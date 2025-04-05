
#!/bin/bash

main() {
    local width=25
    local height=6
    local image_data
    image_data=$(< input.txt)

    local layer_size=$((width * height))
    local data_len=${#image_data}

    for (( i=0; i < layer_size; i++ )); do
        local final_pixel='2'

        for (( offset=i; offset < data_len; offset+=layer_size )); do
            local current_pixel=${image_data:offset:1}
            if [[ "$current_pixel" != "2" ]]; then
                final_pixel="$current_pixel"
                break
            fi
        done

        local output_char=" "
        if [[ "$final_pixel" == "1" ]]; then
            output_char="#"
        fi

        printf "%s" "$output_char"

        if (( (i + 1) % width == 0 )); then
            printf "\n"
        fi
    done
}

main
