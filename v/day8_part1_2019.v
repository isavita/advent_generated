import os

fn main() {
    data := os.read_file('input.txt') or { panic('Failed to read file') }
    image_data := data.trim_space()
    
    width := 25
    height := 6
    layer_size := width * height
    layers := image_data.len / layer_size

    mut min_zero_layer := -1
    mut min_zero_count := layer_size + 1

    for i in 0 .. layers {
        layer := image_data[i * layer_size..(i + 1) * layer_size]
        zero_count := layer.count('0')

        if zero_count < min_zero_count {
            min_zero_count = zero_count
            min_zero_layer = i
        }
    }

    best_layer := image_data[min_zero_layer * layer_size..(min_zero_layer + 1) * layer_size]
    count_1 := best_layer.count('1')
    count_2 := best_layer.count('2')

    result := count_1 * count_2
    println(result)
}