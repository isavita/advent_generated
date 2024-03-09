def decode_image(image_data, width, height):
    layers = []
    layer_size = width * height
    num_layers = len(image_data) // layer_size

    for i in range(num_layers):
        start = i * layer_size
        end = start + layer_size
        layer = image_data[start:end]
        layers.append(layer)

    final_image = [' '] * layer_size
    for i in range(layer_size):
        for layer in layers:
            if layer[i] != '2':
                final_image[i] = '#' if layer[i] == '1' else ' '
                break

    for i in range(height):
        start = i * width
        end = start + width
        print(''.join(final_image[start:end]))

def main():
    with open('input.txt', 'r') as file:
        image_data = file.read().strip()

    width = 25
    height = 6

    decode_image(image_data, width, height)

if __name__ == '__main__':
    main()
