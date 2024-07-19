
function main()
    imageData = strip(read("input.txt", String))
    width, height = 25, 6
    layerSize = width * height
    finalImage = fill('2', layerSize)

    for i in 1:layerSize:length(imageData)
        layer = imageData[i:min(i + layerSize - 1, end)]
        for (j, pixel) in enumerate(layer)
            finalImage[j] == '2' && (finalImage[j] = pixel)
        end
    end

    println("Decoded image:")
    for i in 0:(height - 1)
        for j in 0:(width - 1)
            pixel = finalImage[i * width + j + 1]
            print(pixel == '0' ? ' ' : '#')
        end
        println()
    end
end

main()
