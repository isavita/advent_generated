
BEGIN {
    FS=""
    minZeros = 999999
    result = 0
    layer = ""
}

{
    layer = layer $0
}

END {
    layers = length(layer) / (25*6)
    for (i = 0; i < layers; i++) {
        zeros = 0
        ones = 0
        twos = 0
        for (j = 0; j < 25*6; j++) {
            pixel = substr(layer, i*25*6 + j + 1, 1)
            if (pixel == "0") {
                zeros++
            } else if (pixel == "1") {
                ones++
            } else if (pixel == "2") {
                twos++
            }
        }
        if (zeros < minZeros) {
            minZeros = zeros
            result = ones * twos
        }
    }
    print result
}
