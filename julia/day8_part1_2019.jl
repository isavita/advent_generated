
data = read("input.txt", String)
imageData = strip(data)

width, height = 25, 6
layerSize = width * height

minZeros = layerSize + 1
result = 0

for i in 1:layerSize:length(imageData)
    layer = imageData[i:min(i+layerSize-1, end)]
    zeroCount, oneCount, twoCount = 0, 0, 0

    for pixel in layer
        if pixel == '0'
            zeroCount += 1
        elseif pixel == '1'
            oneCount += 1
        elseif pixel == '2'
            twoCount += 1
        end
    end

    if zeroCount < minZeros
        global minZeros = zeroCount
        global result = oneCount * twoCount
    end
end

println(result)
