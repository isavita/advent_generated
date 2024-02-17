program solution
    implicit none
    character(len=1500) :: imageData
    integer :: width, height, layerSize, minZeros, result
    integer :: i, layerStart, zeroCount, oneCount, twoCount
    character :: pixel

    open(10, file='input.txt', status='old')
    read(10, '(A)') imageData
    close(10)

    width = 25
    height = 6
    layerSize = width * height
    minZeros = layerSize + 1
    result = 0

    do i = 1, len_trim(imageData), layerSize
        layerStart = i
        zeroCount = 0
        oneCount = 0
        twoCount = 0

        do while (layerStart <= min(i+layerSize-1, len_trim(imageData)))
            read(imageData(layerStart:layerStart), '(A)') pixel
            layerStart = layerStart + 1

            select case (pixel)
            case ('0')
                zeroCount = zeroCount + 1
            case ('1')
                oneCount = oneCount + 1
            case ('2')
                twoCount = twoCount + 1
            end select
        end do

        if (zeroCount < minZeros) then
            minZeros = zeroCount
            result = oneCount * twoCount
        end if
    end do

    write(*, '(I0)') result
end program solution