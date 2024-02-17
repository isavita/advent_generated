
program solution
    implicit none
    character(len=100) :: firstRow
    integer :: totalRows, safeTilesCount, i, j
    character(len=100) :: currentRow, nextRow

    totalRows = 40
    open(unit=10, file='input.txt', status='old')
    read(10, '(A)') firstRow
    close(10)

    currentRow = firstRow
    safeTilesCount = countChar(currentRow, '.')

    do i = 2, totalRows
        nextRow = ''
        do j = 1, len_trim(currentRow)
            if (isTrap(j-1, j, j+1, currentRow)) then
                nextRow = trim(adjustl(nextRow)) // '^'
            else
                nextRow = trim(adjustl(nextRow)) // '.'
                safeTilesCount = safeTilesCount + 1
            end if
        end do
        currentRow = nextRow
    end do

    print*, safeTilesCount
contains
    logical function isTrap(left, center, right, row)
        integer, intent(in) :: left, center, right
        character(len=*), intent(in) :: row
        character :: l, c, r

        l = safeIfOutOfBounds(left, row)
        c = row(center: center)
        r = safeIfOutOfBounds(right, row)

        isTrap = (l == '^' .and. c == '^' .and. r == '.') .or. &
                 (c == '^' .and. r == '^' .and. l == '.') .or. &
                 (l == '^' .and. c == '.' .and. r == '.') .or. &
                 (r == '^' .and. c == '.' .and. l == '.')
    end function isTrap

    character function safeIfOutOfBounds(index, row)
        integer, intent(in) :: index
        character(len=*), intent(in) :: row

        if (index < 1 .or. index > len_trim(row)) then
            safeIfOutOfBounds = '.'
        else
            safeIfOutOfBounds = row(index: index)
        end if
    end function safeIfOutOfBounds

    integer function countChar(str, char)
        character(len=*), intent(in) :: str
        character, intent(in) :: char
        integer :: i, count

        count = 0
        do i = 1, len_trim(str)
            if (str(i:i) == char) then
                count = count + 1
            end if
        end do
        countChar = count
    end function countChar
end program solution
