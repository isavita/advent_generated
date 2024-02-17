
program nice_strings
    implicit none
    character(100) :: line
    integer :: num_nice_strings, num_vowels, num_double_letters, i, j
    logical :: has_double_letter, has_disallowed_substring

    num_nice_strings = 0

    open(unit=10, file='input.txt', status='old')

    do
        read(10, '(A)', iostat=i) line
        if (i /= 0) exit

        num_vowels = 0
        num_double_letters = 0
        has_double_letter = .false.
        has_disallowed_substring = .false.

        do i = 1, len_trim(line)
            if (line(i:i) == 'a' .or. line(i:i) == 'e' .or. line(i:i) == 'i' .or. &
                line(i:i) == 'o' .or. line(i:i) == 'u') then
                num_vowels = num_vowels + 1
            endif

            if (i > 1 .and. line(i:i) == line(i-1:i-1)) then
                has_double_letter = .true.
            endif

            if (i > 1) then
                if (line(i-1:i) == 'ab' .or. line(i-1:i) == 'cd' .or. &
                    line(i-1:i) == 'pq' .or. line(i-1:i) == 'xy') then
                    has_disallowed_substring = .true.
                endif
            endif
        end do

        if (num_vowels >= 3 .and. has_double_letter .and. .not. has_disallowed_substring) then
            num_nice_strings = num_nice_strings + 1
        endif
    end do

    close(10)

    print*, num_nice_strings
end program nice_strings
