
program nice_strings
    implicit none
    character(100) :: line
    integer :: num_nice_strings_part1, num_nice_strings_part2

    num_nice_strings_part1 = 0
    num_nice_strings_part2 = 0

    open(unit=10, file='input.txt', status='old')

    do
        read(10, '(A)') line
        if (line(1:1) == '') exit
        if (check_nice_string_part1(line)) num_nice_strings_part1 = num_nice_strings_part1 + 1
        if (check_nice_string_part2(line)) num_nice_strings_part2 = num_nice_strings_part2 + 1
    end do

    close(10)

    print*, "Part 1 - Number of nice strings:", num_nice_strings_part1
    print*, "Part 2 - Number of nice strings:", num_nice_strings_part2

contains

    logical function check_nice_string_part1(str)
        character(len=*), intent(in) :: str
        integer :: i, num_vowels, num_double_letters
        logical :: has_ab_cd_pq_xy

        num_vowels = 0
        num_double_letters = 0
        has_ab_cd_pq_xy = .false.

        do i = 1, len(str)
            if (str(i:i) == 'a' .or. str(i:i) == 'e' .or. str(i:i) == 'i' .or. str(i:i) == 'o' .or. str(i:i) == 'u') num_vowels = num_vowels + 1
            if (i > 1 .and. str(i:i) == str(i-1:i-1)) num_double_letters = num_double_letters + 1
            if (i > 1 .and. (str(i-1:i)//str(i:i) == 'ab' .or. str(i-1:i)//str(i:i) == 'cd' .or. str(i-1:i)//str(i:i) == 'pq' .or. str(i-1:i)//str(i:i) == 'xy')) has_ab_cd_pq_xy = .true.
        end do

        check_nice_string_part1 = (num_vowels >= 3 .and. num_double_letters >= 1 .and. .not. has_ab_cd_pq_xy)
    end function check_nice_string_part1

    logical function check_nice_string_part2(str)
        character(len=*), intent(in) :: str
        integer :: i
        logical :: has_pair_twice, has_repeat_with_one_between

        has_pair_twice = .false.
        has_repeat_with_one_between = .false.

        do i = 1, len(str)-2
            if (index(str(i+2:), str(i:i+1)) /= 0) has_pair_twice = .true.
            if (i < len(str)-1 .and. str(i:i) == str(i+2:i+2)) has_repeat_with_one_between = .true.
        end do

        check_nice_string_part2 = (has_pair_twice .and. has_repeat_with_one_between)
    end function check_nice_string_part2

end program nice_strings
