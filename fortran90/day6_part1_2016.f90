program error_corrected_message
  implicit none
  integer, parameter :: max_length = 100
  integer :: i, j, k, n
  character(len=max_length) :: line
  character(len=max_length) :: message
  integer :: counts(max_length, 26)
  character :: most_frequent

  open(unit=10, file='input.txt', status='old')

  ! Initialize counts array
  counts = 0

  ! Read input file and count occurrences of each character
  do
    read(10, '(a)', iostat=i) line
    if (i /= 0) exit
    do j = 1, len(line)
      k = ichar(line(j:j)) - ichar('a') + 1
      counts(j, k) = counts(j, k) + 1
    end do
  end do

  ! Find most frequent character for each position
  message = ''
  do j = 1, max_length
    most_frequent = ' '
    n = 0
    do k = 1, 26
      if (counts(j, k) > n) then
        n = counts(j, k)
        most_frequent = char(ichar('a') + k - 1)
      end if
    end do
    message(j:j) = most_frequent
  end do

  ! Print error-corrected message
  write (*, '(a)') trim(message)

  close(10)
end program error_corrected_message