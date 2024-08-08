program binary_diagnostic
  implicit none
  integer, parameter :: max_length = 12
  integer :: i, j, n, gamma_rate, epsilon_rate
  integer :: bit_counts(max_length)
  character(len=max_length) :: line
  logical :: file_exists

  ! Initialize bit counts
  bit_counts = 0

  ! Check if file exists
  inquire(file='input.txt', exist=file_exists)
  if (.not. file_exists) then
    write (*,*) 'Error: input.txt not found.'
    stop
  end if

  ! Read input file
  open(unit=10, file='input.txt', status='old')
  n = 0
  do
    read(10, '(a)', iostat=i) line
    if (i /= 0) exit
    n = n + 1
    do j = 1, max_length
      if (line(j:j) == '1') then
        bit_counts(j) = bit_counts(j) + 1
      end if
    end do
  end do
  close(10)

  ! Calculate gamma and epsilon rates
  gamma_rate = 0
  epsilon_rate = 0
  do j = 1, max_length
    if (bit_counts(j) > n/2) then
      gamma_rate = gamma_rate + 2**(max_length-j)
    else
      epsilon_rate = epsilon_rate + 2**(max_length-j)
    end if
  end do

  ! Print result
  write (*,*) 'Power consumption:', gamma_rate * epsilon_rate

end program binary_diagnostic