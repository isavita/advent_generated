program binary_boarding
  implicit none
  integer :: row, col, seat_id, max_seat_id, i
  character(len=10) :: boarding_pass
  logical :: file_exists

  max_seat_id = 0

  inquire(file='input.txt', exist=file_exists)
  if (.not. file_exists) then
    write (*,*) 'Error: input file does not exist.'
    stop
  endif

  open(unit=10, file='input.txt', status='old', action='read')

  do
    read(10,*,iostat=i) boarding_pass
    if (i /= 0) exit

    ! Decode row
    row = 0
    do i = 1, 7
      if (boarding_pass(i:i) == 'F') then
        row = row * 2
      else
        row = row * 2 + 1
      endif
    enddo

    ! Decode column
    col = 0
    do i = 8, 10
      if (boarding_pass(i:i) == 'L') then
        col = col * 2
      else
        col = col * 2 + 1
      endif
    enddo

    ! Calculate seat ID
    seat_id = row * 8 + col

    ! Update max seat ID
    if (seat_id > max_seat_id) then
      max_seat_id = seat_id
    endif
  enddo

  close(10)

  write (*,*) 'Highest seat ID:', max_seat_id
end program binary_boarding