program matchsticks
  implicit none
  character(len=100) :: line
  integer :: total_code = 0, total_memory = 0, total_encoded = 0

  open(unit=10, file='input.txt', status='old')

  do
    read(10, '(A)', iostat=i) line
    if(i /= 0) exit

    total_code = total_code + len_trim(line)
    total_memory = total_memory + count(line(2:len(line)-1), '""') + 2 * count(line(2:len(line)-1), '\') + 3 * count(line(2:len(line)-1), '\x')
    total_encoded = total_encoded + len_trim(adjustl(adjustl(line, 'T'), 'T')) + 2
  end do

  close(10)

  print*, total_code - total_memory
  print*, total_encoded - total_code

end program matchsticks