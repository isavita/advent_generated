
program recipes
  implicit none
  integer, parameter :: max_size = 100000000
  integer :: scoreboard(max_size)
  integer :: input, elf1, elf2, new_score, len, i
  character(len=10) :: line
  
  open(unit=10, file="input.txt", status="old", action="read")
  read(10, '(A)') line
  close(10)
  read(line, *) input

  scoreboard(1) = 3
  scoreboard(2) = 7
  elf1 = 1
  elf2 = 2
  len = 2

  do while (len < input + 10)
    new_score = scoreboard(elf1) + scoreboard(elf2)
    if (new_score >= 10) then
      len = len + 1
      scoreboard(len) = new_score / 10
    end if
    len = len + 1
    scoreboard(len) = mod(new_score, 10)
    
    elf1 = mod(elf1 + scoreboard(elf1) , len) + 1
    elf2 = mod(elf2 + scoreboard(elf2) , len) + 1
  end do

  do i = input + 1, input + 10
    write(*, '(I1)', advance='no') scoreboard(i)
  end do
  write(*,*)
end program recipes
