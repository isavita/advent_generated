program day5
  implicit none
  character(len=100000) :: polymer
  integer :: i, j, n
  logical :: reacted

  open(unit=10, file='input.txt', status='old')
  read(10, '(A)') polymer
  close(10)

  n = len_trim(polymer)
  do while (.true.)
    reacted = .false.
    do i = 1, n-1
      if (iachar(polymer(i:i)) /= iachar(polymer(i+1:i+1)) + 32 .and. &
           iachar(polymer(i:i)) /= iachar(polymer(i+1:i+1)) - 32) cycle
      polymer = polymer(1:i-1) // polymer(i+2:n)
      n = n - 2
      reacted = .true.
    end do
    if (.not. reacted) exit
  end do

  print *, 'Units remaining:', n
end program day5