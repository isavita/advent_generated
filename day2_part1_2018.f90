
program solution
  implicit none
  character(len=255) :: id
  integer :: twoCount, threeCount, iunit, twos, threes, checksum
  logical :: hasTwos, hasThrees

  open(unit=10, file='input.txt', status='old')
  twoCount = 0
  threeCount = 0

  do
    read(10, '(A)', iostat=iunit) id
    if (iunit /= 0) exit

    hasTwos = .false.
    hasThrees = .false.

    call countTwosAndThrees(id, hasTwos, hasThrees)

    if (hasTwos) then
      twoCount = twoCount + 1
    endif
    if (hasThrees) then
      threeCount = threeCount + 1
    endif
  end do

  close(10)

  checksum = twoCount * threeCount
  print *, checksum

contains

  subroutine countTwosAndThrees(id, hasTwos, hasThrees)
    character(len=255), intent(in) :: id
    logical, intent(out) :: hasTwos, hasThrees
    integer :: charCount(255)
    integer :: i, count

    charCount = 0
    do i = 1, len_trim(id)
      count = ichar(id(i:i))
      charCount(count) = charCount(count) + 1
    end do

    hasTwos = .false.
    hasThrees = .false.

    do i = 1, 255
      if (charCount(i) == 2) then
        hasTwos = .true.
      else if (charCount(i) == 3) then
        hasThrees = .true.
      endif
    end do
  end subroutine countTwosAndThrees

end program solution
