
program solve
  implicit none
  integer, parameter :: MAX_SIZE = 500
  integer :: nr, nc, r, c, nr2, nc2, h
  integer :: grid(MAX_SIZE, MAX_SIZE)
  integer(kind=8) :: dp(MAX_SIZE, MAX_SIZE)
  integer(kind=8) :: total
  character(len=MAX_SIZE) :: line
  integer :: dirs(4,2) = reshape((/1,0,-1,0,0,1,0,-1/),(/4,2/))

  open(unit=10, file="input.txt", status="old")
  nr = 0
  do
    read(10, "(a)", end=100) line
    nr = nr + 1
    if (nr .eq. 1) then
      nc = len_trim(line)
    end if
    do c = 1, nc
      read(line(c:c), "(i1)") grid(nr, c)
    end do
  end do
100 close(10)

  dp = -1
  total = 0

  do r = 1, nr
    do c = 1, nc
      if (grid(r, c) .eq. 0) then
        total = total + dfs(r, c)
      end if
    end do
  end do

  print *, total

contains

  recursive function dfs(r, c) result(sum)
    implicit none
    integer, intent(in) :: r, c
    integer(kind=8) :: sum
    integer :: d, nr2, nc2, h

    if (dp(r, c) .ne. -1) then
      sum = dp(r, c)
      return
    end if

    h = grid(r, c)
    if (h .eq. 9) then
      dp(r, c) = 1
      sum = 1
      return
    end if

    sum = 0
    do d = 1, 4
      nr2 = r + dirs(d,1)
      nc2 = c + dirs(d,2)
      if (nr2 >= 1 .and. nr2 <= nr .and. nc2 >= 1 .and. nc2 <= nc) then
        if (grid(nr2, nc2) .eq. h + 1) then
          sum = sum + dfs(nr2, nc2)
        end if
      end if
    end do
    dp(r, c) = sum
  end function dfs

end program solve
