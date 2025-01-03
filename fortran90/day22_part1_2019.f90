
program solve
  implicit none
  integer, parameter :: size = 10007
  integer :: deck(size), i, n, pos
  character(len=256) :: line
  integer :: ios

  do i = 1, size
    deck(i) = i - 1
  end do

  open(unit=10, file="input.txt", status="old", action="read", iostat=ios)
  if (ios /= 0) then
    print *, "Error opening input.txt"
    stop
  end if

  do
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) exit
    
    if (line == "deal into new stack") then
      deck = deal_into_new_stack(deck)
    else if (line(1:3) == "cut") then
      read(line(4:), *, iostat=ios) n
      if (ios == 0) then
        deck = cut_n(deck, n)
      end if
    else if (line(1:19) == "deal with increment") then
      read(line(20:), *, iostat=ios) n
      if (ios == 0) then
        deck = deal_with_increment(deck, n)
      end if
    end if
  end do

  close(10)

  pos = find_2019(deck)
  print *, pos

contains

  function deal_into_new_stack(deck) result(new_deck)
    integer, dimension(:), intent(in) :: deck
    integer, dimension(size) :: new_deck
    integer :: i
    do i = 1, size
      new_deck(i) = deck(size - i + 1)
    end do
  end function

  function cut_n(deck, n) result(new_deck)
    integer, dimension(:), intent(in) :: deck
    integer, intent(in) :: n
    integer, dimension(size) :: new_deck
    integer :: i, j
    if (n >= 0) then
      do i = 1, size
        j = mod(i + n - 1, size) + 1
        new_deck(i) = deck(j)
      end do
    else
      do i = 1, size
        j = mod(i + n - 1 + size, size) + 1
        new_deck(i) = deck(j)
      end do
    end if
  end function

  function deal_with_increment(deck, n) result(new_deck)
    integer, dimension(:), intent(in) :: deck
    integer, intent(in) :: n
    integer, dimension(size) :: new_deck
    integer :: i
    do i = 1, size
      new_deck(mod((i-1)*n, size) + 1) = deck(i)
    end do
  end function

  function find_2019(deck) result(pos)
    integer, dimension(:), intent(in) :: deck
    integer :: pos, i
    pos = -1
    do i = 1, size
      if (deck(i) == 2019) then
        pos = i - 1
        exit
      end if
    end do
  end function

end program solve
