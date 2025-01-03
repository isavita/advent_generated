
program screen_pixels
  implicit none
  integer, parameter :: screenWidth = 50
  integer, parameter :: screenHeight = 6
  logical :: screen(screenHeight, screenWidth)
  character(len=100) :: line
  integer :: i, j, a, b, count
  
  open(unit=10, file="input.txt", status="old")

  screen = .false.

  do
    read(10, '(A)', iostat=i) line
    if (i /= 0) exit
    call processInstruction(line, screen)
  end do

  close(10)

  count = 0
  do i = 1, screenHeight
    do j = 1, screenWidth
      if (screen(i,j)) count = count + 1
    end do
  end do

  print *, count

contains

  subroutine processInstruction(instruction, screen)
    implicit none
    character(len=*), intent(in) :: instruction
    logical, intent(inout) :: screen(screenHeight, screenWidth)
    integer :: a, b
    
    if (instruction(1:4) == "rect") then
      read(instruction(6:),'(I2,1X,A1,1X,I2)') a, line, b
      call rect(screen, a, b)
    else if (instruction(1:10) == "rotate row") then
      read(instruction(14:),'(I1,3X,A2,1X,I2)') a, line, b
      call rotateRow(screen, a, b)
    else if (instruction(1:13) == "rotate column") then
      read(instruction(17:),'(I2,3X,A2,1X,I2)') a, line, b
      call rotateColumn(screen, a, b)
    end if
  end subroutine

  subroutine rect(screen, a, b)
    implicit none
    logical, intent(inout) :: screen(screenHeight, screenWidth)
    integer, intent(in) :: a, b
    integer :: i, j
    do i = 1, b
      do j = 1, a
        screen(i,j) = .true.
      end do
    end do
  end subroutine

  subroutine rotateRow(screen, row, shift)
    implicit none
    logical, intent(inout) :: screen(screenHeight, screenWidth)
    integer, intent(in) :: row, shift
    logical :: temp(screenWidth)
    integer :: i
    temp = screen(row,:)
    do i = 1, screenWidth
      screen(row, mod(i+shift-1, screenWidth)+1) = temp(i)
    end do
  end subroutine

  subroutine rotateColumn(screen, col, shift)
    implicit none
    logical, intent(inout) :: screen(screenHeight, screenWidth)
    integer, intent(in) :: col, shift
    logical :: temp(screenHeight)
    integer :: i
    temp = screen(:,col)
    do i = 1, screenHeight
      screen(mod(i+shift-1, screenHeight)+1, col) = temp(i)
    end do
  end subroutine

end program screen_pixels
