
program instructions
    implicit none
    integer, parameter :: max_lines = 1000
    character(len=3) :: ops(max_lines)
    character(len=1) :: regs(max_lines)
    integer :: offsets(max_lines)
    integer :: a, b, i, n, offset
    character(len=20) :: line
    
    open(unit=10, file="input.txt", status="old")
    n = 0
    do
        read(10, '(A)', end=100) line
        n = n + 1
        if (line(1:3) == 'hlf' .or. line(1:3) == 'tpl' .or. line(1:3) == 'inc') then
            ops(n) = line(1:3)
            regs(n) = line(5:5)
        else if (line(1:3) == 'jmp') then
            ops(n) = line(1:3)
            read(line(5:), *) offsets(n)
        else if (line(1:3) == 'jie' .or. line(1:3) == 'jio') then
            ops(n) = line(1:3)
            regs(n) = line(5:5)
            read(line(8:), *) offsets(n)
        end if
    end do
    
100 close(10)
    
    a = 0
    b = 0
    i = 1
    
    do while (i <= n)
        if (ops(i) == 'hlf') then
            if (regs(i) == 'a') a = a / 2
            if (regs(i) == 'b') b = b / 2
        else if (ops(i) == 'tpl') then
            if (regs(i) == 'a') a = a * 3
            if (regs(i) == 'b') b = b * 3
        else if (ops(i) == 'inc') then
            if (regs(i) == 'a') a = a + 1
            if (regs(i) == 'b') b = b + 1
        else if (ops(i) == 'jmp') then
            i = i + offsets(i) -1
        else if (ops(i) == 'jie') then
            if (regs(i) == 'a' .and. mod(a, 2) == 0) i = i + offsets(i) - 1
            if (regs(i) == 'b' .and. mod(b, 2) == 0) i = i + offsets(i) - 1
        else if (ops(i) == 'jio') then
            if (regs(i) == 'a' .and. a == 1) i = i + offsets(i) - 1
            if (regs(i) == 'b' .and. b == 1) i = i + offsets(i) - 1
        end if
        i = i + 1
    end do
    
    print *, b
    
end program instructions
