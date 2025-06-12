
/* REXX */
call main
return

main:
    data = LINEIN('input.txt')
    do i = 4 to LENGTH(data)
        marker = SUBSTR(data, i - 3, 4)
        if POS(SUBSTR(marker, 1, 1), SUBSTR(marker, 2)) = 0 &,
           POS(SUBSTR(marker, 2, 1), SUBSTR(marker, 3)) = 0 &,
           POS(SUBSTR(marker, 3, 1), SUBSTR(marker, 4)) = 0 then do
            say i
            leave
        end
    end
return
