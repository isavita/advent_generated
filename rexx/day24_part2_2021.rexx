
/* REXX */
Call main
Exit

main:
    fname = 'input.txt'
    i = -1
    Do While Lines(fname) > 0
        i = i + 1
        line = Linein(fname)
        block_idx = i % 18
        rem = i // 18
        If rem = 4  Then l.block_idx = Word(line, 3)
        If rem = 5  Then k.block_idx = Word(line, 3)
        If rem = 15 Then m.block_idx = Word(line, 3)
    End
    Call Stream fname, 'C', 'CLOSE'

    sp = 0
    Do i = 0 To 13
        If l.i = 1 Then Do
            sp = sp + 1
            stack.sp = i
        End
        Else If l.i = 26 Then Do
            pop = stack.sp
            sp = sp - 1
            constraints.pop.0 = i
            constraints.pop.1 = m.pop + k.i
        End
    End

    Do i = 0 To 13; min_val.i = 0; End
    Do i = 0 To 13
        If Symbol('constraints.'||i||'.0') == 'LIT' Then Iterate
        vmin = 1
        Do While (vmin + constraints.i.1) < 1
            vmin = vmin + 1
        End
        min_val.i = vmin
        j = constraints.i.0
        min_val.j = vmin + constraints.i.1
    End

    result = ''
    Do i = 0 To 13
        result = result || min_val.i
    End
    Say result
Return
