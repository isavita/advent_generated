
/* REXX */
Call main
Exit

main:
  instructions.0 = 0
  Do i = 1 While lines('input.txt') > 0
    instructions.i = linein('input.txt')
    instructions.0 = i
  End
  Call lineout 'input.txt'

  Do i = 1 To instructions.0
    Parse Var instructions.i op .
    If op = 'acc' Then Iterate

    result = executeBootCode(i)
    If result \= 'LOOP' Then Do
      Say result
      Exit
    End
  End
Return

executeBootCode: Procedure Expose instructions.
  Parse Arg change_idx
  acc = 0
  ip = 1
  Drop visited.

  Do Forever
    If ip < 1 | ip > instructions.0 Then Return acc
    If visited.ip = 1 Then Return 'LOOP'
    visited.ip = 1

    Parse Var instructions.ip op arg

    If ip = change_idx Then Do
      If op = 'jmp' Then op = 'nop'
      Else op = 'jmp'
    End

    Select
      When op = 'acc' Then Do
        acc = acc + arg
        ip = ip + 1
      End
      When op = 'jmp' Then ip = ip + arg
      Otherwise ip = ip + 1
    End
  End
