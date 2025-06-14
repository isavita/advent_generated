
/* Rexx */
Call main
Exit

main:
  Numeric Digits 32
  p. = 0; v. = 0; a. = 0; alive. = 0
  numParticles = 0
  fname = 'input.txt'

  Do i = 1 While Lines(fname) > 0
    line = Linein(fname)
    numParticles = i
    Parse Var line 'p=<' p.i.1','p.i.2','p.i.3'>, v=<'v.i.1','v.i.2','v.i.3'>, a=<'a.i.1','a.i.2','a.i.3'>'
    alive.i = 1
  End
  Call Linein fname

  Do 1000
    Do i = 1 To numParticles
      If alive.i Then Do
        v.i.1 = v.i.1 + a.i.1
        v.i.2 = v.i.2 + a.i.2
        v.i.3 = v.i.3 + a.i.3
        p.i.1 = p.i.1 + v.i.1
        p.i.2 = p.i.2 + v.i.2
        p.i.3 = p.i.3 + v.i.3
      End
    End

    Do i = 1 To numParticles - 1
      If alive.i Then Do
        Do j = i + 1 To numParticles
          If alive.j Then Do
            If p.i.1 = p.j.1 & p.i.2 = p.j.2 & p.i.3 = p.j.3 Then Do
              alive.i = 0
              alive.j = 0
            End
          End
        End
      End
    End
  End

  count = 0
  Do i = 1 To numParticles
    count = count + alive.i
  End

  Say count
Return
