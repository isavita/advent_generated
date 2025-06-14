
/* REXX */
Call main
Exit

main:
  node_count = 0
  node_id. = 0
  node_name. = ''
  adj. = 0
  filename = 'input.txt'

  Do While lines(filename) > 0
    line = linein(filename)
    Parse Var line name1 '-' name2
    name1 = strip(name1)
    name2 = strip(name2)

    id1 = getNodeId(name1)
    id2 = getNodeId(name2)

    adj.id1.id2 = 1
    adj.id2.id1 = 1
  End
  Call stream filename, 'C', 'CLOSE'

  count = 0
  Do i = 1 To node_count
    Do j = i + 1 To node_count
      If adj.i.j = 1 Then Do
        Do k = j + 1 To node_count
          If adj.j.k = 1 & adj.k.i = 1 Then Do
            If left(node_name.i, 1) = 't' |,
               left(node_name.j, 1) = 't' |,
               left(node_name.k, 1) = 't' Then
              count = count + 1
          End
        End
      End
    End
  End

  Say count
Return

getNodeId:
  Parse Arg name
  id = node_id.name
  If id = 0 Then Do
    node_count = node_count + 1
    id = node_count
    node_id.name = id
    node_name.id = name
  End
Return id
