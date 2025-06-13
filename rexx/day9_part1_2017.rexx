
/* REXX */
main:
  parse arg in_file .
  if in_file = '' then in_file = 'input.txt'
  stream = LINEIN(in_file)

  total_score = 0
  current_score = 0
  in_garbage = 0
  i = 1

  DO WHILE i <= LENGTH(stream)
    char = SUBSTR(stream, i, 1)

    if char = '!' then do
      i = i + 2
      iterate
    end

    if in_garbage then do
      if char = '>' then in_garbage = 0
    end
    else do
      select
        when char = '<' then in_garbage = 1
        when char = '{' then current_score = current_score + 1
        when char = '}' then do
          total_score = total_score + current_score
          current_score = current_score - 1
        end
        otherwise nop
      end
    end
    i = i + 1
  end

  SAY total_score
  EXIT
