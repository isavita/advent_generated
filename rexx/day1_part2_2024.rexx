
/* REXX */
call main
main:
  left_list. = ''
  right_counts. = 0
  count = 0
  fname = 'input.txt'

  do while lines(fname) > 0
    line = linein(fname)
    if line = '' then iterate
    parse var line left right .
    count = count + 1
    left_list.count = left
    right_counts.right = right_counts.right + 1
  end
  call lineout fname

  similarity_score = 0
  do i = 1 to count
    num = left_list.i
    similarity_score = similarity_score + (num * right_counts.num)
  end

  say "Similarity score:" similarity_score
return
