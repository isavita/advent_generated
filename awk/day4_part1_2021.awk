#!/usr/bin/awk -f
BEGIN {
  file = "input.txt";
  if ((getline line < file) <= 0) exit;
  nseq = split(line, seq, /,/);
  for (i = 1; i <= nseq; i++) seq[i] = seq[i] + 0;

  board_row = 0; board_id = 0;
  while ((getline line < file) > 0) {
    if (line ~ /^[[:space:]]*$/) continue;
    n = split(line, row, /[ \t]+/);
    for (j = 1; j <= 5; j++) numbers[board_id, board_row, j-1] = row[j] + 0;
    board_row++;
    if (board_row == 5) { board_row = 0; board_id++; }
  }
  board_count = board_id;

  for (b = 0; b < board_count; b++) {
    for (r = 0; r < 5; r++) {
      for (c = 0; c < 5; c++) {
        marked[b, r, c] = 0;
      }
    }
  }

  for (nidx = 1; nidx <= nseq; nidx++) {
     num = seq[nidx];
     for (b = 0; b < board_count; b++) {
        for (r = 0; r < 5; r++) {
           for (c = 0; c < 5; c++) {
              if (numbers[b, r, c] == num) marked[b, r, c] = 1;
           }
        }
        win = 0;
        for (r = 0; r < 5 && !win; r++) {
           all = 1;
           for (c = 0; c < 5; c++) if (marked[b, r, c] != 1) { all = 0; break; }
           if (all) win = 1;
        }
        if (!win) {
           for (c = 0; c < 5 && !win; c++) {
              all = 1;
              for (r = 0; r < 5; r++) if (marked[b, r, c] != 1) { all = 0; break; }
              if (all) win = 1;
           }
        }
        if (win) {
           sum = 0;
           for (r = 0; r < 5; r++) {
              for (c = 0; c < 5; c++) {
                 if (marked[b, r, c] != 1) sum += numbers[b, r, c];
              }
           }
           score = sum * num;
           print score;
           exit;
        }
     }
  }
  exit;
}