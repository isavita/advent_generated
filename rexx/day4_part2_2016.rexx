
/* REXX */

CALL main
EXIT

main:
  total_sector_ids = 0
  DO WHILE LINES('input.txt') > 0
    line = LINEIN('input.txt')
    PARSE VAR line enc_part '[' checksum ']'
    last_hyphen = LASTPOS('-', enc_part)
    name = SUBSTR(enc_part, 1, last_hyphen - 1)
    sector_id = SUBSTR(enc_part, last_hyphen + 1)

    IF is_real_room(name, checksum) THEN DO
      total_sector_ids = total_sector_ids + sector_id
      IF decrypt_name(name, sector_id) = 'northpole object storage' THEN
        SAY sector_id
    END
  END
  SAY total_sector_ids
RETURN

is_real_room:
  PROCEDURE
  PARSE ARG name, checksum
  name_no_hyphens = TRANSLATE(name, '', '-')
  counts. = 0
  alphabet = 'abcdefghijklmnopqrstuvwxyz'
  DO i = 1 TO LENGTH(name_no_hyphens)
    char = SUBSTR(name_no_hyphens, i, 1)
    counts.char = counts.char + 1
  END

  sort_list. = ''
  list_count = 0
  DO i = 1 TO 26
    char = SUBSTR(alphabet, i, 1)
    IF counts.char > 0 THEN DO
      list_count = list_count + 1
      sort_key = RIGHT(999 - counts.char, 3, '0') || char
      sort_list.list_count = sort_key
    END
  END
  sort_list.0 = list_count

  DO i = 1 TO sort_list.0 - 1
    DO j = i + 1 TO sort_list.0
      IF sort_list.i > sort_list.j THEN DO
        temp = sort_list.i
        sort_list.i = sort_list.j
        sort_list.j = temp
      END
    END
  END

  calculated_checksum = ''
  DO i = 1 TO MIN(5, sort_list.0)
    calculated_checksum = calculated_checksum || RIGHT(sort_list.i, 1)
  END
RETURN (calculated_checksum = checksum)

decrypt_name:
  PROCEDURE
  PARSE ARG name, sector_id
  decrypted = ''
  shift = sector_id // 26
  ord_a = C2D('a')
  DO i = 1 TO LENGTH(name)
    char = SUBSTR(name, i, 1)
    IF char = '-' THEN
      decrypted = decrypted || ' '
    ELSE DO
      new_ord = (C2D(char) - ord_a + shift) // 26 + ord_a
      decrypted = decrypted || D2C(new_ord)
    END
  END
RETURN decrypted
