
/* REXX */
main:
  NUMERIC DIGITS 20
  target_y = 2000000
  filename = 'input.txt'

  interval.0 = 0
  beacons.0 = 0
  beacon_exists. = 0

  DO WHILE LINES(filename) > 0
    line = LINEIN(filename)
    PARSE VAR line 'Sensor at x=' sx ', y=' sy ': closest beacon is at x=' bx ', y=' by
    dist = ABS(sx - bx) + ABS(sy - by)
    reach = dist - ABS(sy - target_y)

    IF reach >= 0 THEN DO
      j = interval.0 + 1
      interval.j.start = sx - reach
      interval.j.end = sx + reach
      interval.0 = j
    END

    IF by = target_y & beacon_exists.bx = 0 THEN DO
        beacon_exists.bx = 1
        k = beacons.0 + 1
        beacons.k = bx
        beacons.0 = k
    END
  END
  CALL LINEIN filename

  IF interval.0 = 0 THEN DO
    SAY 0
    RETURN
  END

  DO i = 1 TO interval.0 - 1
    DO j = i + 1 TO interval.0
      IF interval.i.start > interval.j.start THEN DO
        ts = interval.i.start; te = interval.i.end
        interval.i.start = interval.j.start; interval.i.end = interval.j.end
        interval.j.start = ts; interval.j.end = te
      END
    END
  END

  merged.0 = 0
  merged_start = interval.1.start
  merged_end = interval.1.end
  DO i = 2 TO interval.0
    IF interval.i.start <= merged_end + 1 THEN
      merged_end = MAX(merged_end, interval.i.end)
    ELSE DO
      k = merged.0 + 1; merged.k.start = merged_start; merged.k.end = merged_end; merged.0 = k
      merged_start = interval.i.start
      merged_end = interval.i.end
    END
  END
  k = merged.0 + 1; merged.k.start = merged_start; merged.k.end = merged_end; merged.0 = k

  total_len = 0
  DO i = 1 TO merged.0
    total_len = total_len + (merged.i.end - merged.i.start + 1)
  END

  beacons_to_subtract = 0
  DO i = 1 TO beacons.0
    bx = beacons.i
    DO j = 1 TO merged.0
      IF bx >= merged.j.start & bx <= merged.j.end THEN DO
        beacons_to_subtract = beacons_to_subtract + 1
        LEAVE
      END
    END
  END

  SAY total_len - beacons_to_subtract
RETURN
