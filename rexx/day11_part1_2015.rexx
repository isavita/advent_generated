
/* REXX */
main:
    password = STRIP(LINEIN('input.txt'))
    DO FOREVER
        password = increment_password(password)
        IF has_straight(password) & ,
           has_no_forbidden_letters(password) & ,
           has_two_non_overlapping_pairs(password) THEN
            LEAVE
    END
    SAY password
EXIT

increment_password: PROCEDURE
    PARSE ARG s
    DO i = LENGTH(s) BY -1 TO 1
        char = SUBSTR(s, i, 1)
        IF char = 'z' THEN
            s = OVERLAY('a', s, i)
        ELSE DO
            s = OVERLAY(D2C(C2D(char) + 1), s, i)
            RETURN s
        END
    END
    RETURN s

has_straight: PROCEDURE
    PARSE ARG s
    DO i = 1 TO LENGTH(s) - 2
        c1 = C2D(SUBSTR(s, i, 1))
        c2 = C2D(SUBSTR(s, i+1, 1))
        c3 = C2D(SUBSTR(s, i+2, 1))
        IF c1 + 1 = c2 & c2 + 1 = c3 THEN
            RETURN 1
    END
    RETURN 0

has_no_forbidden_letters: PROCEDURE
    PARSE ARG s
    RETURN VERIFY(s, 'abcdefghjkmnpqrstuvwxyz') = 0

has_two_non_overlapping_pairs: PROCEDURE
    PARSE ARG s
    pairs = 0
    i = 1
    DO WHILE i < LENGTH(s)
        IF SUBSTR(s, i, 1) = SUBSTR(s, i+1, 1) THEN DO
            pairs = pairs + 1
            i = i + 2
        END
        ELSE
            i = i + 1
    END
    RETURN pairs >= 2
