
/* Rexx */
main:
    inputFile = 'input.txt'
    maxStrength = 0
    size = 0
    component. = ''
    used. = 0

    DO WHILE LINES(inputFile) > 0
        line = LINEIN(inputFile)
        size = size + 1
        PARSE VAR line component.size.a '/' component.size.b
    END

    CALL findStrongestBridge 0, 0
    SAY maxStrength
EXIT

findStrongestBridge:
    PROCEDURE EXPOSE maxStrength component. size used.
    ARG port, strength

    maxStrength = MAX(maxStrength, strength)

    DO i = 1 TO size
        IF used.i = 0 THEN DO
            IF component.i.a = port | component.i.b = port THEN DO
                used.i = 1
                IF component.i.a = port THEN
                    nextPort = component.i.b
                ELSE
                    nextPort = component.i.a

                CALL findStrongestBridge nextPort, strength + component.i.a + component.i.b
                used.i = 0
            END
        END
    END
RETURN
