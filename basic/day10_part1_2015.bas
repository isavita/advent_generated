
' Day 10: Elves Look, Elves Say

' Function to perform the look-and-say operation on a string
Function LookAndSay(inputString As String) As String
    Dim outputString As String = ""
    Dim i As Integer = 0
    Dim lenInput As Integer = Len(inputString)

    While i < lenInput
        Dim currentChar As String = Mid(inputString, i + 1, 1)
        Dim count As Integer = 1

        ' Count consecutive occurrences of the current character
        While (i + count < lenInput) And (Mid(inputString, i + count + 1, 1) = currentChar)
            count = count + 1
        Wend

        ' Append the count and the character to the output string
        outputString = outputString & Str(count) & currentChar

        ' Move the index past the current run of characters
        i = i + count
    Wend

    Return outputString
End Function

' Main program entry point
Sub Main()
    Dim inputFile As String = "input.txt"
    Dim currentSequence As String
    Dim fileHandle As Integer

    ' Attempt to open the input file
    fileHandle = FreeFile()
    On Error GoTo FileError
    Open inputFile For Input As #fileHandle
    Input #fileHandle, currentSequence
    Close #fileHandle
    On Error GoTo 0 ' Reset error handling

    ' Perform the look-and-say process 40 times
    Dim iterations As Integer = 40
    For i As Integer = 1 To iterations
        currentSequence = LookAndSay(currentSequence)
    Next i

    ' Print the length of the final sequence to standard output
    Print Len(currentSequence)

    Exit Sub ' Exit cleanly

FileError:
    Print "Error: Could not open or read from file '" & inputFile & "'"
    Print "Please ensure '" & inputFile & "' exists and contains the starting sequence."
    End 1 ' Exit with an error code
End Sub

' Call the main sub to start the program
Main()
