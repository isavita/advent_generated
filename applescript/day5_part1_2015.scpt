
use AppleScript version "2.4"
use scripting additions

set theFile to POSIX file "input.txt"
set theData to read theFile

set disallowPattern to {"ab", "cd", "pq", "xy"}
set nice to 0

set oldDelimiters to text item delimiters
set text item delimiters to linefeed
set theLines to text items of theData
set text item delimiters to oldDelimiters

repeat with aLine in theLines
    set vowels to 0
    set hasDouble to false

    repeat with aChar in aLine
        if aChar is in {"a", "e", "i", "o", "u"} then
            set vowels to vowels + 1
        end if
    end repeat

    repeat with i from 1 to (length of aLine) - 1
        set currentChar to character i of aLine
        set nextChar to character (i + 1) of aLine
        if currentChar is nextChar then
            set hasDouble to true
            exit repeat
        end if
    end repeat

    set foundDisallowed to false
    repeat with aPattern in disallowPattern
        if aLine contains aPattern then
            set foundDisallowed to true
            exit repeat
        end if
    end repeat

    if vowels >= 3 and not foundDisallowed and hasDouble then
        set nice to nice + 1
    end if
end repeat

return nice as text
