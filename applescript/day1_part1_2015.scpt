use AppleScript version "2.4"
use scripting additions

set theFile to POSIX file "input.txt" -- Update this path to where your file is located
set theData to read theFile as «class utf8»

set floor to 0
set position to 0

repeat with i from 1 to count of characters of theData
    set currentCharacter to character i of theData as string
    if currentCharacter is "(" then
        set floor to floor + 1
    else if currentCharacter is ")" then
        set floor to floor - 1
    end if
    if floor is -1 then
        set position to i
        exit repeat
    end if
end repeat

set position to position as string
do shell script "echo " & quoted form of position
