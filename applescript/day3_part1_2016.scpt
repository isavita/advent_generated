
use AppleScript version "2.4"
use scripting additions

set theFile to POSIX file "input.txt" -- Specify the file path
set theData to read theFile as «class utf8»

set validTriangles to 0
set triangleList to paragraphs of theData

repeat with triangle in triangleList
	set sides to words of triangle
	if (count of sides) is not 3 then
		log "Invalid input format"
	else
		set a to item 1 of sides as integer
		set b to item 2 of sides as integer
		set c to item 3 of sides as integer
		
		if isValidTriangle(a, b, c) then
			set validTriangles to validTriangles + 1
		end if
	end if
end repeat

on isValidTriangle(a, b, c)
	return (a + b > c) and (a + c > b) and (b + c > a)
end isValidTriangle

log validTriangles
