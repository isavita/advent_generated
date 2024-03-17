function readInputFromFile(filename)
	local file = io.open(filename, "r")
	if not file then
		return nil
	end
	
	local input = file:read("*line")
	file:close()
	
	return input
end

function runIntcode(program)
	memory = {}
	ip = 1
	relativeBase = 0
	output = 0
	
	for s in program:gmatch("[^,]+") do
		memory[#memory + 1] = tonumber(s)
	end
	
	while true do
		opcode = memory[ip] % 100
		modes = tostring(math.floor(memory[ip] / 100))
		
		function getParam(offset)
			mode = 0
			if #modes >= offset then
				mode = tonumber(modes:sub(-offset, -offset))
			end
			
			param = memory[ip + offset]
			if mode == 0 then
				return memory[param + 1]
			elseif mode == 1 then
				return param
			elseif mode == 2 then
				return memory[relativeBase + param + 1]
			else
				error("unknown parameter mode")
			end
		end
		
		function setParam(offset, value)
			mode = 0
			if #modes >= offset then
				mode = tonumber(modes:sub(-offset, -offset))
			end
			
			param = memory[ip + offset]
			if mode == 0 then
				memory[param + 1] = value
			elseif mode == 2 then
				memory[relativeBase + param + 1] = value
			else
				error("unknown parameter mode")
			end
		end
		
		if opcode == 1 then
			setParam(3, getParam(1) + getParam(2))
			ip = ip + 4
		elseif opcode == 2 then
			setParam(3, getParam(1) * getParam(2))
			ip = ip + 4
		elseif opcode == 3 then
			setParam(1, 1) -- Test mode input
			ip = ip + 2
		elseif opcode == 4 then
			output = getParam(1)
			ip = ip + 2
		elseif opcode == 5 then
			if getParam(1) ~= 0 then
				ip = getParam(2) + 1
			else
				ip = ip + 3
			end
		elseif opcode == 6 then
			if getParam(1) == 0 then
				ip = getParam(2) + 1
			else
				ip = ip + 3
			end
		elseif opcode == 7 then
			if getParam(1) < getParam(2) then
				setParam(3, 1)
			else
				setParam(3, 0)
			end
			ip = ip + 4
		elseif opcode == 8 then
			if getParam(1) == getParam(2) then
				setParam(3, 1)
			else
				setParam(3, 0)
			end
			ip = ip + 4
		elseif opcode == 9 then
			relativeBase = relativeBase + getParam(1)
			ip = ip + 2
		elseif opcode == 99 then
			return output
		else
			error("unknown opcode: " .. opcode)
		end
	end
end

program = readInputFromFile("input.txt")
print(runIntcode(program))