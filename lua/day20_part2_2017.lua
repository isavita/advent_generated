local particles = {}
for line in io.lines("input.txt") do
	local p = {}
	local parts = string.gmatch(line, "%-?%d+")
	for i = 1, 9 do
		p[i] = tonumber(parts())
	end
	particles[#particles + 1] = {p = {p[1], p[2], p[3]}, v = {p[4], p[5], p[6]}, a = {p[7], p[8], p[9]}}
end

for tick = 1, 1000 do
	local positions = {}
	for i, particle in ipairs(particles) do
		for j = 1, 3 do
			particle.v[j] = particle.v[j] + particle.a[j]
			particle.p[j] = particle.p[j] + particle.v[j]
		end
		particles[i] = particle
		local posStr = table.concat(particle.p, ",")
		positions[posStr] = (positions[posStr] or 0) + 1
	end

	local newParticles = {}
	for _, particle in ipairs(particles) do
		local posStr = table.concat(particle.p, ",")
		if positions[posStr] == 1 then
			table.insert(newParticles, particle)
		end
	end
	particles = newParticles
end

print(#particles)