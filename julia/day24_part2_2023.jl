
using LinearAlgebra

const RationalNQ = Rational{BigInt}

struct RatVec3
    x::RationalNQ
    y::RationalNQ
    z::RationalNQ
end

import Base: +, -, *, /

+(a::RatVec3, b::RatVec3) = RatVec3(a.x + b.x, a.y + b.y, a.z + b.z)
-(a::RatVec3, b::RatVec3) = RatVec3(a.x - b.x, a.y - b.y, a.z - b.z)
*(v::RatVec3, s::RationalNQ) = RatVec3(v.x * s, v.y * s, v.z * s)
*(s::RationalNQ, v::RatVec3) = v * s # Commutativity
/(v::RatVec3, s::RationalNQ) = RatVec3(v.x / s, v.y / s, v.z / s)

cross(a::RatVec3, b::RatVec3) = RatVec3(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
)
dot(a::RatVec3, b::RatVec3) = a.x * b.x + a.y * b.y + a.z * b.z

struct HailStone
    p::RatVec3
    v::RatVec3
end

-(a::HailStone, b::HailStone) = HailStone(a.p - b.p, a.v - b.v)

function parse_line(line::String)::HailStone
    nums = map(m -> parse(BigInt, m.match), eachmatch(r"-?\d+", line))
    p = RatVec3(RationalNQ(nums[1]), RationalNQ(nums[2]), RationalNQ(nums[3]))
    v = RatVec3(RationalNQ(nums[4]), RationalNQ(nums[5]), RationalNQ(nums[6]))
    return HailStone(p, v)
end

function read_hailstones(lines::Vector{String})::Vector{HailStone}
    return [parse_line(line) for line in lines]
end

function intersection_time(r::HailStone, s::HailStone)::RationalNQ
    plane_normal = cross(r.p, r.p + r.v)
    denominator = dot(s.v, plane_normal)
    # Assuming non-degenerate input guarantees denominator != 0
    return -dot(s.p, plane_normal) / denominator
end

function solve(lines::Vector{String})::BigInt
    hailstones = read_hailstones(lines[1:3])
    s0 = hailstones[1]
    s1 = hailstones[2]
    s2 = hailstones[3]

    ref1 = s1 - s0
    ref2 = s2 - s0

    t1 = intersection_time(ref2, ref1)
    t2 = intersection_time(ref1, ref2)

    rock_pos1 = s1.p + s1.v * t1
    rock_pos2 = s2.p + s2.v * t2

    rock_v = (rock_pos2 - rock_pos1) / (t2 - t1)
    rock_p = rock_pos1 - rock_v * t1

    total_pos = rock_p.x + rock_p.y + rock_p.z
    
    # Problem guarantees integer result
    return numerator(total_pos)
end

function main()
    lines = readlines("input.txt")
    result = solve(lines)
    println(result)
end

main()
