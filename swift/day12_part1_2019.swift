
import Foundation

struct Vec3 {
    var x, y, z: Int
}

struct Moon {
    var pos, vel: Vec3
}

func applyGravity(moons: inout [Moon]) {
    for i in 0..<moons.count {
        for j in i+1..<moons.count {
            moons[i].vel.x += (moons[i].pos.x < moons[j].pos.x) ? 1 : ((moons[i].pos.x > moons[j].pos.x) ? -1 : 0)
            moons[j].vel.x += (moons[j].pos.x < moons[i].pos.x) ? 1 : ((moons[j].pos.x > moons[i].pos.x) ? -1 : 0)
            moons[i].vel.y += (moons[i].pos.y < moons[j].pos.y) ? 1 : ((moons[i].pos.y > moons[j].pos.y) ? -1 : 0)
            moons[j].vel.y += (moons[j].pos.y < moons[i].pos.y) ? 1 : ((moons[j].pos.y > moons[i].pos.y) ? -1 : 0)
            moons[i].vel.z += (moons[i].pos.z < moons[j].pos.z) ? 1 : ((moons[i].pos.z > moons[j].pos.z) ? -1 : 0)
            moons[j].vel.z += (moons[j].pos.z < moons[i].pos.z) ? 1 : ((moons[j].pos.z > moons[i].pos.z) ? -1 : 0)
        }
    }
}

func applyVelocity(moons: inout [Moon]) {
    for i in 0..<moons.count {
        moons[i].pos.x += moons[i].vel.x
        moons[i].pos.y += moons[i].vel.y
        moons[i].pos.z += moons[i].vel.z
    }
}

func totalEnergy(moons: [Moon]) -> Int {
    var total = 0
    for m in moons {
        let pot = abs(m.pos.x) + abs(m.pos.y) + abs(m.pos.z)
        let kin = abs(m.vel.x) + abs(m.vel.y) + abs(m.vel.z)
        total += pot * kin
    }
    return total
}

let fileURL = URL(fileURLWithPath: "input.txt")
let content = try String(contentsOf: fileURL)
let lines = content.components(separatedBy: "\n").filter { !$0.isEmpty }

var moons: [Moon] = []
for line in lines {
    let components = line.replacingOccurrences(of: "<x=", with: "")
                         .replacingOccurrences(of: ", y=", with: ",")
                         .replacingOccurrences(of: ", z=", with: ",")
                         .replacingOccurrences(of: ">", with: "")
                         .components(separatedBy: ",")
    
    if let x = Int(components[0]), let y = Int(components[1]), let z = Int(components[2]) {
        moons.append(Moon(pos: Vec3(x: x, y: y, z: z), vel: Vec3(x: 0, y: 0, z: 0)))
    }
}

for _ in 0..<1000 {
    applyGravity(moons: &moons)
    applyVelocity(moons: &moons)
}

print(totalEnergy(moons: moons))
