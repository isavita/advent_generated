
import "io" for File

class Cookie {
  static run() {
    var contents = File.read("input.txt")
    var capacity = []
    var durability = []
    var flavor = []
    var texture = []
    var calories = []

    for (line in contents.split("\n")) {
      var l = line.trim()
      if (l == "") continue
      var parts = l.replace(",", "").split(" ")
      capacity.add(Num.fromString(parts[2]))
      durability.add(Num.fromString(parts[4]))
      flavor.add(Num.fromString(parts[6]))
      texture.add(Num.fromString(parts[8]))
      calories.add(Num.fromString(parts[10]))
    }

    var n = capacity.count
    var teaspoons = List.filled(n, 0)
    var maxScore = 0

    var rec
    rec = Fn.new {|idx, rem|
      if (idx == n - 1) {
        teaspoons[idx] = rem
        var cal = 0
        for (i in 0...n) {
          cal = cal + calories[i] * teaspoons[i]
        }
        if (cal == 500) {
          var cap = 0
          var dur = 0
          var fla = 0
          var tex = 0
          for (i in 0...n) {
            cap = cap + capacity[i] * teaspoons[i]
            dur = dur + durability[i] * teaspoons[i]
            fla = fla + flavor[i] * teaspoons[i]
            tex = tex + texture[i] * teaspoons[i]
          }
          if (cap < 0) cap = 0
          if (dur < 0) dur = 0
          if (fla < 0) fla = 0
          if (tex < 0) tex = 0
          var score = cap * dur * fla * tex
          if (score > maxScore) maxScore = score
        }
        return
      }
      for (i in 0..rem) {
        teaspoons[idx] = i
        rec.call(idx + 1, rem - i)
      }
    }

    rec.call(0, 100)
    System.print(maxScore)
  }
}

Cookie.run()
