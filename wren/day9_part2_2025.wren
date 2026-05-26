
import "io" for File

class Sort {
  static quicksort(a, low, high) {
    if (low < high) {
      var p = partition(a, low, high)
      quicksort(a, low, p - 1)
      quicksort(a, p + 1, high)
    }
  }
  static partition(a, low, high) {
    var pivot = a[high]
    var i = low - 1
    for (j in low...high) {
      if (a[j] < pivot) {
        i = i + 1
        var temp = a[i]
        a[i] = a[j]
        a[j] = temp
      }
    }
    var temp = a[i + 1]
    a[i + 1] = a[high]
    a[high] = temp
    return i + 1
  }
  static sort(a) {
    if (a.count > 1) quicksort(a, 0, a.count - 1)
    return a
  }
}

class Main {
  static trim(s) {
    var i = 0
    while (i < s.count && (s[i] == " " || s[i] == "\t" || s[i] == "\r" || s[i] == "\n")) i = i + 1
    var j = s.count - 1
    while (j >= i && (s[j] == " " || s[j] == "\t" || s[j] == "\r" || s[j] == "\n")) j = j - 1
    return s[i..j]
  }

  static run() {
    var lines = File.read("input.txt").split("\n")
    var PX = []
    var PY = []
    var hX = {}
    var hY = {}
    for (line in lines) {
      var parts = line.split(",")
      if (parts.count >= 2) {
        var x = Num.fromString(trim(parts[0]))
        var y = Num.fromString(trim(parts[1]))
        PX.add(x)
        PY.add(y)
        hX[x] = true
        hY[y] = true
      }
    }
    var nP = PX.count
    if (nP == 0) {
      System.print("No points found.")
      return
    }
    var UX = hX.keys.toList
    var UY = hY.keys.toList
    Sort.sort(UX)
    Sort.sort(UY)
    var nx = UX.count
    var ny = UY.count
    var mX = {}
    var mY = {}
    for (i in 0...nx) mX[UX[i]] = 2 * i + 1
    for (i in 0...ny) mY[UY[i]] = 2 * i + 1

    var CW = List.filled(2 * nx + 1, 0)
    for (i in 0...nx) {
      CW[2 * i + 1] = 1
      if (i < nx - 1) {
        CW[2 * i + 2] = UX[i + 1] - UX[i] - 1
      }
    }
    CW[0] = 1
    CW[2 * nx] = 1

    var RH = List.filled(2 * ny + 1, 0)
    for (i in 0...ny) {
      RH[2 * i + 1] = 1
      if (i < ny - 1) {
        RH[2 * i + 2] = UY[i + 1] - UY[i] - 1
      }
    }
    RH[0] = 1
    RH[2 * ny] = 1

    var G = List.filled(2 * ny + 1, null)
    for (i in 0..2 * ny) G[i] = List.filled(2 * nx + 1, 0)

    for (i in 0...nP) {
      var p1 = i
      var p2 = (i + 1) % nP
      var gx1 = mX[PX[p1]]
      var gy1 = mY[PY[p1]]
      var gx2 = mX[PX[p2]]
      var gy2 = mY[PY[p2]]
      if (gx1 == gx2) {
        var s = gy1 < gy2 ? gy1 : gy2
        var e = gy1 > gy2 ? gy1 : gy2
        for (y in s..e) {
          if (RH[y] > 0) G[y][gx1] = 1
        }
      } else {
        var s = gx1 < gx2 ? gx1 : gx2
        var e = gx1 > gx2 ? gx1 : gx2
        for (x in s..e) {
          if (CW[x] > 0) G[gy1][x] = 1
        }
      }
    }

    var mW = 2 * nx
    var mH = 2 * ny
    var qx = [0]
    var qy = [0]
    G[0][0] = 2
    var h = 0
    while (h < qx.count) {
      var cx = qx[h]
      var cy = qy[h]
      h = h + 1
      if (cx + 1 <= mW && G[cy][cx + 1] == 0) {
        G[cy][cx + 1] = 2
        qx.add(cx + 1)
        qy.add(cy)
      }
      if (cx - 1 >= 0 && G[cy][cx - 1] == 0) {
        G[cy][cx - 1] = 2
        qx.add(cx - 1)
        qy.add(cy)
      }
      if (cy + 1 <= mH && G[cy + 1][cx] == 0) {
        G[cy + 1][cx] = 2
        qx.add(cx)
        qy.add(cy + 1)
      }
      if (cy - 1 >= 0 && G[cy - 1][cx] == 0) {
        G[cy - 1][cx] = 2
        qx.add(cx)
        qy.add(cy - 1)
      }
    }

    var S = List.filled(mH + 1, null)
    for (i in 0..mH) S[i] = List.filled(mW + 1, 0)

    for (y in 0..mH) {
      var rs = 0
      for (x in 0..mW) {
        var v = (G[y][x] != 2) ? CW[x] * RH[y] : 0
        rs = rs + v
        S[y][x] = rs + (y > 0 ? S[y - 1][x] : 0)
      }
    }

    var getSum = Fn.new {|x1, y1, x2, y2|
      var lx = x1 < x2 ? x1 : x2
      var rx = x1 > x2 ? x1 : x2
      var ly = y1 < y2 ? y1 : y2
      var ry = y1 > y2 ? y1 : y2
      var res = S[ry][rx]
      if (lx > 0) res = res - S[ry][lx - 1]
      if (ly > 0) res = res - S[ly - 1][rx]
      if (lx > 0 && ly > 0) res = res + S[ly - 1][lx - 1]
      return res
    }

    var maxA = 0
    for (i in 0...nP) {
      for (j in i...nP) {
        var a = ((PX[i] - PX[j]).abs + 1) * ((PY[i] - PY[j]).abs + 1)
        if (a > maxA) {
          if (getSum.call(mX[PX[i]], mY[PY[i]], mX[PX[j]], mY[PY[j]]) == a) {
            maxA = a
          }
        }
      }
    }
    System.print("Largest valid area: %(maxA)")
  }
}

Main.run()
