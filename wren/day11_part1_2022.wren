
import "io" for File

class Monkey {
  construct new() {
    _items = []
    _op = ""
    _arg = ""
    _div = 0
    _t = 0
    _f = 0
    _ins = 0
  }
  items { _items }
  op=(v) { _op = v }
  op { _op }
  arg=(v) { _arg = v }
  arg { _arg }
  div=(v) { _div = v }
  div { _div }
  t=(v) { _t = v }
  t { _t }
  f=(v) { _f = v }
  f { _f }
  ins { _ins }
  ins=(v) { _ins = v }
}

var content = File.read("input.txt").replace("\r", "")
var blocks = content.split("\n\n")
var monkeys = []

for (block in blocks) {
  if (block.trim() == "") continue
  var m = Monkey.new()
  for (line in block.split("\n")) {
    var t = line.trim()
    if (t.startsWith("Starting items:")) {
      for (item in t.split(": ")[1].split(", ")) {
        m.items.add(Num.fromString(item.trim()))
      }
    } else if (t.startsWith("Operation:")) {
      var p = t.split(" ")
      m.op = p[-2]
      m.arg = p[-1]
    } else if (t.startsWith("Test:")) {
      m.div = Num.fromString(t.split(" ")[-1])
    } else if (t.startsWith("If true:")) {
      m.t = Num.fromString(t.split(" ")[-1])
    } else if (t.startsWith("If false:")) {
      m.f = Num.fromString(t.split(" ")[-1])
    }
  }
  monkeys.add(m)
}

for (round in 1..20) {
  for (m in monkeys) {
    m.ins = m.ins + m.items.count
    while (m.items.count > 0) {
      var item = m.items.removeAt(0)
      var argVal = (m.arg == "old") ? item : Num.fromString(m.arg)
      if (m.op == "*") {
        item = item * argVal
      } else if (m.op == "+") {
        item = item + argVal
      }
      item = (item / 3).floor
      var target = (item % m.div == 0) ? m.t : m.f
      monkeys[target].items.add(item)
    }
  }
}

var ins = monkeys.map { |m| m.ins }.toList
ins.sort()
System.print(ins[-1] * ins[-2])
