import strutils

type Elf = ref object
  id: int
  presents: int
  next: Elf

proc solve(filename: string) =
  let file = open(filename)
  let numElves = file.readLine().parseInt()
  file.close()

  var head: Elf = Elf(id: 1, presents: 1)
  var current = head
  for i in 2..numElves:
    current.next = Elf(id: i, presents: 1)
    current = current.next
  current.next = head

  current = head
  while current.next != current:
    current.presents += current.next.presents
    current.next = current.next.next
    current = current.next

  echo "Elf ", current.id, " gets all the presents."

solve("input.txt")