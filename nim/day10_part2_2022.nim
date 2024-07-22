
import strutils, sequtils, os

# Function to execute the instructions and calculate signal strengths
proc executeInstructions(filename: string) =
  let lines = readFile(filename).splitLines()
  var X = 1
  var cycle = 0
  var signalStrengths = 0
  var crt = newSeq[char](240) # 40 columns * 6 rows
  var spritePos: int

  for line in lines:
    if line.startsWith("addx"):
      let value = line.split(' ')[1].parseInt()
      for _ in 0..1: # addx takes 2 cycles
        cycle.inc()
        spritePos = X - 1
        if (cycle - 1) mod 40 in spritePos .. (spritePos + 2):
          crt[cycle - 1] = '#'
        else:
          crt[cycle - 1] = '.'
      X += value
    elif line.startsWith("noop"):
      cycle.inc()
      spritePos = X - 1
      if (cycle - 1) mod 40 in spritePos .. (spritePos + 2):
        crt[cycle - 1] = '#'
      else:
        crt[cycle - 1] = '.'

    # Calculate signal strength at specific cycles
    if cycle in {20, 60, 100, 140, 180, 220}:
      signalStrengths += cycle * X

  # Print the signal strengths
  echo "Sum of signal strengths: ", signalStrengths

  # Print the CRT output
  for row in 0 .. 5:
    echo crt[row * 40 .. (row + 1) * 40 - 1].join("")

# Main execution
proc main() =
  executeInstructions("input.txt")

main()
