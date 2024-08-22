import strutils, sequtils, strformat

type
  TokenKind = enum
    Number, Plus, Times, LParen, RParen

  Token = object
    kind: TokenKind
    value: int

proc tokenize(expr: string): seq[Token] =
  result = @[]
  for c in expr:
    case c
    of '0'..'9':
      result.add(Token(kind: Number, value: ord(c) - ord('0')))
    of '+':
      result.add(Token(kind: Plus))
    of '*':
      result.add(Token(kind: Times))
    of '(':
      result.add(Token(kind: LParen))
    of ')':
      result.add(Token(kind: RParen))
    else:
      discard

proc parse(tokens: seq[Token]): int =
  var stack: seq[int] = @[]
  var opStack: seq[TokenKind] = @[]
  var i = 0

  while i < tokens.len:
    case tokens[i].kind
    of Number:
      stack.add(tokens[i].value)
    of Plus, Times:
      while opStack.len > 0 and opStack[^1] == Plus:
        let right = stack.pop
        let left = stack.pop
        stack.add(left + right)
        opStack.delete(opStack.len - 1)
      opStack.add(tokens[i].kind)
    of LParen:
      opStack.add(tokens[i].kind)
    of RParen:
      while opStack.len > 0 and opStack[^1] != LParen:
        let right = stack.pop
        let left = stack.pop
        case opStack.pop
        of Plus:
          stack.add(left + right)
        of Times:
          stack.add(left * right)
        else:
          discard
      opStack.delete(opStack.len - 1)
    else:
      discard
    inc i

  while opStack.len > 0:
    let right = stack.pop
    let left = stack.pop
    case opStack.pop
    of Plus:
      stack.add(left + right)
    of Times:
      stack.add(left * right)
    else:
      discard

  stack[0]

proc evalExpr(expr: string): int =
  let tokens = tokenize(expr)
  parse(tokens)

proc main() =
  let input = readFile("input.txt").splitLines()
  var sum = 0
  for line in input:
    sum += evalExpr(line)
  echo sum

when isMainModule:
  main()