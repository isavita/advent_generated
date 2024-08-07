class SnailNumber {
  value: number;
  left: SnailNumber | null;
  right: SnailNumber | null;

  constructor(value: number, left: SnailNumber | null = null, right: SnailNumber | null = null) {
    this.value = value;
    this.left = left;
    this.right = right;
  }

  isRegular(): boolean {
    return this.left === null && this.right === null;
  }

  add(other: SnailNumber): SnailNumber {
    const newNumber = new SnailNumber(-1, this, other);
    return newNumber.reduce();
  }

  reduce(): SnailNumber {
    while (true) {
      const [exploded, _, __] = this.explode(0);
      if (exploded) continue;
      if (!this.split()) break;
    }
    return this;
  }

  explode(depth: number): [boolean, number, number] {
    if (this.isRegular()) return [false, 0, 0];

    if (depth === 4) {
      const leftValue = this.left!.value;
      const rightValue = this.right!.value;
      this.left = null;
      this.right = null;
      this.value = 0;
      return [true, leftValue, rightValue];
    }

    let [exploded, leftValue, rightValue] = this.left!.explode(depth + 1);
    if (exploded) {
      if (rightValue > 0 && this.right !== null) {
        this.right.addLeft(rightValue);
      }
      return [true, leftValue, 0];
    }

    [exploded, leftValue, rightValue] = this.right!.explode(depth + 1);
    if (exploded) {
      if (leftValue > 0 && this.left !== null) {
        this.left.addRight(leftValue);
      }
      return [true, 0, rightValue];
    }

    return [false, 0, 0];
  }

  addLeft(value: number): void {
    if (this.isRegular()) {
      this.value += value;
    } else {
      this.left!.addLeft(value);
    }
  }

  addRight(value: number): void {
    if (this.isRegular()) {
      this.value += value;
    } else {
      this.right!.addRight(value);
    }
  }

  split(): boolean {
    if (this.isRegular()) {
      if (this.value >= 10) {
        this.left = new SnailNumber(Math.floor(this.value / 2));
        this.right = new SnailNumber(Math.ceil(this.value / 2));
        this.value = -1;
        return true;
      }
      return false;
    }
    return this.left!.split() || this.right!.split();
  }

  magnitude(): number {
    if (this.isRegular()) return this.value;
    return 3 * this.left!.magnitude() + 2 * this.right!.magnitude();
  }

  deepCopy(): SnailNumber {
    if (this.isRegular()) return new SnailNumber(this.value);
    return new SnailNumber(-1, this.left!.deepCopy(), this.right!.deepCopy());
  }
}

function parseSnailNumber(input: string): SnailNumber {
  input = input.trim();
  if (input[0] !== '[') return new SnailNumber(parseInt(input));

  let balance = 0;
  let splitIndex = 0;
  for (let i = 1; i < input.length - 1; i++) {
    switch (input[i]) {
      case '[': balance++; break;
      case ']': balance--; break;
      case ',': if (balance === 0) { splitIndex = i; break; }
    }
    if (splitIndex !== 0) break;
  }

  const left = parseSnailNumber(input.slice(1, splitIndex));
  const right = parseSnailNumber(input.slice(splitIndex + 1, input.length - 1));
  return new SnailNumber(-1, left, right);
}

async function main() {
  const fs = require('fs').promises;
  const data = await fs.readFile('input.txt', 'utf8');
  const snailNumbers = data.split('\n').filter((line: string) => line.trim() !== '').map(parseSnailNumber);

  let largestMagnitude = 0;

  for (let i = 0; i < snailNumbers.length; i++) {
    for (let j = 0; j < snailNumbers.length; j++) {
      if (i === j) continue;

      const aCopy = snailNumbers[i].deepCopy();
      const bCopy = snailNumbers[j].deepCopy();

      const sum1 = aCopy.add(bCopy.deepCopy()).magnitude();
      const sum2 = bCopy.add(aCopy.deepCopy()).magnitude();

      largestMagnitude = Math.max(largestMagnitude, sum1, sum2);
    }
  }

  console.log(largestMagnitude);
}

main();