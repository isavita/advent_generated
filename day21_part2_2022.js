const fs = require('fs');

class Monkey {
	constructor(name, val, hasVal, left, right, op) {
		this.name = name;
		this.val = val;
		this.hasVal = hasVal;
		this.left = left;
		this.right = right;
		this.op = op;
	}

	solve() {
		if (this.hasVal) {
			return [this.val, true];
		}

		if (this.left && this.right) {
			const [left, lOk] = this.left.solve();
			const [right, rOk] = this.right.solve();

			if (lOk && rOk) {
				switch (this.op) {
					case "+":
						return [left + right, true];
					case "-":
						return [left - right, true];
					case "*":
						return [left * right, true];
					case "/":
						return [left / right, true];
					case "==":
						if (left === right) {
							return [0, true];
						} else {
							return [1, true];
						}
				}
			}
		}
		return [0, false];
	}

	expect(x) {
		if (this.name === "humn") {
			return x;
		}

		const [left, lOk] = this.left.solve();
		const [right, rOk] = this.right.solve();

		if (!lOk) {
			switch (this.op) {
				case "+":
					return this.left.expect(x - right);
				case "-":
					return this.left.expect(x + right);
				case "*":
					return this.left.expect(x / right);
				case "/":
					return this.left.expect(x * right);
				case "==":
					return this.left.expect(right);
			}
		}

		if (!rOk) {
			switch (this.op) {
				case "+":
					return this.right.expect(x - left);
				case "-":
					return this.right.expect(left - x);
				case "*":
					return this.right.expect(x / left);
				case "/":
					return this.right.expect(left / x);
				case "==":
					return this.right.expect(left);
			}
		}

		throw new Error("Impossible");
	}
}

function parse() {
	const index = {};

	const data = fs.readFileSync("input.txt", "utf8");
	const lines = data.trim().split('\n');
	for (const line of lines) {
		const [goal, value] = line.split(": ");
		initMonkey(goal);

		const num = parseInt(value);
		if (!isNaN(num)) {
			index[goal].val = num;
			index[goal].hasVal = true;
			continue;
		}

		const [left, op, right] = value.split(' ');

		initMonkey(left);
		initMonkey(right);

		index[goal].left = index[left];
		index[goal].op = op;
		index[goal].right = index[right];
	}

	return index;

	function initMonkey(name) {
		if (!index[name]) {
			index[name] = new Monkey(name);
		}
	}
}

const index = parse();

index["humn"].hasVal = false;
index["root"].op = "==";

console.log(index["root"].expect(0));