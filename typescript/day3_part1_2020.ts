const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

const countTrees = (forest, right, down) => {
	let trees = 0;
	let x = 0;
	const width = forest[0].length;

	for (let y = 0; y < forest.length; y += down) {
		if (forest[y][x % width] === '#') {
			trees++;
		}
		x += right;
	}

	return trees;
};

const trees = countTrees(input, 3, 1);
console.log(trees);