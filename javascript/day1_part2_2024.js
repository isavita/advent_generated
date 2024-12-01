const fs = require('fs');

// Function to parse the input file and return the left and right lists
function parseInput(filename) {
  const data = fs.readFileSync(filename, 'utf8');
  const leftList = [];
  const rightList = [];

  data.split('\n').forEach(line => {
    if (line.trim() === '') return;
    const [left, right] = line.split(/\s+/).map(Number);
    leftList.push(left);
    rightList.push(right);
  });

  return { leftList, rightList };
}

// Function to calculate the similarity score
function calculateSimilarityScore(leftList, rightList) {
  // Count occurrences in the right list
  const rightFrequency = rightList.reduce((freq, num) => {
    freq[num] = (freq[num] || 0) + 1;
    return freq;
  }, {});

  // Compute the similarity score
  return leftList.reduce((score, num) => {
    return score + (num * (rightFrequency[num] || 0));
  }, 0);
}

// Main function
function main() {
  const filename = 'input.txt';
  const { leftList, rightList } = parseInput(filename);
  const similarityScore = calculateSimilarityScore(leftList, rightList);
  console.log('Similarity score:', similarityScore);
}

// Execute the main function
main();
