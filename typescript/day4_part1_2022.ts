const fs = require('fs');

function parseRange(r) {
  const parts = r.split("-");
  const start = parseInt(parts[0]);
  const end = parseInt(parts[1]);
  return [start, end];
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error("Error opening file:", err);
    return;
  }

  const lines = data.trim().split('\n');
  let count = 0;

  lines.forEach((line) => {
    const ranges = line.split(",");
    if (ranges.length !== 2) {
      return;
    }
    const [start1, end1] = parseRange(ranges[0]);
    const [start2, end2] = parseRange(ranges[1]);

    if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
      count++;
    }
  });

  console.log(count);
});