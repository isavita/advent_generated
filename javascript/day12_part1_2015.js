const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const jsonData = JSON.parse(data);

const sumNumbers = (data) => {
  let sum = 0;
  switch (typeof data) {
    case 'object':
      if (Array.isArray(data)) {
        data.forEach((v) => {
          sum += sumNumbers(v);
        });
      } else {
        Object.values(data).forEach((v) => {
          sum += sumNumbers(v);
        });
      }
      break;
    case 'number':
      sum += data;
      break;
  }
  return sum;
};

const sum = sumNumbers(jsonData);
console.log(sum);