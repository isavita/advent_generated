const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const jsonData = JSON.parse(data);

const sumNumbers = (data) => {
  let sum = 0;
  switch (typeof data) {
    case 'object':
      if (Array.isArray(data)) {
        for (let i = 0; i < data.length; i++) {
          sum += sumNumbers(data[i]);
        }
      } else {
        if (!containsRed(data)) {
          for (let key in data) {
            sum += sumNumbers(data[key]);
          }
        }
      }
      break;
    case 'number':
      sum += data;
      break;
  }
  return sum;
};

const containsRed = (obj) => {
  for (let key in obj) {
    if (obj[key] === 'red') {
      return true;
    }
  }
  return false;
};

const sum = sumNumbers(jsonData);
console.log(sum);