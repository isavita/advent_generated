const fs = require('fs');

const validatePassword = (policy, password) => {
  const [min, max, char] = policy.split(/[- ]/);
  const count = password.split(char).length - 1;
  return count >= parseInt(min) && count <= parseInt(max);
};

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const lines = data.split('\n');
  let validCount = 0;

  lines.forEach((line) => {
    const [policy, password] = line.split(': ');
    if (validatePassword(policy, password)) {
      validCount++;
    }
  });

  console.log(validCount);
});