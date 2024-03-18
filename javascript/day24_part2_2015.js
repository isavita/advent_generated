const fs = require('fs');

function canSplit(packages, firstGroupComb, targetWeight) {
  const remainingPackages = packages.filter((_, i) => (firstGroupComb & (1 << i)) === 0);
  for (let comb1 = 1; comb1 < (1 << remainingPackages.length); comb1++) {
    let group1Weight = 0;
    for (let i = 0; i < remainingPackages.length; i++) {
      if (comb1 & (1 << i)) {
        group1Weight += remainingPackages[i];
      }
    }
    if (group1Weight === targetWeight) {
      for (let comb2 = 1; comb2 < (1 << remainingPackages.length); comb2++) {
        if ((comb1 & comb2) === 0) {
          let group2Weight = 0;
          for (let i = 0; i < remainingPackages.length; i++) {
            if (comb2 & (1 << i)) {
              group2Weight += remainingPackages[i];
            }
          }
          if (group2Weight === targetWeight) {
            return true;
          }
        }
      }
    }
  }
  return false;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const lines = data.trim().split('\n');
  const packages = lines.map(line => parseInt(line, 10));
  const totalWeight = packages.reduce((sum, weight) => sum + weight, 0);
  const targetWeight = Math.floor(totalWeight / 4);
  let bestQE = Number.MAX_SAFE_INTEGER;
  let bestLength = Number.MAX_SAFE_INTEGER;

  for (let comb = 1; comb < (1 << packages.length); comb++) {
    let groupWeight = 0;
    let qe = 1;
    let groupLength = 0;
    for (let i = 0; i < packages.length; i++) {
      if (comb & (1 << i)) {
        groupWeight += packages[i];
        qe *= packages[i];
        groupLength++;
      }
    }
    if (groupWeight === targetWeight && groupLength <= bestLength) {
      if (groupLength < bestLength || qe < bestQE) {
        if (canSplit(packages, comb, targetWeight)) {
          bestLength = groupLength;
          bestQE = qe;
        }
      }
    }
  }

  console.log(bestQE);
});