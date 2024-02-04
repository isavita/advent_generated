const fs = require('fs');

function itemPriority(item) {
    if (item >= 'a' && item <= 'z') {
        return item.charCodeAt(0) - 'a'.charCodeAt(0) + 1;
    }
    return item.charCodeAt(0) - 'A'.charCodeAt(0) + 27;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error opening file:", err);
        return;
    }

    let sum = 0;
    let groupLineCounter = 0;
    let groupItems = [{}, {}, {}];
    const lines = data.split('\n');

    lines.forEach(line => {
        const itemsMap = {};
        for (let i = 0; i < line.length; i++) {
            const item = line[i];
            itemsMap[item] = (itemsMap[item] || 0) + 1;
        }
        groupItems[groupLineCounter] = itemsMap;
        groupLineCounter++;

        if (groupLineCounter === 3) {
            const commonItems = {};
            for (let item in groupItems[0]) {
                if (groupItems[1][item] > 0 && groupItems[2][item] > 0) {
                    commonItems[item] = 1;
                }
            }
            for (let item in commonItems) {
                sum += itemPriority(item);
                break; // Since we need only one common item per group
            }
            groupLineCounter = 0;
        }
    });

    console.log(sum);
});