const fs = require('fs');

function generateAddresses(mask, address) {
    let floating = [];
    let addresses = [];

    for (let i = 0; i < mask.length; i++) {
        if (mask[i] === '1') {
            address |= (1n << BigInt(35 - i));
        } else if (mask[i] === 'X') {
            floating.push(35 - i);
        }
    }

    let count = 1 << floating.length;
    for (let i = 0; i < count; i++) {
        let modAddress = address;
        for (let j = 0; j < floating.length; j++) {
            if ((i & (1 << j)) === 0) {
                modAddress &= ~(1n << BigInt(floating[j]));
            } else {
                modAddress |= (1n << BigInt(floating[j]));
            }
        }
        addresses.push(modAddress);
    }
    return addresses;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    let lines = data.trim().split('\n');
    let mask = "";
    let mem = new Map();
    let reMem = /mem\[(\d+)] = (\d+)/;

    lines.forEach(line => {
        if (line.startsWith("mask = ")) {
            mask = line.substring(7);
        } else {
            let matches = line.match(reMem);
            if (matches) {
                let address = BigInt(matches[1]);
                let value = BigInt(matches[2]);
                let addresses = generateAddresses(mask, address);
                addresses.forEach(addr => {
                    mem.set(addr, value);
                });
            }
        }
    });

    let sum = 0n;
    mem.forEach(value => {
        sum += value;
    });

    console.log(sum.toString());
});