import * as fs from 'fs';
import * as crypto from 'crypto';

function findPassword(doorId: string): string {
    let password = '';
    let index = 0;

    while (password.length < 8) {
        const hash = crypto.createHash('md5').update(doorId + index).digest('hex');
        if (hash.startsWith('00000')) {
            password += hash[5];
        }
        index++;
    }

    return password;
}

function main() {
    fs.readFile('input.txt', 'utf8', (err, data) => {
        if (err) {
            console.error('Error reading file:', err);
            return;
        }

        const doorId = data.trim();
        const password = findPassword(doorId);
        console.log(`The password is: ${password}`);
    });
}

main();