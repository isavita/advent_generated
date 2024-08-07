import * as fs from 'fs';

interface Policy {
    min: number;
    max: number;
    letter: string;
    password: string;
}

const parseInput = (data: string): Policy[] => {
    return data.trim().split('\n').map(line => {
        const [policy, password] = line.split(': ');
        const [range, letter] = policy.split(' ');
        const [min, max] = range.split('-').map(Number);
        return { min, max, letter, password };
    });
};

const isValidPartOne = ({ min, max, letter, password }: Policy): boolean => {
    const count = [...password].filter(char => char === letter).length;
    return count >= min && count <= max;
};

const isValidPartTwo = ({ min, max, letter, password }: Policy): boolean => {
    const firstPosition = password[min - 1] === letter;
    const secondPosition = password[max - 1] === letter;
    return (firstPosition || secondPosition) && !(firstPosition && secondPosition);
};

const countValidPasswords = (policies: Policy[], validator: (policy: Policy) => boolean): number => {
    return policies.filter(validator).length;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf8');
    const policies = parseInput(data);

    const validCountPartOne = countValidPasswords(policies, isValidPartOne);
    const validCountPartTwo = countValidPasswords(policies, isValidPartTwo);

    console.log(`Valid passwords (Part One): ${validCountPartOne}`);
    console.log(`Valid passwords (Part Two): ${validCountPartTwo}`);
};

main();