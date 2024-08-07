import * as fs from 'fs';

const calculateFuel = (mass: number): number => Math.max(Math.floor(mass / 3) - 2, 0);

const calculateTotalFuel = (mass: number): number => {
    let totalFuel = 0;
    let fuel = calculateFuel(mass);
    while (fuel > 0) {
        totalFuel += fuel;
        fuel = calculateFuel(fuel);
    }
    return totalFuel;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const masses = input.split('\n').map(Number).filter(Boolean);
    
    const totalFuel = masses.reduce((sum, mass) => sum + calculateTotalFuel(mass), 0);
    
    console.log(totalFuel);
};

main();