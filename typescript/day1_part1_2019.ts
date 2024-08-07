import * as fs from 'fs';

const calculateFuel = (mass: number): number => Math.floor(mass / 3) - 2;

const totalFuelRequirement = (masses: number[]): number => 
  masses.reduce((total, mass) => total + calculateFuel(mass), 0);

const readInput = (filePath: string): number[] => {
  const content = fs.readFileSync(filePath, 'utf-8');
  return content.split('\n').map(line => parseInt(line, 10)).filter(Boolean);
};

const main = () => {
  const masses = readInput('input.txt');
  const totalFuel = totalFuelRequirement(masses);
  console.log(totalFuel);
};

main();