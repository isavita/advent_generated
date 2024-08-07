import * as fs from 'fs';

interface Component {
    port1: number;
    port2: number;
}

const readComponents = (filename: string): Component[] => {
    const data = fs.readFileSync(filename, 'utf-8');
    return data.trim().split('\n').map(line => {
        const [port1, port2] = line.split('/').map(Number);
        return { port1, port2 };
    });
};

const findStrongestBridge = (components: Component[], currentPort: number, used: boolean[]): number => {
    let maxStrength = 0;

    for (let i = 0; i < components.length; i++) {
        if (!used[i] && (components[i].port1 === currentPort || components[i].port2 === currentPort)) {
            used[i] = true;
            const nextPort = components[i].port1 === currentPort ? components[i].port2 : components[i].port1;
            const strength = components[i].port1 + components[i].port2;
            maxStrength = Math.max(maxStrength, strength + findStrongestBridge(components, nextPort, used));
            used[i] = false;
        }
    }

    return maxStrength;
};

const main = () => {
    const components = readComponents('input.txt');
    const strongestBridgeStrength = findStrongestBridge(components, 0, new Array(components.length).fill(false));
    console.log(strongestBridgeStrength);
};

main();