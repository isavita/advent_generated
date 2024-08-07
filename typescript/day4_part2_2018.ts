import * as fs from 'fs';

interface SleepRecord {
    timestamp: Date;
    guardId?: number;
    action: 'falls asleep' | 'wakes up' | 'begins shift';
}

const parseInput = (data: string): SleepRecord[] => {
    return data.trim().split('\n').map(line => {
        const regex = /\[(.+?)\] (Guard #(\d+) begins shift|falls asleep|wakes up)/;
        const match = line.match(regex);
        return {
            timestamp: new Date(match![1]),
            guardId: match![3] ? parseInt(match![3]) : undefined,
            action: match![3] ? 'begins shift' : match![2] as 'falls asleep' | 'wakes up'
        };
    });
};

const analyzeRecords = (records: SleepRecord[]) => {
    const guardSleepMinutes: Map<number, number[]> = new Map();
    let currentGuardId: number | null = null;

    records.forEach(record => {
        if (record.guardId) {
            currentGuardId = record.guardId;
            if (!guardSleepMinutes.has(currentGuardId)) {
                guardSleepMinutes.set(currentGuardId, new Array(60).fill(0));
            }
        } else if (currentGuardId !== null) {
            const minute = record.timestamp.getUTCMinutes();
            if (record.action === 'falls asleep') {
                guardSleepMinutes.get(currentGuardId)![minute] += 1;
            } else if (record.action === 'wakes up') {
                for (let i = minute - 1; i >= 0 && guardSleepMinutes.get(currentGuardId)![i] > 0; i--) {
                    guardSleepMinutes.get(currentGuardId)![i] += 1;
                }
            }
        }
    });

    return guardSleepMinutes;
};

const findMostAsleepGuard = (guardSleepMinutes: Map<number, number[]>): [number, number] => {
    let maxSleep = 0;
    let chosenGuardId = -1;
    let chosenMinute = -1;

    for (const [guardId, minutes] of guardSleepMinutes) {
        const totalSleep = minutes.reduce((sum, count) => sum + count, 0);
        if (totalSleep > maxSleep) {
            maxSleep = totalSleep;
            chosenGuardId = guardId;
            chosenMinute = minutes.indexOf(Math.max(...minutes));
        }
    }

    return [chosenGuardId, chosenMinute];
};

const findMostFrequentSleepMinute = (guardSleepMinutes: Map<number, number[]>): [number, number] => {
    let maxFrequency = 0;
    let chosenGuardId = -1;
    let chosenMinute = -1;

    for (const [guardId, minutes] of guardSleepMinutes) {
        const maxMinuteFrequency = Math.max(...minutes);
        if (maxMinuteFrequency > maxFrequency) {
            maxFrequency = maxMinuteFrequency;
            chosenGuardId = guardId;
            chosenMinute = minutes.indexOf(maxMinuteFrequency);
        }
    }

    return [chosenGuardId, chosenMinute];
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const records = parseInput(input);
    records.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());

    const guardSleepMinutes = analyzeRecords(records);

    const [guard1, minute1] = findMostAsleepGuard(guardSleepMinutes);
    const answer1 = guard1 * minute1;

    const [guard2, minute2] = findMostFrequentSleepMinute(guardSleepMinutes);
    const answer2 = guard2 * minute2;

    console.log(`Strategy 1: ${answer1}`);
    console.log(`Strategy 2: ${answer2}`);
};

main();