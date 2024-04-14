<?php

$input = file_get_contents("input.txt");
$input = trim($input);

echo part1($input);

function part1($input) {
    $blueprints = parseInput($input);

    $sum = 0;
    foreach ($blueprints as $bp) {
        $st = newState($bp);
        $geodesMade = $st->calcMostGeodes(0, [], 24, 24);

        $sum += $st->blueprint['id'] * $geodesMade;
    }

    return $sum;
}

function newState($blueprint) {
    return new State($blueprint);
}

class State {
    public $blueprint;
    public $ore = 0;
    public $clay = 0;
    public $obsidian = 0;
    public $geode = 0;
    public $oreRobots = 1;
    public $clayRobots = 0;
    public $obsidianRobots = 0;
    public $geodeRobots = 0;

    function __construct($blueprint) {
        $this->blueprint = $blueprint;
    }

    function farm() {
        $this->ore += $this->oreRobots;
        $this->clay += $this->clayRobots;
        $this->obsidian += $this->obsidianRobots;
        $this->geode += $this->geodeRobots;
    }

    function hash($time) {
        return implode(',', [$time, $this->ore, $this->clay, $this->obsidian, $this->geode, $this->oreRobots, $this->clayRobots, $this->obsidianRobots, $this->geodeRobots]);
    }

    function copy() {
        return clone $this;
    }

    function calcMostGeodes($time, $memo, $totalTime, $earliestGeode) {
        if ($time == $totalTime) {
            return $this->geode;
        }

        $h = $this->hash($time);
        if (isset($memo[$h])) {
            return $memo[$h];
        }

        if ($this->geode == 0 && $time > $earliestGeode) {
            return 0;
        }

        $mostGeodes = $this->geode;

        if ($this->ore >= $this->blueprint['oreForGeodeRobot'] &&
            $this->obsidian >= $this->blueprint['obsidianForGeodeRobot']) {
            $cp = $this->copy();

            $cp->farm();

            $cp->ore -= $cp->blueprint['oreForGeodeRobot'];
            $cp->obsidian -= $cp->blueprint['obsidianForGeodeRobot'];
            $cp->geodeRobots++;
            if ($cp->geodeRobots == 1) {
                $earliestGeode = min($earliestGeode, $time + 1);
            }
            $mostGeodes = max($mostGeodes, $cp->calcMostGeodes($time + 1, $memo, $totalTime, $earliestGeode));

            $memo[$h] = $mostGeodes;
            return $mostGeodes;
        }

        if ($time <= $totalTime - 16 &&
            $this->oreRobots < $this->blueprint['oreForObsidianRobot'] * 2 &&
            $this->ore >= $this->blueprint['oreForOreRobot']) {
            $cp = $this->copy();
            $cp->ore -= $cp->blueprint['oreForOreRobot'];

            $cp->farm();

            $cp->oreRobots++;
            $mostGeodes = max($mostGeodes, $cp->calcMostGeodes($time + 1, $memo, $totalTime, $earliestGeode));
        }
        if ($time <= $totalTime - 8 &&
            $this->clayRobots < $this->blueprint['clayForObsidianRobot'] &&
            $this->ore >= $this->blueprint['oreForClayRobot']) {
            $cp = $this->copy();
            $cp->ore -= $cp->blueprint['oreForClayRobot'];

            $cp->farm();

            $cp->clayRobots++;
            $mostGeodes = max($mostGeodes, $cp->calcMostGeodes($time + 1, $memo, $totalTime, $earliestGeode));
        }
        if ($time <= $totalTime - 4 &&
            $this->obsidianRobots < $this->blueprint['obsidianForGeodeRobot'] &&
            $this->ore >= $this->blueprint['oreForObsidianRobot'] && $this->clay >= $this->blueprint['clayForObsidianRobot']) {

            $cp = $this->copy();
            $cp->ore -= $cp->blueprint['oreForObsidianRobot'];
            $cp->clay -= $cp->blueprint['clayForObsidianRobot'];
            $cp->farm();

            $cp->obsidianRobots++;
            $mostGeodes = max($mostGeodes, $cp->calcMostGeodes($time + 1, $memo, $totalTime, $earliestGeode));
        }

        $cp = $this->copy();
        $cp->farm();
        $mostGeodes = max($mostGeodes, $cp->calcMostGeodes($time + 1, $memo, $totalTime, $earliestGeode));

        $memo[$h] = $mostGeodes;
        return $mostGeodes;
    }
}

function parseInput($input) {
    $ans = [];
    $lines = explode("\n", $input);
    foreach ($lines as $line) {
        sscanf($line, "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
            $id, $oreForOreRobot, $oreForClayRobot, $oreForObsidianRobot,
            $clayForObsidianRobot, $oreForGeodeRobot, $obsidianForGeodeRobot);
        $ans[] = [
            'id' => $id,
            'oreForOreRobot' => $oreForOreRobot,
            'oreForClayRobot' => $oreForClayRobot,
            'oreForObsidianRobot' => $oreForObsidianRobot,
            'clayForObsidianRobot' => $clayForObsidianRobot,
            'oreForGeodeRobot' => $oreForGeodeRobot,
            'obsidianForGeodeRobot' => $obsidianForGeodeRobot
        ];
    }
    return $ans;
}