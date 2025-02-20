
<?php

/**
 * Class Blueprint represents a blueprint for robot production.
 */
class Blueprint
{
    public int $id;
    public int $oreRobotCostOre;
    public int $clayRobotCostOre;
    public int $obsidianRobotCostOre;
    public int $obsidianRobotCostClay;
    public int $geodeRobotCostOre;
    public int $geodeRobotCostObsidian;

    /**
     * Blueprint constructor.
     * @param array $blueprintData
     */
    public function __construct(array $blueprintData)
    {
        $this->id = $blueprintData[0];
        $this->oreRobotCostOre = $blueprintData[1];
        $this->clayRobotCostOre = $blueprintData[2];
        $this->obsidianRobotCostOre = $blueprintData[3];
        $this->obsidianRobotCostClay = $blueprintData[4];
        $this->geodeRobotCostOre = $blueprintData[5];
        $this->geodeRobotCostObsidian = $blueprintData[6];
    }
}

/**
 * Calculates the maximum number of geodes that can be opened in a given time.
 *
 * @param Blueprint $blueprint The blueprint to use.
 * @param int $timeLimit The time limit in minutes.
 *
 * @return int The maximum number of geodes.
 */
function maxGeodes(Blueprint $blueprint, int $timeLimit): int
{
    $maxGeodes = 0;
    $queue = [];

    // Initial state: 1 ore robot, no resources, 0 robots of other types, time 0
    $initialState = [
        'oreRobots' => 1,
        'clayRobots' => 0,
        'obsidianRobots' => 0,
        'geodeRobots' => 0,
        'ore' => 0,
        'clay' => 0,
        'obsidian' => 0,
        'geode' => 0,
        'time' => 0,
    ];

    $queue[] = $initialState;

    $maxOreCost = max($blueprint->oreRobotCostOre, $blueprint->clayRobotCostOre, $blueprint->obsidianRobotCostOre, $blueprint->geodeRobotCostOre);
    $seenStates = [];

    while (!empty($queue)) {
        $currentState = array_shift($queue);

        // Update maxGeodes if current state has more
        $maxGeodes = max($maxGeodes, $currentState['geode']);

        // Optimization: if not enough time to potentially improve, skip.
        $remainingTime = $timeLimit - $currentState['time'];
        $potentialGeodes = $currentState['geode'] + $currentState['geodeRobots'] * $remainingTime + ($remainingTime * ($remainingTime-1) / 2);
        if ($potentialGeodes <= $maxGeodes) {
          continue;
        }
        
        // Optimization to remove redundant states.  Only store the resources we *could possibly need*.
        $currentState['ore'] = min($currentState['ore'], $remainingTime * $maxOreCost - $currentState['oreRobots'] * ($remainingTime - 1));
        $currentState['clay'] = min($currentState['clay'], $remainingTime * $blueprint->obsidianRobotCostClay - $currentState['clayRobots'] * ($remainingTime - 1));
        $currentState['obsidian'] = min($currentState['obsidian'], $remainingTime * $blueprint->geodeRobotCostObsidian - $currentState['obsidianRobots'] * ($remainingTime - 1));

        $stateKey = hash('sha256', json_encode($currentState));
        if(isset($seenStates[$stateKey])) {
            continue;
        }
        $seenStates[$stateKey] = true;


        // If we reached the time limit, no more actions
        if ($currentState['time'] == $timeLimit) {
            continue;
        }

        // Collect resources
        $nextState = $currentState;
        $nextState['ore'] += $nextState['oreRobots'];
        $nextState['clay'] += $nextState['clayRobots'];
        $nextState['obsidian'] += $nextState['obsidianRobots'];
        $nextState['geode'] += $nextState['geodeRobots'];
        $nextState['time']++;

        // Option 1: Do nothing (always an option)
        $queue[] = $nextState;

        // Option 2: Build an ore robot if possible and helpful
        if ($currentState['ore'] >= $blueprint->oreRobotCostOre && $currentState['oreRobots'] < $maxOreCost) {
            $newState = $nextState;
            $newState['ore'] -= $blueprint->oreRobotCostOre;
            $newState['oreRobots']++;
            $queue[] = $newState;
        }

        // Option 3: Build a clay robot if possible and helpful
        if ($currentState['ore'] >= $blueprint->clayRobotCostOre && $currentState['clayRobots'] < $blueprint->obsidianRobotCostClay ) {
            $newState = $nextState;
            $newState['ore'] -= $blueprint->clayRobotCostOre;
            $newState['clayRobots']++;
            $queue[] = $newState;
        }

        // Option 4: Build an obsidian robot if possible and helpful
        if ($currentState['ore'] >= $blueprint->obsidianRobotCostOre && $currentState['clay'] >= $blueprint->obsidianRobotCostClay && $currentState['obsidianRobots'] < $blueprint->geodeRobotCostObsidian) {
            $newState = $nextState;
            $newState['ore'] -= $blueprint->obsidianRobotCostOre;
            $newState['clay'] -= $blueprint->obsidianRobotCostClay;
            $newState['obsidianRobots']++;
            $queue[] = $newState;
        }

        // Option 5: Build a geode robot if possible
        if ($currentState['ore'] >= $blueprint->geodeRobotCostOre && $currentState['obsidian'] >= $blueprint->geodeRobotCostObsidian) {
            $newState = $nextState;
            $newState['ore'] -= $blueprint->geodeRobotCostOre;
            $newState['obsidian'] -= $blueprint->geodeRobotCostObsidian;
            $newState['geodeRobots']++;
            $queue[] = $newState;
        }

        usort($queue, function ($a, $b) {
            $timeDiff = $b['time'] - $a['time'];
            if ($timeDiff !== 0) {
                return $timeDiff;
            }

            $geodeDiff = $b['geode'] - $a['geode'];
            if ($geodeDiff !== 0) {
                return $geodeDiff;
            }

            $geodeRobotDiff = $b['geodeRobots'] - $a['geodeRobots'];
            if ($geodeRobotDiff !== 0) {
                return $geodeRobotDiff;
            }

            $obsidianDiff = $b['obsidian'] - $a['obsidian'];
            if ($obsidianDiff !== 0) {
                return $obsidianDiff;
            }
          
            $obsidianRobotDiff = $b['obsidianRobots'] - $a['obsidianRobots'];
            if($obsidianRobotDiff !== 0){
                return $obsidianRobotDiff;
            }
            
            $clayDiff = $b['clay'] - $a['clay'];
            if ($clayDiff !== 0) {
                return $clayDiff;
            }
            
            $clayRobotDiff = $b['clayRobots'] - $a['clayRobots'];
            if ($clayRobotDiff !== 0) {
                return $clayRobotDiff;
            }
            

            return $b['oreRobots'] - $a['oreRobots'];
        });


        $queue = array_slice($queue, 0, 30000);
    }

    return $maxGeodes;
}

/**
 * Parses blueprints from the input file.
 *
 * @param string $filename The name of the input file.
 *
 * @return Blueprint[] An array of Blueprint objects.
 */
function parseBlueprints(string $filename): array
{
    $blueprints = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        preg_match_all('/\d+/', $line, $matches);
        $blueprintData = array_map('intval', $matches[0]);
        $blueprints[] = new Blueprint($blueprintData);
    }

    return $blueprints;
}

// Main execution for Part One
$blueprints = parseBlueprints('input.txt');
$totalQualityLevel = 0;

foreach ($blueprints as $blueprint) {
    $maxGeodes = maxGeodes($blueprint, 24);
    $qualityLevel = $blueprint->id * $maxGeodes;
    $totalQualityLevel += $qualityLevel;
}

echo "Part One - Total quality level: " . $totalQualityLevel . "\n";

// Main execution for Part Two
$productOfMaxGeodes = 1;
$firstThreeBlueprints = array_slice($blueprints, 0, 3);
foreach ($firstThreeBlueprints as $blueprint) {
    $maxGeodes = maxGeodes($blueprint, 32);
    $productOfMaxGeodes *= $maxGeodes;
}

echo "Part Two - Product of max geodes for first three blueprints: " . $productOfMaxGeodes . "\n";

?>
