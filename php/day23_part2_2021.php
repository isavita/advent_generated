
<?php

/**
 * Class representing the state of the burrow and amphipods.
 */
class BurrowState {
  public $hallway;
  public $rooms;
  public $energy;
  public $roomSize;

  /**
   * BurrowState constructor.
   * @param array $hallway
   * @param array $rooms
   * @param int $energy
   * @param int $roomSize
   */
  public function __construct(array $hallway, array $rooms, int $energy, int $roomSize) {
    $this->hallway = $hallway;
    $this->rooms = $rooms;
    $this->energy = $energy;
    $this->roomSize = $roomSize;
  }

  /**
   * Returns a unique string representation of the current state.
   * @return string
   */
  public function getKey(): string {
    return implode('', $this->hallway) . '|' . implode('', array_map(function ($room) {
      return implode('', $room);
    }, $this->rooms));
  }

  /**
   * Checks if the burrow is in the final organized state.
   * @return bool
   */
  public function isFinal(): bool {
    foreach (['A', 'B', 'C', 'D'] as $i => $type) {
      if (count($this->rooms[$i]) != $this->roomSize || count(array_filter($this->rooms[$i], function ($a) use ($type) {
        return $a != $type;
      })) > 0) {
        return false;
      }
    }
    return true;
  }
}

/**
 * Solves the Amphipod puzzle.
 * @param array $initialRooms
 * @param int $roomSize
 * @return int
 */
function solveAmphipod(array $initialRooms, int $roomSize): int {
  $hallwaySize = 11;
  $initialHallway = array_fill(0, $hallwaySize, '.');
  $initialState = new BurrowState($initialHallway, $initialRooms, 0, $roomSize);

  $energyCosts = ['A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000];
  $roomPositions = [0 => 2, 1 => 4, 2 => 6, 3 => 8];

  $queue = new SplPriorityQueue();
  $queue->insert($initialState, 0);
  $visited = [];
  $minEnergy = PHP_INT_MAX;

  while (!$queue->isEmpty()) {
    $currentState = $queue->extract();

    if ($currentState->energy >= $minEnergy) {
      continue;
    }

    if ($currentState->isFinal()) {
      $minEnergy = min($minEnergy, $currentState->energy);
      continue;
    }

    $key = $currentState->getKey();
    if (isset($visited[$key]) && $visited[$key] <= $currentState->energy) {
      continue;
    }
    $visited[$key] = $currentState->energy;


    // Move amphipods from rooms to hallway
    foreach ($currentState->rooms as $roomIndex => $room) {
      if (count(array_unique($room)) <= 1 && count(array_filter($room)) > 0 ) {
          $target_type = ['A','B','C','D'][$roomIndex];
          if (str_replace('.','',$room) == str_repeat($target_type, count($room))) continue;
      }
      
      $firstAmphipodIndex = -1;
        for($k=0; $k<count($room); ++$k){
            if ($room[$k] != ".") {
                $firstAmphipodIndex = $k;
                break;
            }
        }

      if ($firstAmphipodIndex != -1) {
        $amphipod = $room[$firstAmphipodIndex];
        $roomPos = $roomPositions[$roomIndex];

        for ($hallwayPos = $roomPos - 1; $hallwayPos >= 0; $hallwayPos--) {
          if ($currentState->hallway[$hallwayPos] != '.') {
            break;
          }
          if (in_array($hallwayPos, $roomPositions)) {
            continue;
          }

          $newHallway = $currentState->hallway;
          $newHallway[$hallwayPos] = $amphipod;
          $newRooms = $currentState->rooms;
          $newRooms[$roomIndex][$firstAmphipodIndex] = '.';
          $steps = abs($hallwayPos - $roomPos) + 1 + $firstAmphipodIndex;
          $newEnergy = $currentState->energy + $steps * $energyCosts[$amphipod];
          $newState = new BurrowState($newHallway, $newRooms, $newEnergy, $roomSize);
            
          $queue->insert($newState, -$newEnergy);
        }

        for ($hallwayPos = $roomPos + 1; $hallwayPos < $hallwaySize; $hallwayPos++) {
          if ($currentState->hallway[$hallwayPos] != '.') {
            break;
          }
          if (in_array($hallwayPos, $roomPositions)) {
            continue;
          }

          $newHallway = $currentState->hallway;
          $newHallway[$hallwayPos] = $amphipod;
          $newRooms = $currentState->rooms;
          $newRooms[$roomIndex][$firstAmphipodIndex] = '.';
          $steps = abs($hallwayPos - $roomPos) + 1 + $firstAmphipodIndex;
          $newEnergy = $currentState->energy + $steps * $energyCosts[$amphipod];
          $newState = new BurrowState($newHallway, $newRooms, $newEnergy, $roomSize);
            
          $queue->insert($newState, -$newEnergy);
        }
      }
    }

    // Move amphipods from hallway to rooms
    foreach ($currentState->hallway as $hallwayPos => $amphipod) {
      if ($amphipod == '.') {
        continue;
      }

      $targetRoomIndex = ord($amphipod) - ord('A');
      $targetRoomPos = $roomPositions[$targetRoomIndex];

      $canMove = true;
      $start = min($hallwayPos + 1, $targetRoomPos);
      $end = max($hallwayPos - 1, $targetRoomPos);
      for ($i = $start; $i <= $end; $i++) {
        if ($currentState->hallway[$i] != '.' && $i != $hallwayPos) {
          $canMove = false;
          break;
        }
      }

        $targetRoom = $currentState->rooms[$targetRoomIndex];
        $empty_pos = -1;
         for($k=count($targetRoom)-1; $k>=0; --$k){
            if ($targetRoom[$k] == ".") {
                $empty_pos = $k;
                break;
            }
        }
      if ($canMove && $empty_pos != -1) {

            $all_same_type = true;
          for($m = 0; $m<count($targetRoom); ++$m) {
                if ($targetRoom[$m] != "." && $targetRoom[$m] != $amphipod) $all_same_type = false;
          }

        if($all_same_type){
            $newHallway = $currentState->hallway;
            $newHallway[$hallwayPos] = '.';
            $newRooms = $currentState->rooms;
            $newRooms[$targetRoomIndex][$empty_pos] = $amphipod;
            $steps = abs($hallwayPos - $targetRoomPos) + 1 + $empty_pos;
            $newEnergy = $currentState->energy + $steps * $energyCosts[$amphipod];
            $newState = new BurrowState($newHallway, $newRooms, $newEnergy, $roomSize);
            $queue->insert($newState, -$newEnergy);
        }
      }
    }
  }

  return $minEnergy;
}

// Read input from file
$inputFile = fopen("input.txt", "r");
$lines = [];
while (($line = fgets($inputFile)) !== false) {
  $lines[] = trim($line);
}
fclose($inputFile);

// Parse input for part 1
$initialRooms1 = [];
$roomLines1 = array_slice($lines, 2, 2);
foreach ($roomLines1 as $line) {
  preg_match_all('/([A-D])/', $line, $matches);
  foreach ($matches[1] as $index => $amphipod) {
    $initialRooms1[$index][] = $amphipod;
  }
}

// Solve part 1
$minEnergy1 = solveAmphipod($initialRooms1, 2);
echo "Part 1: " . $minEnergy1 . "\n";

// Parse and modify input for part 2
$initialRooms2 = [];
$roomLines2 = array_slice($lines, 2, 2);
array_splice( $roomLines2, 1, 0, ["  #D#C#B#A#", "  #D#B#A#C#"] );

foreach ($roomLines2 as $line) {
    preg_match_all('/([A-D])/', $line, $matches);
    foreach ($matches[1] as $index => $amphipod) {
      $initialRooms2[$index][] = $amphipod;
    }
  }

// Solve part 2
$minEnergy2 = solveAmphipod($initialRooms2, 4);
echo "Part 2: " . $minEnergy2 . "\n";

?>
