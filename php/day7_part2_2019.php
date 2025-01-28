
<?php

/**
 * Intcode computer implementation.
 */
class IntcodeComputer {
    private $program;
    private $inputs;
    private $pointer;
    private $output;
    private $halted;

    public function __construct(array $program) {
        $this->program = $program;
        $this->inputs = [];
        $this->pointer = 0;
        $this->output = null;
        $this->halted = false;
    }

    public function addInput(int $input): void {
        $this->inputs[] = $input;
    }
    
    public function isHalted(): bool {
      return $this->halted;
    }

    public function getOutput() {
        return $this->output;
    }

    public function run(): void {
      while (true) {
          $instruction = str_pad($this->program[$this->pointer], 5, '0', STR_PAD_LEFT);
          $opcode = (int)substr($instruction, 3, 2);
          $mode1 = (int)$instruction[2];
          $mode2 = (int)$instruction[1];

          if ($opcode === 99) {
              $this->halted = true;
              break;
          }

          $param1 = $this->getValue($mode1, 1);
          $param2 = $this->getValue($mode2, 2);

          if (in_array($opcode, [1, 2, 7, 8])) {
              $dest = $this->program[$this->pointer + 3];
          }

          switch ($opcode) {
              case 1:
                  $this->program[$dest] = $param1 + $param2;
                  $this->pointer += 4;
                  break;
              case 2:
                  $this->program[$dest] = $param1 * $param2;
                  $this->pointer += 4;
                  break;
              case 3:
                  if (empty($this->inputs)) {
                      return; 
                  }
                  $this->program[$this->program[$this->pointer + 1]] = array_shift($this->inputs);
                  $this->pointer += 2;
                  break;
              case 4:
                  $this->output = $param1;
                  $this->pointer += 2;
                  return;
              case 5:
                  $this->pointer = ($param1 !== 0) ? $param2 : $this->pointer + 3;
                  break;
              case 6:
                  $this->pointer = ($param1 === 0) ? $param2 : $this->pointer + 3;
                  break;
              case 7:
                  $this->program[$dest] = ($param1 < $param2) ? 1 : 0;
                  $this->pointer += 4;
                  break;
              case 8:
                  $this->program[$dest] = ($param1 === $param2) ? 1 : 0;
                  $this->pointer += 4;
                  break;
              default:
                  throw new Exception("Unknown opcode: $opcode");
          }
      }
    }

    private function getValue(int $mode, int $offset) {
        if ($mode === 0) {
            return $this->program[$this->program[$this->pointer + $offset]] ?? 0;
        } else {
            return $this->program[$this->pointer + $offset] ?? 0;
        }
    }
}

/**
 * Generates all permutations of an array.
 */
function permutations(array $elements): array {
    if (count($elements) <= 1) {
        return [$elements];
    }

    $result = [];
    foreach ($elements as $key => $element) {
        $remaining = $elements;
        unset($remaining[$key]);
        foreach (permutations($remaining) as $permutation) {
            array_unshift($permutation, $element);
            $result[] = $permutation;
        }
    }
    return $result;
}

/**
 * Runs the amplifier circuit.
 */
function runAmplifiers(array $program, array $phaseSettings): int {
    $signal = 0;
    foreach ($phaseSettings as $phaseSetting) {
        $computer = new IntcodeComputer($program);
        $computer->addInput($phaseSetting);
        $computer->addInput($signal);
        $computer->run();
        $signal = $computer->getOutput();
    }
    return $signal;
}
/**
 * Runs the amplifier circuit with feedback loop.
 */
function runAmplifiersWithFeedback(array $program, array $phaseSettings): int {
    $numAmplifiers = count($phaseSettings);
    $amplifiers = [];
    $signal = 0;

    for ($i = 0; $i < $numAmplifiers; $i++) {
      $amplifiers[] = new IntcodeComputer($program);
      $amplifiers[$i]->addInput($phaseSettings[$i]);
    }
    $amplifiers[0]->addInput(0);
    
    $amplifierIndex = 0;
    while(true) {
      $amplifiers[$amplifierIndex]->run();
      $output = $amplifiers[$amplifierIndex]->getOutput();
      
      if($amplifiers[$amplifierIndex]->isHalted() && $amplifierIndex == $numAmplifiers -1) {
        return $output;
      }

      $amplifierIndex = ($amplifierIndex + 1) % $numAmplifiers;
      $amplifiers[$amplifierIndex]->addInput($output);
    }
}

// Read program from input file
$program = array_map('intval', explode(',', trim(file_get_contents('input.txt'))));

// Part 1: Find the highest signal without feedback loop
$maxSignal = 0;
foreach (permutations(range(0, 4)) as $phaseSettings) {
    $signal = runAmplifiers($program, $phaseSettings);
    $maxSignal = max($maxSignal, $signal);
}
echo "Part 1: " . $maxSignal . "\n";

// Part 2: Find the highest signal with feedback loop
$maxSignal = 0;
foreach (permutations(range(5, 9)) as $phaseSettings) {
    $signal = runAmplifiersWithFeedback($program, $phaseSettings);
    $maxSignal = max($maxSignal, $signal);
}
echo "Part 2: " . $maxSignal . "\n";

?>
