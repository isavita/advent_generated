
<?php

class Room {
    public string $name;
    public array $connections = [];

    public function __construct(string $name) {
        $this->name = $name;
    }
}

class EmulatorStatus {
    const HALTED = 0;
    const OUTPUT = 1;
    const WAITING_FOR_INPUT = 2;
}

class Emulator {
    public array $memory;
    public array $input;
    public int $ip = 0;
    public int $relative_base = 0;

    public function __construct(array $program, ?array $input_values = null) {
        $this->memory = $program;
        $this->input = $input_values ?? [];
    }

    public function write_string(string $s): int {
        $len = strlen($s);
        for ($i = 0; $i < $len; $i++) {
            $this->input[] = ord($s[$i]);
        }
        return $len;
    }

    public function emulate(): array {
        while (true) {
            $instruction = $this->memory[$this->ip] ?? 0;
            $opcode = $instruction % 100;

            $get_parameter = function(int $offset) use ($instruction): int {
                $mode = intdiv($instruction, 10 ** ($offset + 1)) % 10;
                $param = $this->memory[$this->ip + $offset] ?? 0;
                if ($mode === 0) {
                    return $this->memory[$param] ?? 0;
                } elseif ($mode === 1) {
                    return $param;
                } elseif ($mode === 2) {
                    $address = $this->relative_base + $param;
                    return $this->memory[$address] ?? 0;
                }
                throw new Exception("Unknown parameter mode: {$mode}");
            };

            $get_write_address = function(int $offset) use ($instruction): int {
                $mode = intdiv($instruction, 10 ** ($offset + 1)) % 10;
                $param = $this->memory[$this->ip + $offset] ?? 0;
                if ($mode === 0) {
                    return $param;
                } elseif ($mode === 2) {
                    return $this->relative_base + $param;
                }
                throw new Exception("Invalid mode for writing: {$mode}");
            };

            if ($opcode === 1) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $c = $get_write_address(3);
                $this->memory[$c] = $a + $b;
                $this->ip += 4;
            } elseif ($opcode === 2) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $c = $get_write_address(3);
                $this->memory[$c] = $a * $b;
                $this->ip += 4;
            } elseif ($opcode === 3) {
                if (empty($this->input)) {
                    return [null, EmulatorStatus::WAITING_FOR_INPUT];
                }
                $a = $get_write_address(1);
                $this->memory[$a] = array_shift($this->input);
                $this->ip += 2;
            } elseif ($opcode === 4) {
                $a = $get_parameter(1);
                $this->ip += 2;
                return [$a, EmulatorStatus::OUTPUT];
            } elseif ($opcode === 5) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $this->ip = ($a !== 0) ? $b : $this->ip + 3;
            } elseif ($opcode === 6) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $this->ip = ($a === 0) ? $b : $this->ip + 3;
            } elseif ($opcode === 7) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $c = $get_write_address(3);
                $this->memory[$c] = ($a < $b) ? 1 : 0;
                $this->ip += 4;
            } elseif ($opcode === 8) {
                $a = $get_parameter(1);
                $b = $get_parameter(2);
                $c = $get_write_address(3);
                $this->memory[$c] = ($a === $b) ? 1 : 0;
                $this->ip += 4;
            } elseif ($opcode === 9) {
                $a = $get_parameter(1);
                $this->relative_base += $a;
                $this->ip += 2;
            } elseif ($opcode === 99) {
                return [null, EmulatorStatus::HALTED];
            } else {
                throw new Exception("Unknown opcode: {$opcode} at position {$this->ip}");
            }
        }
    }
}

function find_path(Room $from_room, Room $to_room): ?array {
    $queue = [[$from_room, [$from_room]]];
    $visited = [$from_room->name => true];

    while (!empty($queue)) {
        [$current, $path] = array_shift($queue);
        if ($current === $to_room) {
            return $path;
        }
        foreach ($current->connections as $neighbor) {
            if ($neighbor && !isset($visited[$neighbor->name])) {
                $visited[$neighbor->name] = true;
                $queue[] = [$neighbor, array_merge($path, [$neighbor])];
            }
        }
    }
    return null;
}

$opposite = ["north" => "south", "south" => "north", "west" => "east", "east" => "west"];

$text = file_get_contents("input.txt");
$program = array_map('intval', explode(",", $text));
$emulator = new Emulator($program);

$send_command = function(string $format, ...$args) use ($emulator) {
    $cmd = sprintf($format, ...$args);
    $emulator->write_string($cmd);
};

$room_name_regex = '/^== (.+) ==$/';
$list_item_regex = '/^- (.+)$/';
$taken_regex = '/^You take the (.+)\.$/';
$dropped_regex = '/^You drop the (.+)\.$/';
$result_regex = '/"Oh, hello! You should be able to get in by typing (\d+) on the keypad at the main airlock\."$/';

$world = [];
$inventory = [];
$mode = 0; 
$path = [];
$checkpoint = null;
$floor = null;
$test_dir = "";
$available_items = [];
$item_mask = 0;
$last = null;
$last_items = [];
$last_dir = "";
$output_builder = [];
$current_room = null;

while (true) {
    [$char, $status] = $emulator->emulate();

    if ($status === EmulatorStatus::HALTED) {
        $output = implode(array_map('chr', $output_builder));
        if (preg_match($result_regex, $output, $match)) {
            echo $match[1];
            exit;
        }
    } elseif ($status === EmulatorStatus::OUTPUT) {
        if ($char !== null) {
            $output_builder[] = $char;
        }
    } elseif ($status === EmulatorStatus::WAITING_FOR_INPUT) {
        $output = implode(array_map('chr', $output_builder));
        $output_builder = [];

        $items = [];
        $lines = explode("\n", $output);
        $i = 0;
        while ($i < count($lines)) {
            $line = trim($lines[$i]);

            if (!$line || $line === "Command?") {
                $i++;
                continue;
            }

            if (preg_match($room_name_regex, $line, $match)) {
                $name = $match[1];
                $i++;
                while ($i < count($lines) && trim($lines[$i]) !== "") {
                    $i++;
                }
                if (!isset($world[$name])) {
                    $current_room = new Room($name);
                    $world[$name] = $current_room;
                } else {
                    $current_room = $world[$name];
                }
                $items = [];
                $i++;
                continue;
            }

            if ($line === "Doors here lead:") {
                $i++;
                while ($i < count($lines) && trim($lines[$i]) !== "") {
                    $door_line = trim($lines[$i]);
                    if (preg_match($list_item_regex, $door_line, $match) && $current_room) {
                        $direction = $match[1];
                        if (!isset($current_room->connections[$direction])) {
                            $current_room->connections[$direction] = null;
                        }
                    }
                    $i++;
                }
                continue;
            }

            if ($line === "Items here:") {
                $i++;
                while ($i < count($lines) && trim($lines[$i]) !== "") {
                    $item_line = trim($lines[$i]);
                    if (preg_match($list_item_regex, $item_line, $match)) {
                        $item = $match[1];
                        $items[] = $item;
                    }
                    $i++;
                }
                continue;
            }

            if (preg_match($taken_regex, $line, $match)) {
                $taken = $match[1];
                $inventory[$taken] = true;
                if ($last) {
                    $current_room = $last;
                    $items = array_filter($last_items, function($item) use ($taken) { return $item !== $taken; });
                }
                $i++;
                continue;
            }

            if (preg_match($dropped_regex, $line, $match)) {
                $dropped = $match[1];
                $inventory[$dropped] = false;
                if ($last) {
                    $current_room = $last;
                    $items = array_merge($last_items, [$dropped]);
                }
                $i++;
                continue;
            }

            if (strpos($line, 'A loud, robotic voice says "Alert!') === 0) {
                if ($mode === 0) {
                    if (!empty($path)) {
                        array_pop($path);
                    }
                    $checkpoint = $last;
                    $floor = $current_room;
                    $test_dir = $last_dir;
                    if ($checkpoint && $test_dir) {
                        $checkpoint->connections[$test_dir] = $floor;
                    }
                }
                $last = null;
                $last_items = [];
                $last_dir = "";
                $i++;
                continue;
            }

            $i++;
        }

        if ($last !== null && $last_dir && $current_room !== null) {
            if (!isset($last->connections[$last_dir])) {
                $last->connections[$last_dir] = $current_room;
                $current_room->connections[$opposite[$last_dir]] = $last;
            }
        }

        $last = $current_room;
        $last_items = $items;
        $last_dir = "";

        if ($mode === 0) {
            $blacklist = ["photons", "escape pod", "molten lava", "infinite loop", "giant electromagnet"];
            $item_taken = false;
            foreach ($items as $item) {
                if (!in_array($item, $blacklist)) {
                    $send_command("take %s\n", $item);
                    $item_taken = true;
                    break;
                }
            }
            if (!$item_taken) {
                $target = null;
                foreach ($current_room->connections as $direction => $room) {
                    if ($room === null) {
                        $path[] = $current_room;
                        $target = $direction;
                        break;
                    }
                }

                if ($target) {
                    $last_dir = $target;
                    $send_command("%s\n", $target);
                    continue;
                }

                if (!empty($path)) {
                    $last_room = array_pop($path);
                    $back_dir = null;
                    foreach ($current_room->connections as $direction => $room) {
                        if ($room === $last_room) {
                            $back_dir = $direction;
                            break;
                        }
                    }
                    if ($back_dir) {
                        $last_dir = $back_dir;
                        $send_command("%s\n", $back_dir);
                        continue;
                    } else {
                        throw new Exception("Cannot go from \"{$current_room->name}\" to \"{$last_room->name}\"");
                    }
                }

                if ($checkpoint && $floor) {
                    $new_path = find_path($current_room, $checkpoint);
                    if ($new_path) {
                        array_shift($new_path);
                        $path = $new_path;
                    }
                    $mode = 1;
                    continue;
                }
            }
        } elseif ($mode === 1) {
            if (!empty($path)) {
                $next_room = array_shift($path);
                $direction = null;
                foreach ($current_room->connections as $dir => $room) {
                    if ($room === $next_room) {
                        $direction = $dir;
                        break;
                    }
                }
                if ($direction) {
                    $last_dir = $direction;
                    $send_command("%s\n", $direction);
                    continue;
                } else {
                    throw new Exception("Cannot go from \"{$current_room->name}\" to \"{$next_room->name}\"");
                }
            } else {
                $available_items = [];
                foreach ($inventory as $item => $has) {
                    if ($has) {
                        $available_items[] = $item;
                    }
                }
                $item_mask = 0;
                $mode = 2;
            }
        } elseif ($mode === 2) {
            $item_processed = false;
            foreach ($available_items as $index => $item) {
                $target_state = ($item_mask & (1 << $index)) !== 0;
                if ($inventory[$item] !== $target_state) {
                    $action = $target_state ? "take" : "drop";
                    $send_command("%s %s\n", $action, $item);
                    $item_processed = true;
                    break;
                }
            }
            if (!$item_processed) {
                $item_mask++;
                if ($test_dir) {
                    $send_command("%s\n", $test_dir);
                    continue;
                } else {
                    throw new Exception("Test direction (test_dir) is not set.");
                }
            }
            continue;
        }
    }
}
