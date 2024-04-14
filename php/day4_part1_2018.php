<?php
class Record {
    public $timestamp;
    public $action;
    public $guardID;

    public function __construct($timestamp, $action, $guardID) {
        $this->timestamp = $timestamp;
        $this->action = $action;
        $this->guardID = $guardID;
    }
}

function readAndParseInput($filename) {
    $file = fopen($filename, "r");
    $records = [];
    $layout = 'Y-m-d H:i';

    while ($line = fgets($file)) {
        $parts = explode("] ", trim($line));
        $timePart = substr($parts[0], 1);
        $actionPart = $parts[1];

        $ts = DateTime::createFromFormat($layout, $timePart);
        $guardID = -1;

        if (strpos($actionPart, "Guard") !== false) {
            sscanf($actionPart, "Guard #%d begins shift", $guardID);
            $actionPart = "begins shift";
        } elseif (strpos($actionPart, "falls asleep") !== false) {
            $actionPart = "falls asleep";
        } elseif (strpos($actionPart, "wakes up") !== false) {
            $actionPart = "wakes up";
        }

        $records[] = new Record($ts, $actionPart, $guardID);
    }
    fclose($file);
    return $records;
}

function main() {
    $records = readAndParseInput("input.txt");
    usort($records, function($a, $b) {
        return $a->timestamp < $b->timestamp ? -1 : 1;
    });

    $guardSleepMinutes = [];
    $currentGuardID = null;
    $sleepStart = null;

    foreach ($records as $record) {
        switch ($record->action) {
            case "begins shift":
                $currentGuardID = $record->guardID;
                break;
            case "falls asleep":
                $sleepStart = $record->timestamp;
                break;
            case "wakes up":
                if (!isset($guardSleepMinutes[$currentGuardID])) {
                    $guardSleepMinutes[$currentGuardID] = array_fill(0, 60, 0);
                }
                for ($i = (int)$sleepStart->format("i"); $i < (int)$record->timestamp->format("i"); $i++) {
                    $guardSleepMinutes[$currentGuardID][$i]++;
                }
                break;
        }
    }

    $maxSleep = 0;
    $sleepiestGuard = null;
    foreach ($guardSleepMinutes as $guardID => $minutes) {
        $totalSleep = array_sum($minutes);
        if ($totalSleep > $maxSleep) {
            $maxSleep = $totalSleep;
            $sleepiestGuard = $guardID;
        }
    }

    $maxMinute = 0;
    $maxMinuteCount = 0;
    foreach ($guardSleepMinutes[$sleepiestGuard] as $i => $count) {
        if ($count > $maxMinuteCount) {
            $maxMinuteCount = $count;
            $maxMinute = $i;
        }
    }

    echo $sleepiestGuard * $maxMinute;
}

main();