<?php
class Record {
    public $time;
    public $event;

    function __construct($time, $event) {
        $this->time = $time;
        $this->event = $event;
    }
}

class Guard {
    public $id;
    public $minutes;
    public $totalMin;

    function __construct($id) {
        $this->id = $id;
        $this->minutes = array_fill(0, 60, 0);
        $this->totalMin = 0;
    }
}

$lines = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
$records = [];

foreach ($lines as $line) {
    $time = DateTime::createFromFormat("Y-m-d H:i", substr($line, 1, 16));
    $event = substr($line, 19);
    $records[] = new Record($time, $event);
}

usort($records, function($a, $b) {
    return $a->time <=> $b->time;
});

$guards = [];
$currentGuard = null;
$sleepStart = 0;

foreach ($records as $record) {
    if (strpos($record->event, "begins shift") !== false) {
        preg_match('/#(\d+)/', $record->event, $matches);
        $id = intval($matches[1]);
        if (!array_key_exists($id, $guards)) {
            $guards[$id] = new Guard($id);
        }
        $currentGuard = $guards[$id];
    } elseif (strpos($record->event, "falls asleep") !== false) {
        $sleepStart = (int)$record->time->format('i');
    } elseif (strpos($record->event, "wakes up") !== false) {
        $wakeMinute = (int)$record->time->format('i');
        for ($i = $sleepStart; $i < $wakeMinute; $i++) {
            $currentGuard->minutes[$i]++;
            $currentGuard->totalMin++;
        }
    }
}

$mostFreqGuard = null;
$mostFreqMin = 0;

foreach ($guards as $guard) {
    for ($i = 0; $i < 60; $i++) {
        if ($mostFreqGuard === null || $guard->minutes[$i] > $mostFreqGuard->minutes[$mostFreqMin]) {
            $mostFreqGuard = $guard;
            $mostFreqMin = $i;
        }
    }
}

echo $mostFreqGuard->id * $mostFreqMin;