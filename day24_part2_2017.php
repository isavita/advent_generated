
<?php

class Component {
    public $a;
    public $b;

    public function __construct($a, $b) {
        $this->a = $a;
        $this->b = $b;
    }
}

$maxStrength = 0;
$maxLength = 0;

function findStrongestLongestBridge($components, $used, $port, $strength, $length) {
    global $maxStrength, $maxLength;

    if ($length > $maxLength || ($length == $maxLength && $strength > $maxStrength)) {
        $maxStrength = $strength;
        $maxLength = $length;
    }

    foreach ($components as $i => $c) {
        if ($used[$i]) {
            continue;
        }

        if ($c->a == $port || $c->b == $port) {
            $used[$i] = true;
            $nextPort = $c->a;
            if ($c->a == $port) {
                $nextPort = $c->b;
            }
            findStrongestLongestBridge($components, $used, $nextPort, $strength + $c->a + $c->b, $length + 1);
            $used[$i] = false;
        }
    }
}

$components = [];
$used = [];

$lines = file("input.txt", FILE_IGNORE_NEW_LINES);
foreach ($lines as $line) {
    $ports = explode("/", $line);
    $a = (int)$ports[0];
    $b = (int)$ports[1];
    $components[] = new Component($a, $b);
    $used[] = false;
}

findStrongestLongestBridge($components, $used, 0, 0, 0);

echo $maxStrength . "\n";
?>
