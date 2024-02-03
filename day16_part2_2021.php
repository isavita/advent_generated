
<?php

function hexToBin($hex) {
    $bin = "";
    for ($i = 0; $i < strlen($hex); $i++) {
        $b = hexdec($hex[$i]);
        $bin .= sprintf("%04b", $b);
    }
    return $bin;
}

function parsePacket($binStr, $idx) {
    $version = (int)$binStr[$idx] << 2 | (int)$binStr[$idx + 1] << 1 | (int)$binStr[$idx + 2];
    $typeID = (int)$binStr[$idx + 3] << 2 | (int)$binStr[$idx + 4] << 1 | (int)$binStr[$idx + 5];
    $idx += 6;

    if ($typeID == 4) {
        $value = 0;
        while ($binStr[$idx] == '1') {
            $value = $value << 4 | (int)$binStr[$idx + 1] << 3 | (int)$binStr[$idx + 2] << 2 | (int)$binStr[$idx + 3] << 1 | (int)$binStr[$idx + 4];
            $idx += 5;
        }
        $value = $value << 4 | (int)$binStr[$idx + 1] << 3 | (int)$binStr[$idx + 2] << 2 | (int)$binStr[$idx + 3] << 1 | (int)$binStr[$idx + 4];
        $idx += 5;
        return array($version, $idx, $value);
    }

    $lengthTypeID = (int)$binStr[$idx];
    $idx++;
    $numSubPackets = 0;
    $subPacketLength = 0;

    if ($lengthTypeID == 0) {
        $subPacketLength = 0;
        for ($i = 0; $i < 15; $i++) {
            $subPacketLength = $subPacketLength << 1 | (int)$binStr[$idx];
            $idx++;
        }
    } else {
        $numSubPackets = 0;
        for ($i = 0; $i < 11; $i++) {
            $numSubPackets = $numSubPackets << 1 | (int)$binStr[$idx];
            $idx++;
        }
    }

    $values = array();
    while (true) {
        if ($lengthTypeID == 0 && $subPacketLength == 0) {
            break;
        }
        if ($lengthTypeID == 1 && $numSubPackets == 0) {
            break;
        }
        list($_, $newIndex, $subValue) = parsePacket($binStr, $idx);
        $values[] = $subValue;

        if ($lengthTypeID == 0) {
            $subPacketLength -= $newIndex - $idx;
        } else {
            $numSubPackets--;
        }
        $idx = $newIndex;
    }

    $result = 0;
    switch ($typeID) {
        case 0:
            $result = 0;
            foreach ($values as $value) {
                $result += $value;
            }
            break;
        case 1:
            $result = 1;
            foreach ($values as $value) {
                $result *= $value;
            }
            break;
        case 2:
            $result = $values[0];
            foreach ($values as $value) {
                if ($value < $result) {
                    $result = $value;
                }
            }
            break;
        case 3:
            $result = $values[0];
            foreach ($values as $value) {
                if ($value > $result) {
                    $result = $value;
                }
            }
            break;
        case 5:
            $result = 0;
            if ($values[0] > $values[1]) {
                $result = 1;
            }
            break;
        case 6:
            $result = 0;
            if ($values[0] < $values[1]) {
                $result = 1;
            }
            break;
        case 7:
            $result = 0;
            if ($values[0] == $values[1]) {
                $result = 1;
            }
            break;
        default:
            throw new Exception("Unknown typeID");
    }

    return array($version, $idx, $result);
}

$data = file_get_contents("input.txt");
$hexStr = trim($data);
$binStr = hexToBin($hexStr);
list(, , $value) = parsePacket($binStr, 0);
echo $value . "\n";

?>
