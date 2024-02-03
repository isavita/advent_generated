
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
    $version = ($binStr[$idx]-'0')<<2 | ($binStr[$idx+1]-'0')<<1 | ($binStr[$idx+2]-'0');
    $typeID = ($binStr[$idx+3]-'0')<<2 | ($binStr[$idx+4]-'0')<<1 | ($binStr[$idx+5]-'0');
    $idx += 6;

    if ($typeID == 4) {
        while ($binStr[$idx] == '1') {
            $idx += 5;
        }
        $idx += 5;
        return [$version, $idx];
    }

    $lengthTypeID = $binStr[$idx] - '0';
    $idx++;
    $numSubPackets = 0;
    $subPacketLength = 0;

    if ($lengthTypeID == 0) {
        $subPacketLength = 0;
        for ($i = 0; $i < 15; $i++) {
            $subPacketLength = $subPacketLength<<1 | ($binStr[$idx]-'0');
            $idx++;
        }
    } else {
        $numSubPackets = 0;
        for ($i = 0; $i < 11; $i++) {
            $numSubPackets = $numSubPackets<<1 | ($binStr[$idx]-'0');
            $idx++;
        }
    }

    $versionSum = $version;
    while (true) {
        if ($lengthTypeID == 0 && $subPacketLength == 0) {
            break;
        }
        if ($lengthTypeID == 1 && $numSubPackets == 0) {
            break;
        }
        list($subVersion, $newIndex) = parsePacket($binStr, $idx);
        $versionSum += $subVersion;

        if ($lengthTypeID == 0) {
            $subPacketLength -= $newIndex - $idx;
        } else {
            $numSubPackets--;
        }
        $idx = $newIndex;
    }
    return [$versionSum, $idx];
}

$data = trim(file_get_contents("input.txt"));
$hexStr = $data;
$binStr = hexToBin($hexStr);
list($versionSum, $_) = parsePacket($binStr, 0);
echo $versionSum . "\n";
?>
