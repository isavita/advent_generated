
<?php

const HighCard = 1;
const OnePair = 2;
const TwoPair = 3;
const ThreeKind = 4;
const FullHouse = 5;
const FourKind = 6;
const FiveKind = 7;

class Hand {
    public $cards;
    public $bid;

    public function __construct($cards, $bid) {
        $this->cards = $cards;
        $this->bid = $bid;
    }
}

class RankedHand {
    public $hand;
    public $rank;

    public function __construct($hand, $rank) {
        $this->hand = $hand;
        $this->rank = $rank;
    }
}

$matches = [[], [], [], [], [], [], []];

function findMatches($hands) {
    global $matches;

    foreach ($hands as $hand) {
        $count = array_count_values(str_split($hand->cards));
        $value = 1;

        foreach ($count as $c) {
            $value *= $c;
        }

        switch ($value) {
            case 1:
                array_push($matches[6], $hand);
                break;
            case 2:
                array_push($matches[5], $hand);
                break;
            case 3:
                array_push($matches[3], $hand);
                break;
            case 4:
                if (count($count) == 2) {
                    array_push($matches[1], $hand);
                } else {
                    array_push($matches[4], $hand);
                }
                break;
            case 5:
                array_push($matches[0], $hand);
                break;
            case 6:
                array_push($matches[2], $hand);
                break;
            default:
                echo "oh no\n";
        }
    }
}

function convertAndOrderMatches() {
    global $matches;

    $convertedMatches = [];

    foreach ($matches as $category) {
        $temp = [];

        foreach ($category as $hand) {
            $cards = str_replace(['A', 'T', 'J', 'Q', 'K'], ['E', 'A', 'B', 'C', 'D'], $hand->cards);
            $num = hexdec($cards);

            $temp[] = new RankedHand($hand, $num);
        }

        usort($temp, function($a, $b) {
            return $b->rank - $a->rank;
        });

        $convertedMatches = array_merge($convertedMatches, $temp);
    }

    return $convertedMatches;
}

$input = file_get_contents("input.txt");
$lines = explode("\n", $input);
$hands = [];

foreach ($lines as $line) {
    if (empty($line)) {
        continue;
    }

    preg_match('/[\dAKQJT]+/', $line, $cards);
    preg_match('/ [\d]+/', $line, $bid);
    $bid = intval(ltrim($bid[0]));

    $hands[] = new Hand($cards[0], $bid);
}

findMatches($hands);
$convertedMatches = convertAndOrderMatches();

$total = 0;
foreach ($convertedMatches as $key => $convertedMatch) {
    $total += $convertedMatch->hand->bid * (count($convertedMatches) - $key);
}

echo $total . "\n";
?>
