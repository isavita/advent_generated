
use v6;

class Item {
    has Int $.cost = 0;
    has Int $.damage = 0;
    has Int $.armor = 0;
}

class Character {
    has Int $.hit-points;
    has Int $.damage;
    has Int $.armor;
}

sub parse-stat(Str $line --> Int) {
    return $line.split(": ")[1].Int;
}

sub player-wins(Character $player, Character $boss --> Bool) {
    my $player-damage = max(1, $player.damage - $boss.armor);
    my $boss-damage = max(1, $boss.damage - $player.armor);

    my $player-turns = ($boss.hit-points + $player-damage - 1) div $player-damage;
    my $boss-turns = ($player.hit-points + $boss-damage - 1) div $boss-damage;

    return $player-turns <= $boss-turns;
}

sub MAIN {
    my @lines = "input.txt".IO.slurp.lines;
    my $boss = Character.new(
        hit-points => parse-stat(@lines[0]),
        damage     => parse-stat(@lines[1]),
        armor      => parse-stat(@lines[2])
    );

    my @weapons = (
        Item.new(cost => 8,  damage => 4),
        Item.new(cost => 10, damage => 5),
        Item.new(cost => 25, damage => 6),
        Item.new(cost => 40, damage => 7),
        Item.new(cost => 74, damage => 8),
    );

    my @armors = (
        Item.new(),
        Item.new(cost => 13, armor => 1),
        Item.new(cost => 31, armor => 2),
        Item.new(cost => 53, armor => 3),
        Item.new(cost => 75, armor => 4),
        Item.new(cost => 102, armor => 5),
    );

    my @rings = (
        Item.new(),
        Item.new(cost => 25, damage => 1),
        Item.new(cost => 50, damage => 2),
        Item.new(cost => 100, damage => 3),
        Item.new(cost => 20, armor => 1),
        Item.new(cost => 40, armor => 2),
        Item.new(cost => 80, armor => 3),
    );

    my $max-cost = 0;

    for @weapons -> $weapon {
        for @armors -> $armor {
            for 0 .. @rings.end -> $ri {
                for $ri + 1 .. @rings.end -> $rj {
                    my $ring1 = @rings[$ri];
                    my $ring2 = @rings[$rj];

                    my $player = Character.new(
                        hit-points => 100,
                        damage     => $weapon.damage + $ring1.damage + $ring2.damage,
                        armor      => $armor.armor + $ring1.armor + $ring2.armor
                    );
                    my $current-cost = $weapon.cost + $armor.cost + $ring1.cost + $ring2.cost;

                    if !player-wins($player, $boss) and $current-cost > $max-cost {
                        $max-cost = $current-cost;
                    }
                }
            }
        }
    }

    say $max-cost;
}
