use strict;
use warnings;

sub parse_stat {
    my ($line) = @_;
    my ($label, $value) = split /: /, $line;
    return $value;
}

sub max {
    my ($a, $b) = @_;
    return $a > $b ? $a : $b;
}

sub player_wins {
    my ($player, $boss) = @_;
    my $player_damage = max(1, $player->{Damage} - $boss->{Armor});
    my $boss_damage = max(1, $boss->{Damage} - $player->{Armor});

    my $player_turns = int(($boss->{HitPoints} + $player_damage - 1) / $player_damage);
    my $boss_turns = int(($player->{HitPoints} + $boss_damage - 1) / $boss_damage);

    return $player_turns <= $boss_turns;
}

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @lines = <$fh>;
close $fh;

chomp @lines;

my $boss = {
    HitPoints => parse_stat($lines[0]),
    Damage => parse_stat($lines[1]),
    Armor => parse_stat($lines[2]),
};

my @weapons = (
    { Cost => 8, Damage => 4 },
    { Cost => 10, Damage => 5 },
    { Cost => 25, Damage => 6 },
    { Cost => 40, Damage => 7 },
    { Cost => 74, Damage => 8 },
);

my @armors = (
    { Cost => 0, Armor => 0 },
    { Cost => 13, Armor => 1 },
    { Cost => 31, Armor => 2 },
    { Cost => 53, Armor => 3 },
    { Cost => 75, Armor => 4 },
    { Cost => 102, Armor => 5 },
);

my @rings = (
    { Cost => 0 },
    { Cost => 25, Damage => 1 },
    { Cost => 50, Damage => 2 },
    { Cost => 100, Damage => 3 },
    { Cost => 20, Armor => 1 },
    { Cost => 40, Armor => 2 },
    { Cost => 80, Armor => 3 },
);

my $min_cost = 2**31 - 1;

for my $w (@weapons) {
    for my $a (@armors) {
        for my $ri (0 .. $#rings) {
            for my $rj ($ri + 1 .. $#rings) {
                my $player = {
                    HitPoints => 100,
                    Damage => $w->{Damage} + ($rings[$ri]->{Damage} // 0) + ($rings[$rj]->{Damage} // 0),
                    Armor => $a->{Armor} + ($rings[$ri]->{Armor} // 0) + ($rings[$rj]->{Armor} // 0),
                };
                my $cost = $w->{Cost} + $a->{Cost} + $rings[$ri]->{Cost} + $rings[$rj]->{Cost};
                if (player_wins($player, $boss) && $cost < $min_cost) {
                    $min_cost = $cost;
                }
            }
        }
    }
}

print "$min_cost\n";