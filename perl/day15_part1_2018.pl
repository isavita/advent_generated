use strict;
use warnings;
use List::Util qw(min max);

my %KindRunes = (
    1 => '.',
    2 => 'E',
    4 => 'G',
    8 => '#',
    16 => '@',
);

my %RuneKinds = (
    '.' => 1,
    'E' => 2,
    'G' => 4,
    '#' => 8,
);

my @offsets = (
    { X => 0, Y => -1 },
    { X => -1, Y => 0 },
    { X => 1, Y => 0 },
    { X => 0, Y => 1 },
);

sub is_unit {
    my ($bit) = @_;
    return (6 & $bit) != 0;
}

sub new_cave {
    my ($input, $elf_power) = @_;
    my $cave = {
        Units => [],
        Map => {},
    };
    parse_map($cave, $input, $elf_power);
    return $cave;
}

sub parse_map {
    my ($cave, $input, $elf_power) = @_;
    my $m = $cave->{Map};

    for my $y (0 .. $#$input) {
        my $row = $input->[$y];
        for my $x (0 .. length($row) - 1) {
            my $col = substr($row, $x, 1);
            my $kind = $RuneKinds{$col} // 8;

            my $tile = { Kind => $kind };
            if (is_unit($kind)) {
                push @{$cave->{Units}}, new_unit($tile, $kind, $elf_power);
            }
            set_tile($m, $tile, $x, $y);
        }
    }
}

sub set_tile {
    my ($m, $t, $x, $y) = @_;
    $m->{$y} //= {};
    $m->{$y}{$x} = $t;
    $t->{X} = $x;
    $t->{Y} = $y;
    $t->{Map} = $m;
}

sub tile {
    my ($m, $x, $y) = @_;
    return $m->{$y}{$x} if exists $m->{$y};
    return undef;
}

sub find_walkable_tiles {
    my ($m, $t) = @_;
    my @frontier = ($t);
    my %distance = ($t => 0);
    my %came_from = ($t => undef);

    while (@frontier) {
        my $current = shift @frontier;

        for my $next (walkable_neighbors($current)) {
            if (!exists $distance{$next}) {
                push @frontier, $next;
                $distance{$next} = $distance{$current} + 1;
                $came_from{$next} = $current;
            }
        }
    }

    return (\%distance, \%came_from);
}

sub walkable_neighbors {
    my ($t) = @_;
    my @neighbors;

    for my $offset (@offsets) {
        my $n = tile($t->{Map}, $t->{X} + $offset->{X}, $t->{Y} + $offset->{Y});
        if (defined $n && $n->{Kind} == 1) {
            push @neighbors, $n;
        }
    }

    return @neighbors;
}

sub new_unit {
    my ($tile, $kind, $elf_power) = @_;
    my $unit = {
        Kind => $kind,
        Hitpoints => 200,
        Power => 3,
        Tile => $tile,
    };
    $tile->{Unit} = $unit;
    if ($unit->{Kind} == 2) {
        $unit->{Power} = $elf_power;
    }
    return $unit;
}

sub targets {
    my ($u, $c) = @_;
    for my $unit (@{$c->{Units}}) {
        if ($unit->{Kind} != $u->{Kind} && $unit->{Hitpoints} > 0) {
            return 1;
        }
    }
    return 0;
}

sub next_tile {
    my ($u, $c) = @_;
    my @targets;

    my $closest_target_distance = 1e9;
    my ($distances, $path) = find_walkable_tiles($c->{Map}, $u->{Tile});
    my @enemies = enemies($u, $c);

    for my $enemy (@enemies) {
        for my $target (walkable_neighbors($enemy->{Tile})) {
            if (exists $distances->{$target} && $distances->{$target} <= $closest_target_distance) {
                if ($distances->{$target} < $closest_target_distance) {
                    $closest_target_distance = $distances->{$target};
                    @targets = ();
                }
                push @targets, $target;
            }
        }
    }
    @targets = sort { $a->{Y} <=> $b->{Y} || $a->{X} <=> $b->{X} } @targets;
    if (@targets) {
        my $target = $targets[0];
        my $current = $target;
        while (1) {
            if ($path->{$current} == $u->{Tile}) {
                return ($current, $target);
            }
            $current = $path->{$current};
        }
    }
    return (undef, undef);
}

sub enemies {
    my ($u, $c) = @_;
    my @enemies = grep { $_->{Kind} != $u->{Kind} && $_->{Hitpoints} > 0 } @{$c->{Units}};
    @enemies = sort { $a->{Tile}{Y} <=> $b->{Tile}{Y} || $a->{Tile}{X} <=> $b->{Tile}{X} } @enemies;
    return @enemies;
}

sub enemy_neighbor {
    my ($u, $c) = @_;
    my $target;
    for my $offset (@offsets) {
        my $t = tile($c->{Map}, $u->{Tile}{X} + $offset->{X}, $u->{Tile}{Y} + $offset->{Y});
        if (defined $t && defined $t->{Unit} && $t->{Unit}{Kind} != $u->{Kind} && $t->{Unit}{Hitpoints} > 0) {
            if (!defined $target || $t->{Unit}{Hitpoints} < $target->{Hitpoints}) {
                $target = $t->{Unit};
            }
        }
    }
    return $target;
}

sub move {
    my ($u, $c) = @_;
    if (defined enemy_neighbor($u, $c)) {
        return;
    }
    my ($next, undef) = next_tile($u, $c);
    if (defined $next) {
        $next->{Unit} = $u;
        $next->{Kind} = $u->{Kind};
        $u->{Tile}{Kind} = 1;
        $u->{Tile}{Unit} = undef;
        $u->{Tile} = $next;
    }
}

sub attack {
    my ($u, $c) = @_;
    my $enemy = enemy_neighbor($u, $c);
    if (defined $enemy) {
        my $killed = damage($enemy, $c, $u->{Power});
        return $killed && $enemy->{Kind} == 2;
    }
    return 0;
}

sub damage {
    my ($u, $c, $damage) = @_;
    $u->{Hitpoints} -= $damage;
    if ($u->{Hitpoints} <= 0) {
        remove_unit($c, $u);
        return 1;
    }
    return 0;
}

sub remove_unit {
    my ($c, $u) = @_;
    $u->{Tile}{Kind} = 1;
    $u->{Tile}{Unit} = undef;
    $u->{Tile} = undef;
}

sub status {
    my ($c) = @_;
    my ($elves, $goblins, $hp) = (0, 0, 0);

    for my $u (@{$c->{Units}}) {
        if ($u->{Hitpoints} <= 0) {
            next;
        }
        if ($u->{Kind} == 2) {
            $elves = 1;
        } else {
            $goblins = 1;
        }
        $hp += $u->{Hitpoints};
    }

    return ($hp, $elves && $goblins);
}

sub remove_the_dead {
    my ($c) = @_;
    my @new_units = grep { $_->{Hitpoints} > 0 } @{$c->{Units}};
    $c->{Units} = \@new_units;
}

sub tick {
    my ($c, $stop_on_elf_death) = @_;
    remove_the_dead($c);
    @{$c->{Units}} = sort { $a->{Tile}{Y} <=> $b->{Tile}{Y} || $a->{Tile}{X} <=> $b->{Tile}{X} } @{$c->{Units}};

    for my $unit (@{$c->{Units}}) {
        if ($unit->{Hitpoints} <= 0) {
            next;
        }
        if (!targets($unit, $c)) {
            return (0, 0);
        }
        move($unit, $c);
        if (attack($unit, $c) && $stop_on_elf_death) {
            return (0, 1);
        }
    }
    return (1, 0);
}

sub combat {
    my ($input) = @_;
    my $cave = new_cave($input, 3);
    my $i = 1;
    while (1) {
        my ($hp, $combat) = status($cave);

        if (!$combat) {
            return ($i - 1) * $hp;
        }

        my ($clean_round, undef) = tick($cave, 0);
        if (!$clean_round) {
            $i--;
        }
        $i++;
    }
    return -1;
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @lines = map { chomp; $_ } <$fh>;
close $fh;

my $result = combat(\@lines);
print "$result\n";