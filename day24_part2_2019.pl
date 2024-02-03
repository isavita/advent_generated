
use strict;
use warnings;

my $Side = 5;
my $Square = $Side * $Side;

sub parse {
    my @res = ();

    open(my $fh, '<', 'input.txt') or die $!;
    my $row = 0;
    while (my $line = <$fh>) {
        chomp $line;
        my @chars = split('', $line);
        for (my $col = 0; $col < $Side; $col++) {
            if ($chars[$col] eq '#') {
                $res[$row * $Side + $col] = 1;
            } else {
                $res[$row * $Side + $col] = 0;
            }
        }
        $row++;
    }
    close($fh);
    return \@res;
}

sub next2 {
    my ($space) = @_;
    my %newSpace = ();

    my ($minLevel, $maxLevel) = minMaxLevel($space);

    for my $level ($minLevel - 1 .. $maxLevel + 1) {
        $newSpace{$level} = [];

        for my $cell (0 .. $Square - 1) {
            next if $cell == 12;

            my $row = int($cell / $Side);
            my $col = $cell % $Side;
            my $neighbours = 0;

            if ($row == 0) {
                $neighbours++ if infested($space, $level - 1, 7);
            }

            if ($col == 0) {
                $neighbours++ if infested($space, $level - 1, 11);
            }

            if ($col == 4) {
                $neighbours++ if infested($space, $level - 1, 13);
            }

            if ($row == 4) {
                $neighbours++ if infested($space, $level - 1, 17);
            }

            if ($cell == 7) {
                for my $i (0 .. $Side - 1) {
                    $neighbours++ if infested($space, $level + 1, $i);
                }
            }

            if ($cell == 11) {
                for my $i (0 .. $Side - 1) {
                    $neighbours++ if infested($space, $level + 1, 5 * $i);
                }
            }

            if ($cell == 13) {
                for my $i (0 .. $Side - 1) {
                    $neighbours++ if infested($space, $level + 1, 5 * $i + $Side - 1);
                }
            }

            if ($cell == 17) {
                for my $i (0 .. $Side - 1) {
                    $neighbours++ if infested($space, $level + 1, ($Side - 1) * $Side + $i);
                }
            }

            if ($row > 0 && $cell != 17) {
                $neighbours++ if infested($space, $level, $cell - $Side);
            }

            if ($col > 0 && $cell != 13) {
                $neighbours++ if infested($space, $level, $cell - 1);
            }

            if ($col < $Side - 1 && $cell != 11) {
                $neighbours++ if infested($space, $level, $cell + 1);
            }

            if ($row < $Side - 1 && $cell != 7) {
                $neighbours++ if infested($space, $level, $cell + $Side);
            }

            if (infested($space, $level, $cell) && $neighbours != 1) {
                $newSpace{$level}[$cell] = 0;
                next;
            }

            if (!infested($space, $level, $cell) && ($neighbours == 1 || $neighbours == 2)) {
                $newSpace{$level}[$cell] = 1;
                next;
            }

            $newSpace{$level}[$cell] = infested($space, $level, $cell);
        }
    }

    clean(\%newSpace);

    return \%newSpace;
}

sub clean {
    my ($space) = @_;
    my ($min, $max) = minMaxLevel($space);

    my $countMin = 0;
    my $countMax = 0;
    for my $cell (0 .. $Square - 1) {
        $countMin++ if $space->{$min}[$cell];
        $countMax++ if $space->{$max}[$cell];
    }
    delete $space->{$min} if $countMin == 0;
    delete $space->{$max} if $countMax == 0;
}

sub infested {
    my ($space, $level, $cell) = @_;
    return 0 unless defined $space->{$level};
    return $space->{$level}[$cell];
}

sub minMaxLevel {
    my ($space) = @_;
    my $min = 999999;
    my $max = -999999;
    for my $level (keys %$space) {
        $min = $level if $level < $min;
        $max = $level if $level > $max;
    }
    return ($min, $max);
}

my $input = parse();

my %space = (0 => $input);

for (my $i = 0; $i < 200; $i++) {
    %space = %{ next2(\%space) };
}

my $count = 0;
for my $grid (values %space) {
    for (my $i = 0; $i < $Square; $i++) {
        $count++ if $grid->[$i];
    }
}
print "$count\n";
