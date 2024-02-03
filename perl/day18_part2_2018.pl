
use strict;
use warnings;

my $Open = '.';
my $Trees = '|';
my $Lumberyard = '#';
my $Size = 50;

my @grid = readInput("input.txt");

my %seenStates;
my ($cycleStart, $cycleLength);
for (my $minute = 0; ; $minute++) {
    my $state = gridToString(\@grid);
    if (exists $seenStates{$state}) {
        $cycleStart = $seenStates{$state};
        $cycleLength = $minute - $seenStates{$state};
        last;
    }
    $seenStates{$state} = $minute;
    @grid = transform(\@grid);
}

my $remainingMinutes = (1000000000 - $cycleStart) % $cycleLength;
for (my $i = 0; $i < $remainingMinutes; $i++) {
    @grid = transform(\@grid);
}

my ($wooded, $lumberyards) = countResources(\@grid);
print $wooded * $lumberyards . "\n";

sub readInput {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Error reading file: $!";
    my @grid;
    while (my $line = <$fh>) {
        chomp $line;
        my @row = split //, $line;
        push @grid, \@row;
    }
    close $fh;
    return @grid;
}

sub transform {
    my ($grid) = @_;
    my @newGrid;
    for my $i (0..$#{$grid}) {
        for my $j (0..$#{$grid->[$i]}) {
            $newGrid[$i][$j] = nextAcreState($grid, $i, $j);
        }
    }
    return @newGrid;
}

sub nextAcreState {
    my ($grid, $i, $j) = @_;
    my $acreType = $grid->[$i][$j];
    if ($acreType eq $Open) {
        return $Trees if countAdjacent($grid, $i, $j, $Trees) >= 3;
    } elsif ($acreType eq $Trees) {
        return $Lumberyard if countAdjacent($grid, $i, $j, $Lumberyard) >= 3;
    } elsif ($acreType eq $Lumberyard) {
        return $Lumberyard if countAdjacent($grid, $i, $j, $Lumberyard) >= 1 && countAdjacent($grid, $i, $j, $Trees) >= 1;
        return $Open;
    }
    return $acreType;
}

sub countAdjacent {
    my ($grid, $i, $j, $acreType) = @_;
    my $count = 0;
    for my $x (-1, 0, 1) {
        for my $y (-1, 0, 1) {
            next if $x == 0 && $y == 0;
            if ($i+$x >= 0 && $i+$x < @{$grid} && $j+$y >= 0 && $j+$y < @{$grid->[$i]} && $grid->[$i+$x][$j+$y] eq $acreType) {
                $count++;
            }
        }
    }
    return $count;
}

sub countResources {
    my ($grid) = @_;
    my ($wooded, $lumberyards) = (0, 0);
    for my $i (0..$#{$grid}) {
        for my $j (0..$#{$grid->[$i]}) {
            $wooded++ if $grid->[$i][$j] eq $Trees;
            $lumberyards++ if $grid->[$i][$j] eq $Lumberyard;
        }
    }
    return ($wooded, $lumberyards);
}

sub gridToString {
    my ($grid) = @_;
    my $str = '';
    for my $row (@{$grid}) {
        $str .= join('', @{$row}) . "\n";
    }
    return $str;
}
