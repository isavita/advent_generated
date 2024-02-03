
use strict;
use warnings;

my $input = readFile("input.txt");
my @grid;
foreach my $line (split("\n", $input)) {
    push @grid, [split("", $line)];
}

my @graph;
foreach my $r (0 .. $#grid) {
    foreach my $c (0 .. $#{$grid[$r]}) {
        my $cell = $grid[$r][$c];
        if ($cell =~ /[0-9]/) {
            my $poi = $cell;
            my @distancesFromPOI = bfsGetEdgeWeights(\@grid, [$r, $c]);

            if (!@graph) {
                for (my $i = 0; $i < scalar @distancesFromPOI; $i++) {
                    push @graph, [(0) x scalar @distancesFromPOI];
                }
            }
            my $index = int($poi);
            $graph[$index] = \@distancesFromPOI;
        }
    }
}

my $result = dfs(\@graph, 0, {0 => 1}, 0);
print "$result\n";

sub bfsGetEdgeWeights {
    my ($grid, $start) = @_;
    my %poiToDistance = ($grid->[$start->[0]][$start->[1]] => 0);
    my @queue = ([$start->[0], $start->[1], 0]);
    my %visited;
    my @dirs = ([0, -1], [0, 1], [1, 0], [-1, 0]);

    while (@queue) {
        my $front = shift @queue;
        my ($row, $col, $distance) = @$front;

        if ($visited{"$row,$col"}) {
            next;
        }
        $visited{"$row,$col"} = 1;

        if ($grid->[$row][$col] =~ /[0-9]/) {
            $poiToDistance{$grid->[$row][$col]} = $distance;
        }
        foreach my $d (@dirs) {
            my ($nextRow, $nextCol) = ($row + $d->[0], $col + $d->[1]);

            if ($grid->[$nextRow][$nextCol] ne "#") {
                push @queue, [$nextRow, $nextCol, $distance + 1];
            }
        }
    }

    my @distances;
    foreach my $numStr (keys %poiToDistance) {
        my $n = int($numStr);
        $distances[$n] = $poiToDistance{$numStr};
    }
    return @distances;
}

sub dfs {
    my ($graph, $entryIndex, $visited, $returnToZero) = @_;

    if (scalar @$graph == scalar keys %$visited) {
        if ($returnToZero) {
            return $graph->[$entryIndex][0];
        }
        return 0;
    }

    my $minDistance = 999999999;
    foreach my $i (0 .. $#{$graph->[$entryIndex]}) {
        if (!$visited->{$i}) {
            $visited->{$i} = 1;

            my $dist = $graph->[$entryIndex][$i] + dfs($graph, $i, $visited, $returnToZero);
            $minDistance = minInt($minDistance, $dist);

            delete $visited->{$i};
        }
    }

    return $minDistance;
}

sub readFile {
    my ($pathFromCaller) = @_;
    my ($filename) = (caller(0))[1];
    my $absolutePath = $filename;
    $absolutePath =~ s/[^\/]*$//;
    $absolutePath .= $pathFromCaller;

    open(my $fh, '<', $absolutePath) or die "Cannot open file: $!";
    my $content = do { local $/; <$fh> };
    close($fh);

    $content =~ s/\n$//;
    return $content;
}

sub minInt {
    my ($a, $b) = @_;
    return $a < $b ? $a : $b;
}
