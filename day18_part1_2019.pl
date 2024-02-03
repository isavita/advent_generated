
use strict;
use warnings;

my @grid;
my %keyMap;
my $keyCounter = 0;
my $start;

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line;
    for my $x (0 .. length($line) - 1) {
        if (substr($line, $x, 1) eq '@') {
            $start = {x => $x, y => $. - 1};
        } elsif (substr($line, $x, 1) ge 'a' && substr($line, $x, 1) le 'z') {
            $keyMap{substr($line, $x, 1)} = $keyCounter;
            $keyCounter++;
        }
    }
}

close($fh);

my @dirs = ([0, -1], [-1, 0], [0, 1], [1, 0]);
my %visited;
my @queue = ({pos => $start, keys => 0});
my $steps = 0;

while (@queue) {
    my $size = scalar @queue;
    for (my $i = 0; $i < $size; $i++) {
        my $current = shift @queue;

        if ($current->{keys} == (1 << scalar(keys %keyMap)) - 1) {
            print "$steps\n";
            exit;
        }

        for my $d (@dirs) {
            my $next = {x => $current->{pos}{x} + $d->[0], y => $current->{pos}{y} + $d->[1]};
            if ($next->{x} >= 0 && $next->{x} < length($grid[0]) && $next->{y} >= 0 && $next->{y} < scalar @grid) {
                my $char = substr($grid[$next->{y}], $next->{x}, 1);
                if ($char ne '#' && !($char ge 'A' && $char le 'Z' && !($current->{keys} & (1 << $keyMap{lc($char)})))) {
                    my %newState = (pos => $next, keys => $current->{keys});
                    if ($char ge 'a' && $char le 'z') {
                        $newState{keys} |= 1 << $keyMap{$char};
                    }
                    my $stateKey = $newState{pos}{x} . "," . $newState{pos}{y} . "," . $newState{keys};
                    if (!$visited{$stateKey}) {
                        $visited{$stateKey} = 1;
                        push @queue, \%newState;
                    }
                }
            }
        }
    }
    $steps++;
}

print "-1\n";
