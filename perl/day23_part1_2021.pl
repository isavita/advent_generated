use strict;
use warnings;

package MinHeap;

sub new {
    my $class = shift;
    my $self = {
        nodes => [],
        closerToRoot => sub {
            my ($val1, $val2) = @_;
            return $val1 < $val2;
        },
    };
    bless $self, $class;
    return $self;
}

sub Front {
    my $self = shift;
    return scalar(@{$self->{nodes}}) == 0 ? undef : $self->{nodes}[0];
}

sub Add {
    my ($self, $newNode) = @_;
    push @{$self->{nodes}}, $newNode;
    $self->heapifyFromEnd();
}

sub Remove {
    my $self = shift;
    return undef if scalar(@{$self->{nodes}}) == 0;

    my $rootNode = $self->{nodes}[0];
    $self->{nodes}[0] = $self->{nodes}[-1];
    pop @{$self->{nodes}};
    $self->heapifyFromStart();

    return $rootNode;
}

sub Length {
    my $self = shift;
    return scalar(@{$self->{nodes}});
}

sub swap {
    my ($self, $i, $j) = @_;
    @{$self->{nodes}}[$i, $j] = @{$self->{nodes}}[$j, $i];
}

sub heapifyFromEnd {
    my $self = shift;
    my $currentIndex = $#{$self->{nodes}};
    while ($currentIndex > 0) {
        my $parentIndex = int(($currentIndex - 1) / 2);
        my $parentNode = $self->{nodes}[$parentIndex];
        if ($self->{closerToRoot}->($self->{nodes}[$currentIndex]->Value(), $parentNode->Value())) {
            $self->swap($parentIndex, $currentIndex);
            $currentIndex = $parentIndex;
        } else {
            last;
        }
    }
}

sub heapifyFromStart {
    my $self = shift;
    my $currentIndex = 0;

    while (1) {
        my $smallerChildIndex = $currentIndex;
        for my $i (1..2) {
            my $childIndex = $currentIndex * 2 + $i;
            if ($childIndex < scalar(@{$self->{nodes}}) &&
                $self->{closerToRoot}->($self->{nodes}[$childIndex]->Value(), $self->{nodes}[$smallerChildIndex]->Value())) {
                $smallerChildIndex = $childIndex;
            }
        }

        last if $smallerChildIndex == $currentIndex;

        $self->swap($smallerChildIndex, $currentIndex);
        $currentIndex = $smallerChildIndex;
    }
}

package MaxHeap;

sub new {
    my $class = shift;
    my $self = {
        nodes => [],
        closerToRoot => sub {
            my ($val1, $val2) = @_;
            return $val1 > $val2;
        },
    };
    bless $self, $class;
    return $self;
}

package State;

sub new {
    my ($class, $grid, $energyUsed, $path) = @_;
    my $self = {
        grid => $grid,
        energyUsed => $energyUsed // 0,
        path => $path // '',
    };
    bless $self, $class;
    return $self;
}

sub Value {
    my $self = shift;
    return $self->{energyUsed};
}

sub copy {
    my $self = shift;
    my $cp = State->new(
        [ map { [ @$_ ] } @{$self->{grid}} ],
        $self->{energyUsed},
        $self->{path},
    );
    return $cp;
}

sub allDone {
    my ($self, $roomCoordToWantChar) = @_;
    for my $coord (keys %$roomCoordToWantChar) {
        my ($row, $col) = split /,/, $coord;
        return 0 if $self->{grid}[$row][$col] ne $roomCoordToWantChar->{$coord};
    }
    return 1;
}

sub getUnsettledCoords {
    my ($self, $roomCoordToWantChar) = @_;
    my @unsettled;

    for my $col (1..$#{$self->{grid}[0]}) {
        if ($self->{grid}[1][$col] =~ /[ABCD]/) {
            push @unsettled, [1, $col];
        }
    }

    for my $col (3, 5, 7, 9) {
        my $roomFullFromBack = 1;
        for my $row (reverse 2..$#{$self->{grid}}-1) {
            my $coord = "$row,$col";
            my $wantChar = $roomCoordToWantChar->{$coord};
            my $gotChar = $self->{grid}[$row][$col];
            if ($gotChar ne '.') {
                if ($gotChar ne $wantChar) {
                    $roomFullFromBack = 0;
                    push @unsettled, [$row, $col];
                } elsif ($gotChar eq $wantChar && !$roomFullFromBack) {
                    push @unsettled, [$row, $col];
                }
            }
        }
    }
    return @unsettled;
}

sub getNextPossibleMoves {
    my ($self, $unsettledCoord, $roomCoordToWantChar, $coordsInFrontOfRooms) = @_;
    my ($ur, $uc) = @$unsettledCoord;
    my $unsettledChar = $self->{grid}[$ur][$uc];

    die "unexpected character to get next moves for $unsettledChar" unless $unsettledChar =~ /[ABCD]/;

    my @possible;
    my $startedInHallway = $ur == 1;

    my @queue = ($unsettledCoord);
    my %seen;
    while (@queue) {
        my $front = shift @queue;
        my ($fr, $fc) = @$front;
        next if $seen{"$fr,$fc"};
        $seen{"$fr,$fc"} = 1;

        if ($front ne $unsettledCoord) {
            if (!$coordsInFrontOfRooms->{"$fr,$fc"}) {
                my $wantChar = $roomCoordToWantChar->{"$fr,$fc"};
                if (!defined $wantChar) {
                    push @possible, $front unless $startedInHallway;
                } elsif ($wantChar eq $unsettledChar) {
                    my $isStuckAmphipod = 0;
                    my $roomHasDeeperOpenSpaces = 0;
                    for my $r ($fr+1..$#{$self->{grid}}-1) {
                        my $char = $self->{grid}[$r][$fc];
                        if ($char eq '.') {
                            $roomHasDeeperOpenSpaces = 1;
                        }
                        if ($char ne '.' && $char ne $unsettledChar) {
                            $isStuckAmphipod = 1;
                            last;
                        }
                    }

                    push @possible, $front if !$roomHasDeeperOpenSpaces && !$isStuckAmphipod;
                }
            }
        }

        for my $d ([-1, 0], [1, 0], [0, -1], [0, 1]) {
            my ($nr, $nc) = ($fr + $d->[0], $fc + $d->[1]);
            if ($self->{grid}[$nr][$nc] eq '.') {
                push @queue, [$nr, $nc];
            }
        }
    }

    return @possible;
}

my %roomCoordToWantChar = (
    '2,3' => 'A', '3,3' => 'A',
    '2,5' => 'B', '3,5' => 'B',
    '2,7' => 'C', '3,7' => 'C',
    '2,9' => 'D', '3,9' => 'D',
);

my %coordsInFrontOfRooms = (
    '1,3' => 1,
    '1,5' => 1,
    '1,7' => 1,
    '1,9' => 1,
);

sub calcEnergy {
    my ($char, $start, $end) = @_;
    my ($sr, $sc) = @$start;
    my ($er, $ec) = @$end;

    my $dist = abs($ec - $sc);
    $dist += $sr - 1;
    $dist += $er - 1;

    my %energyPerType = (
        A => 1,
        B => 10,
        C => 100,
        D => 1000,
    );

    die "$char should not call calcEnergy()" unless exists $energyPerType{$char};
    return $energyPerType{$char} * $dist;
}

sub parseInput {
    my $input = shift;
    my @grid;
    for my $line (split /\n/, $input) {
        push @grid, [split //, $line];
    }
    return State->new(\@grid);
}

sub amphipod {
    my $input = shift;
    my $start = parseInput($input);

    my $minHeap = MinHeap->new();
    $minHeap->Add($start);
    my %seenGrids;
    while ($minHeap->Length() > 0) {
        my $front = $minHeap->Remove();
        my $key = join '', map { join '', @$_ } @{$front->{grid}};
        next if $seenGrids{$key};
        $seenGrids{$key} = 1;

        return $front->{energyUsed} if $front->allDone(\%roomCoordToWantChar);

        my @unsettledCoords = $front->getUnsettledCoords(\%roomCoordToWantChar);
        for my $unsettledCoord (@unsettledCoords) {
            my ($ur, $uc) = @$unsettledCoord;
            my @nextMoves = $front->getNextPossibleMoves($unsettledCoord, \%roomCoordToWantChar, \%coordsInFrontOfRooms);
            for my $nextCoord (@nextMoves) {
                my ($nr, $nc) = @$nextCoord;
                die sprintf "should only be moving to walkable spaces, got '%s' at %d,%d", $front->{grid}[$nr][$nc], $nr, $nc if $front->{grid}[$nr][$nc] ne '.';

                my $cp = $front->copy();
                $cp->{energyUsed} += calcEnergy($cp->{grid}[$ur][$uc], $unsettledCoord, $nextCoord);
                $cp->{path} .= sprintf "%s%s->%s{%d},", $front->{grid}[$ur][$uc], join(',', @$unsettledCoord), join(',', @$nextCoord), $cp->{energyUsed};
                ($cp->{grid}[$nr][$nc], $cp->{grid}[$ur][$uc]) = ($cp->{grid}[$ur][$uc], $cp->{grid}[$nr][$nc]);

                $minHeap->Add($cp);
            }
        }
    }

    die "should return from loop";
}

sub main {
    open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
    my $input = do { local $/; <$fh> };
    close $fh;
    $input =~ s/\s+$//;

    my $ans = amphipod($input);
    print "$ans\n";
}

main();