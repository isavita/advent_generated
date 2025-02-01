#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';

my %roomTarget = (
  "2,3" => "A", "3,3" => "A", "4,3" => "A", "5,3" => "A",
  "2,5" => "B", "3,5" => "B", "4,5" => "B", "5,5" => "B",
  "2,7" => "C", "3,7" => "C", "4,7" => "C", "5,7" => "C",
  "2,9" => "D", "3,9" => "D", "4,9" => "D", "5,9" => "D",
);
my %frontRoom = map { $_ => 1 } ("1,3", "1,5", "1,7", "1,9");

sub main {
    open my $fh, "<", "input.txt" or die $!;
    my $input = do { local $/; <$fh> };
    close $fh;
    chomp $input;
    say amphipod($input);
}
sub amphipod {
    my ($input) = @_;
    my $start = parse_input($input);
    push @{$start->{grid}}, [], [];
    $start->{grid}[6] = $start->{grid}[4];
    $start->{grid}[5] = $start->{grid}[3];
    $start->{grid}[3] = [ split //, "  #D#C#B#A#  " ];
    $start->{grid}[4] = [ split //, "  #D#B#A#C#  " ];
    my $heap = [];
    heap_push($heap, $start);
    my %seen;
    while(my $state = heap_pop($heap)){
        my $key = join "\n", map { join("", @$_) } @{$state->{grid}};
        next if $seen{$key}++;
        return $state->{energy} if all_done($state);
        my @unsettled = get_unsettled_coords($state);
        for my $coord (@unsettled) {
            my ($ur, $uc) = @$coord;
            my @moves = get_next_possible_moves($state, $coord);
            for my $dest (@moves) {
                my ($nr, $nc) = @$dest;
                die "non-empty destination" unless $state->{grid}[$nr][$nc] eq ".";
                my $cp = copy_state($state);
                $cp->{energy} += calc_energy($cp->{grid}[$ur][$uc], [$ur,$uc], [$nr,$nc]);
                ($cp->{grid}[$nr][$nc], $cp->{grid}[$ur][$uc]) = ($cp->{grid}[$ur][$uc], $cp->{grid}[$nr][$nc]);
                heap_push($heap, $cp);
            }
        }
    }
    die "No solution";
}
sub parse_input {
    my ($input) = @_;
    my @grid = map { [ split //, $_ ] } split /\n/, $input;
    return { grid => \@grid, energy => 0, path => "" };
}
sub all_done {
    my ($st) = @_;
    for my $key (keys %roomTarget) {
        my ($r, $c) = split /,/, $key;
        return 0 if $st->{grid}[$r][$c] ne $roomTarget{$key};
    }
    return 1;
}
sub get_unsettled_coords {
    my ($st) = @_;
    my @res;
    my $grid = $st->{grid};
    my $cols = scalar(@{$grid->[0]});
    for my $c (1 .. $cols-1) {
        push @res, [1,$c] if $grid->[1][$c] =~ /[ABCD]/;
    }
    for my $c (3,5,7,9) {
        my $roomFull = 1;
        for (my $r = scalar(@$grid)-2; $r >= 2; $r--) {
            my $cell = $grid->[$r][$c];
            my $want = $roomTarget{"$r,$c"};
            if ($cell ne ".") {
                if ($cell ne $want) {
                    $roomFull = 0;
                    push @res, [$r,$c];
                } elsif (!$roomFull) {
                    push @res, [$r,$c];
                }
            }
        }
    }
    return @res;
}
sub get_next_possible_moves {
    my ($st, $startCoord) = @_;
    my ($sr, $sc) = @$startCoord;
    my $grid = $st->{grid};
    my $char = $grid->[$sr][$sc];
    die "unexpected $char" unless $char =~ /[ABCD]/;
    my $startedInHall = ($sr == 1);
    my @poss;
    my @queue = ([$sr,$sc]);
    my %visited;
    while(@queue){
        my $cur = shift @queue;
        my ($r,$c) = @$cur;
        my $key = "$r,$c";
        next if $visited{$key}++;
        if($r != $sr or $c != $sc){
            unless(exists $frontRoom{"$r,$c"}){
                if(!exists $roomTarget{"$r,$c"}){
                    push @poss, [$r,$c] unless $startedInHall;
                } else {
                    if($roomTarget{"$r,$c"} eq $char){
                        my $room_deeper = 0;
                        my $stuck = 0;
                        for(my $nr = $r+1; $nr < scalar(@$grid)-1; $nr++){
                            my $cell = $grid->[$nr][$c];
                            $room_deeper = 1 if $cell eq ".";
                            $stuck = 1 if $cell ne "." && $cell ne $char;
                        }
                        push @poss, [$r,$c] if not($room_deeper or $stuck);
                    }
                }
            }
        }
        for my $d ( [ -1, 0 ], [ 1, 0 ], [ 0, -1 ], [ 0, 1 ] ) {
            my ($dr, $dc) = @$d;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            next unless defined $grid->[$nr] and defined $grid->[$nr][$nc];
            push @queue, [$nr,$nc] if $grid->[$nr][$nc] eq ".";
        }
    }
    return @poss;
}
sub calc_energy {
    my ($char, $start, $end) = @_;
    my ($sr,$sc) = @$start;
    my ($er,$ec) = @$end;
    my $dist = abs($ec - $sc) + ($sr - 1) + ($er - 1);
    my %energy = ( A=>1, B=>10, C=>100, D=>1000 );
    die "$char invalid" unless exists $energy{$char};
    return $energy{$char} * $dist;
}
sub copy_state {
    my ($st) = @_;
    my @newGrid = map { [ @$_ ] } @{$st->{grid}};
    return { grid => \@newGrid, energy => $st->{energy}, path => $st->{path} };
}
sub heap_push {
    my ($heap, $item) = @_;
    push @$heap, $item;
    my $i = $#$heap;
    while ($i > 0) {
        my $p = int(($i-1)/2);
        last if $heap->[$p]{energy} <= $heap->[$i]{energy};
        @$heap[$p,$i] = @$heap[$i,$p];
        $i = $p;
    }
}
sub heap_pop {
    my ($heap) = @_;
    return unless @$heap;
    my $ret = $heap->[0];
    my $last = pop @$heap;
    if (@$heap) {
        $heap->[0] = $last;
        my $i = 0;
        my $n = scalar(@$heap);
        while (1) {
            my $l = 2*$i+1;
            my $r = 2*$i+2;
            my $small = $i;
            if ($l < $n and $heap->[$l]{energy} < $heap->[$small]{energy}) {
                $small = $l;
            }
            if ($r < $n and $heap->[$r]{energy} < $heap->[$small]{energy}) {
                $small = $r;
            }
            last if $small == $i;
            @$heap[$i,$small] = @$heap[$small,$i];
            $i = $small;
        }
    }
    return $ret;
}
main();