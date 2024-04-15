use strict;
use warnings;

package BoardState;

sub new {
    my ($class, $board) = @_;
    my @picked;
    foreach my $row (@$board) {
        push @picked, [(0) x @$row];
    }
    return bless {
        board => $board,
        picked => \@picked,
    }, $class;
}

sub PickNum {
    my ($self, $num) = @_;
    my $board = $self->{board};
    my $picked = $self->{picked};

    for (my $r = 0; $r < @$board; $r++) {
        for (my $c = 0; $c < @{$board->[$r]}; $c++) {
            if ($board->[$r][$c] == $num) {
                $picked->[$r][$c] = 1;
            }
        }
    }

    for (my $i = 0; $i < @$board; $i++) {
        my ($isFullRow, $isFullCol) = (1, 1);

        for (my $j = 0; $j < @$board; $j++) {
            $isFullRow &&= $picked->[$i][$j];
            $isFullCol &&= $picked->[$j][$i];
        }

        return 1 if $isFullRow || $isFullCol;
    }

    return 0;
}

sub Score {
    my ($self) = @_;
    my $board = $self->{board};
    my $picked = $self->{picked};

    my $score = 0;
    for (my $r = 0; $r < @$board; $r++) {
        for (my $c = 0; $c < @{$board->[$r]}; $c++) {
            $score += $board->[$r][$c] unless $picked->[$r][$c];
        }
    }

    return $score;
}

package main;

sub solve {
    my ($input) = @_;
    my ($nums, $boards) = parseInput($input);

    my $lastWinningScore = -1;
    my %alreadyWon;
    foreach my $n (@$nums) {
        for (my $bi = 0; $bi < @$boards; $bi++) {
            next if $alreadyWon{$bi};
            my $didWin = $boards->[$bi]->PickNum($n);
            if ($didWin) {
                $lastWinningScore = $boards->[$bi]->Score() * $n;
                $alreadyWon{$bi} = 1;
            }
        }
    }

    return $lastWinningScore;
}

sub parseInput {
    my ($input) = @_;
    my @lines = split(/\n\n/, $input);

    my @nums = split(/,/, $lines[0]);

    my @boards;
    foreach my $grid (@lines[1..$#lines]) {
        my @b;
        foreach my $line (split(/\n/, $grid)) {
            $line =~ s/\s+/ /g;
            $line =~ s/^\s+//;
            my @row = split(/\s+/, $line);
            push @b, \@row;
        }
        push @boards, BoardState->new(\@b);
    }

    return (\@nums, \@boards);
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my $input = do { local $/; <$fh> };
close $fh;

my $result = solve($input);
print "$result\n";