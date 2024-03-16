use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Could not open file 'input.txt' $!";
my @numbers = split(',', <$fh>);
my @boards;

while (my $line = <$fh>) {
    my @boardNumbers;
    for (1..5) {
        my $row = <$fh>;
        push @boardNumbers, [split(' ', $row)];
    }
    push @boards, \@boardNumbers;
}

close $fh;

sub mark {
    my ($board, $number) = @_;
    for my $i (0..4) {
        for my $j (0..4) {
            if ($board->[$i][$j] == $number) {
                $board->[$i][$j] = 0;
            }
        }
    }
}

sub hasWon {
    my $board = shift;
    for my $i (0..4) {
        if (isRowMarked($board->[$i]) || isColumnMarked($board, $i)) {
            return 1;
        }
    }
    return 0;
}

sub isRowMarked {
    my $row = shift;
    for my $marked (@$row) {
        return 0 unless $marked == 0;
    }
    return 1;
}

sub isColumnMarked {
    my ($board, $column) = @_;
    for my $row (@$board) {
        return 0 unless $row->[$column] == 0;
    }
    return 1;
}

my $winningBoard;
my $winningNumber;
for my $number (@numbers) {
    for my $board (@boards) {
        mark($board, $number);
        if (hasWon($board)) {
            $winningBoard = $board;
            $winningNumber = $number;
            last;
        }
    }
    last if defined $winningBoard;
}

my $sum = 0;
for my $i (0..4) {
    for my $j (0..4) {
        $sum += $winningBoard->[$i][$j] if $winningBoard->[$i][$j] != 0;
    }
}

print $sum * $winningNumber;