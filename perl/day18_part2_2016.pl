use strict;
use warnings;

my $totalRows = 400000;

sub main {
    my $firstRow = readFirstRow("input.txt");
    my $safeTilesCount = countSafeTiles($firstRow, $totalRows);
    print "$safeTilesCount\n";
}

sub readFirstRow {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open file: $!";
    my $firstRow = <$fh>;
    chomp $firstRow;
    close $fh;
    return $firstRow;
}

sub countSafeTiles {
    my ($firstRow, $totalRows) = @_;
    my $currentRow = $firstRow;
    my $safeCount = countChar($currentRow, '.');

    for my $i (1 .. $totalRows - 1) {
        my $nextRow = '';
        for my $j (0 .. length($currentRow) - 1) {
            if (isTrap($j-1, $j, $j+1, $currentRow)) {
                $nextRow .= '^';
            } else {
                $nextRow .= '.';
                $safeCount++;
            }
        }
        $currentRow = $nextRow;
    }
    return $safeCount;
}

sub isTrap {
    my ($left, $center, $right, $row) = @_;
    my $l = safeIfOutOfBounds($left, $row);
    my $c = substr($row, $center, 1);
    my $r = safeIfOutOfBounds($right, $row);

    return ($l eq '^' && $c eq '^' && $r eq '.') ||
           ($c eq '^' && $r eq '^' && $l eq '.') ||
           ($l eq '^' && $c eq '.' && $r eq '.') ||
           ($r eq '^' && $c eq '.' && $l eq '.');
}

sub safeIfOutOfBounds {
    my ($index, $row) = @_;
    return $index < 0 || $index >= length($row) ? '.' : substr($row, $index, 1);
}

sub countChar {
    my ($str, $char) = @_;
    my $count = 0;
    $count++ while $str =~ /\Q$char/g;
    return $count;
}

main();