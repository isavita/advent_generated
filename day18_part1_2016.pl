
use strict;
use warnings;

my $totalRows = 40; # Total number of rows to generate

sub readFirstRow {
    my $filename = shift;
    open(my $fh, '<', $filename) or die "Failed to open file: $!";
    my $firstRow = <$fh>;
    close($fh);
    return $firstRow;
}

sub countSafeTiles {
    my ($firstRow, $totalRows) = @_;
    my $currentRow = $firstRow;
    my $safeCount = countChar($currentRow, '.');

    for (my $i = 1; $i < $totalRows; $i++) {
        my $nextRow = "";
        for (my $j = 0; $j < length($currentRow); $j++) {
            if (isTrap($j-1, $j, $j+1, $currentRow)) {
                $nextRow .= "^";
            } else {
                $nextRow .= ".";
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

    return (($l eq '^' && $c eq '^' && $r eq '.') ||
            ($c eq '^' && $r eq '^' && $l eq '.') ||
            ($l eq '^' && $c eq '.' && $r eq '.') ||
            ($r eq '^' && $c eq '.' && $l eq '.'));
}

sub safeIfOutOfBounds {
    my ($index, $row) = @_;
    if ($index < 0 || $index >= length($row)) {
        return '.';
    }
    return substr($row, $index, 1);
}

sub countChar {
    my ($str, $char) = @_;
    my $count = 0;
    for my $c (split //, $str) {
        if ($c eq $char) {
            $count++;
        }
    }
    return $count;
}

my $firstRow = readFirstRow("input.txt");
my $safeTilesCount = countSafeTiles($firstRow, $totalRows);
print "$safeTilesCount\n";
