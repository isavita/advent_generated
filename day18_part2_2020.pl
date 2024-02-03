
use strict;
use warnings;

sub parseInput {
    my $input = shift;
    my @lines = split("\n", $input);
    my @ans;
    foreach my $l (@lines) {
        push @ans, [split("", join("", split(" ", $l)))];
    }
    return @ans;
}

sub doMaths {
    my ($input, $flatteningFunc) = @_;
    my @stackOpenIndices;
    my @stackFlattened;
    foreach my $i (0..$#{$input}) {
        push @stackFlattened, $input->[$i];
        if ($input->[$i] eq "(") {
            push @stackOpenIndices, $#stackFlattened;
        } elsif ($input->[$i] eq ")") {
            my $openIndex = $stackOpenIndices[$#stackOpenIndices];
            pop @stackOpenIndices;
            my @sliToFlatten = @stackFlattened[$openIndex+1..$#stackFlattened-1];
            $stackFlattened[$openIndex] = $flatteningFunc->(\@sliToFlatten);
            @stackFlattened = @stackFlattened[0..$openIndex];
        }
    }
    return toInt($flatteningFunc->(\@stackFlattened));
}

sub calcFlatSlicePart {
    my $input = shift;
    foreach my $v (@{$input}) {
        if ($v eq "(" || $v eq ")") {
            die "unexpected paren in flat input, @{$input}";
        }
    }
    for (my $i = 1; $i < @{$input}-1; $i++) {
        if ($input->[$i] eq "+") {
            my $toLeft = $input->[$i-1];
            my $toRight = $input->[$i+1];
            if (isNum($toLeft) && isNum($toRight)) {
                $input->[$i-1] = addStrings($toLeft, $toRight);
                splice(@{$input}, $i, 2);
                $i--;
            }
        }
    }
    for (my $i = 1; $i < @{$input}-1; $i++) {
        if ($input->[$i] eq "*") {
            my $toLeft = $input->[$i-1];
            my $toRight = $input->[$i+1];
            if (isNum($toLeft) && isNum($toRight)) {
                $input->[$i-1] = multiplyStrings($toLeft, $toRight);
                splice(@{$input}, $i, 2);
                $i--;
            }
        }
    }
    return $input->[0];
}

sub isNum {
    my $str = shift;
    return $str =~ /[0-9]/;
}

sub addStrings {
    my $sum = 0;
    foreach my $str (@_) {
        $sum += toInt($str);
    }
    return toString($sum);
}

sub multiplyStrings {
    my $sum = 1;
    foreach my $str (@_) {
        $sum *= toInt($str);
    }
    return toString($sum);
}

sub splice {
    my ($sli, $startIndex, $items) = @_;
    splice @{$sli}, $startIndex, $items;
    return @{$sli};
}

sub toInt {
    my $s = shift;
    die "error converting to int" unless $s =~ /^[0-9]+$/;
    return int($s);
}

sub toString {
    my $n = shift;
    return "$n";
}

open(my $fh, '<', "input.txt") or die "Error opening file: $!";
my $input = do { local $/; <$fh> };
close($fh);

$input =~ s/^\s+|\s+$//g;
my @lines = parseInput($input);
my $total = 0;

foreach my $line (@lines) {
    $total += doMaths($line, \&calcFlatSlicePart);
}

print "$total\n";
