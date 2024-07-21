
use strict;
use warnings;

sub read_instructions {
    open my $fh, '<', 'input.txt' or die $!;
    chomp(my @instructions = <$fh>);
    close $fh;
    return @instructions;
}

sub swap_positions {
    my ($pw, $x, $y) = @_;
    @$pw[$x, $y] = @$pw[$y, $x];
}

sub swap_letters {
    my ($pw, $x, $y) = @_;
    my ($i, $j) = (index(join('', @$pw), $x), index(join('', @$pw), $y));
    swap_positions($pw, $i, $j);
}

sub rotate {
    my ($pw, $steps) = @_;
    my $length = @$pw;
    $steps %= $length;
    $steps += $length if $steps < 0;
    @$pw = (@$pw[$length - $steps .. $length - 1], @$pw[0 .. $length - $steps - 1]);
}

sub rotate_letter {
    my ($pw, $x) = @_;
    my $index = index(join('', @$pw), $x);
    $index++ if $index >= 4;
    rotate($pw, $index + 1);
}

sub derotate_letter {
    my ($pw, $x) = @_;
    my $index = index(join('', @$pw), $x);
    my $rot = $index % 2 == 1 ? -($index + 1) / 2 : $index ? (6 - $index) / 2 : -1;
    rotate($pw, $rot);
}

sub reverse_positions {
    my ($pw, $x, $y) = @_;
    @$pw[$x .. $y] = reverse @$pw[$x .. $y];
}

sub move {
    my ($pw, $x, $y) = @_;
    my $ch = splice(@$pw, $x, 1);
    splice(@$pw, $y, 0, $ch);
}

sub scramble {
    my ($instructions, $pw, $direction) = @_;
    $direction < 0 and @$instructions = reverse @$instructions;
    for my $instruction (@$instructions) {
        my @line = split ' ', $instruction;
        if ($instruction =~ /^swap/) {
            my ($x, $y) = ($line[2], $line[-1]);
            $line[1] eq 'position' ? swap_positions($pw, $x, $y) : swap_letters($pw, $x, $y);
        } elsif ($instruction =~ /^rotate/) {
            if ($line[1] eq 'based') {
                $direction > 0 ? rotate_letter($pw, $line[-1]) : derotate_letter($pw, $line[-1]);
            } else {
                my $x = $line[2];
                $x = -$x if $line[1] eq 'left';
                $direction < 0 and $x = -$x;
                rotate($pw, $x);
            }
        } elsif ($instruction =~ /^reverse/) {
            my ($x, $y) = ($line[2], $line[-1]);
            reverse_positions($pw, $x, $y);
        } elsif ($instruction =~ /^move/) {
            my ($x, $y) = ($line[2], $line[-1]);
            $direction < 0 and ($x, $y) = ($y, $x);
            move($pw, $x, $y);
        }
    }
    return $pw;
}

my @instructions = read_instructions();
my @hashed = split //, 'fbgdceah';
my $result = scramble(\@instructions, \@hashed, -1);
print join('', @$result), "\n";
