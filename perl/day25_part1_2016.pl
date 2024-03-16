use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @instructions;
while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}
close($fh);

my $a = 1;
while (1) {
    if (producesClockSignal($a, \@instructions)) {
        print "$a\n";
        last;
    }
    $a++;
}

sub producesClockSignal {
    my ($a, $instructions) = @_;
    my %registers = ('a' => $a, 'b' => 0, 'c' => 0, 'd' => 0);
    my ($lastOutput, $outputCount) = (0, 0);

    my $i = 0;
    while ($i < scalar(@$instructions)) {
        my @parts = split(' ', $instructions->[$i]);
        my $command = $parts[0];

        if ($command eq 'cpy') {
            my $val = getValue($parts[1], \%registers);
            $registers{$parts[2]} = $val;
        } elsif ($command eq 'inc') {
            $registers{$parts[1]}++;
        } elsif ($command eq 'dec') {
            $registers{$parts[1]}--;
        } elsif ($command eq 'jnz') {
            my $val = getValue($parts[1], \%registers);
            if ($val != 0) {
                my $jump = $parts[2];
                $i += $jump;
                next;
            }
        } elsif ($command eq 'out') {
            my $val = getValue($parts[1], \%registers);
            if ($val != 0 && $val != 1) {
                return 0;
            }
            if ($outputCount > 0 && $val == $lastOutput) {
                return 0;
            }
            $lastOutput = $val;
            $outputCount++;
            if ($outputCount > 50) {
                return 1;
            }
        }
        $i++;
    }
    return 0;
}

sub getValue {
    my ($s, $registers) = @_;
    if ($s =~ /^\d+$/) {
        return $s;
    } else {
        return $registers->{$s} || 0;
    }
}