use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my @instructions;
while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}

my %registers = ('a' => 0, 'b' => 0, 'c' => 1, 'd' => 0);
executeInstructions(\@instructions, \%registers);

print $registers{'a'} . "\n";

sub executeInstructions {
    my ($instructions, $registers) = @_;

    my $i = 0;
    while ($i < scalar(@$instructions)) {
        my @parts = split(' ', $instructions->[$i]);
        my $instruction = $parts[0];

        if ($instruction eq 'cpy') {
            my $val = getValue($parts[1], $registers);
            $registers->{$parts[2]} = $val;
            $i++;
        } elsif ($instruction eq 'inc') {
            $registers->{$parts[1]}++;
            $i++;
        } elsif ($instruction eq 'dec') {
            $registers->{$parts[1]}--;
            $i++;
        } elsif ($instruction eq 'jnz') {
            my $val = getValue($parts[1], $registers);
            if ($val != 0) {
                my $jump = $parts[2];
                $i += $jump;
            } else {
                $i++;
            }
        }
    }
}

sub getValue {
    my ($s, $registers) = @_;
    
    if ($s =~ /^\d+$/) {
        return $s;
    } else {
        return $registers->{$s};
    }
}