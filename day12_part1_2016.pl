
use strict;
use warnings;

my %registers = ("a" => 0, "b" => 0, "c" => 0, "d" => 0);
my @instructions;

open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    push @instructions, $line;
}
close($fh);

executeInstructions(\@instructions, \%registers);

print $registers{"a"} . "\n";

sub executeInstructions {
    my ($instructions, $registers) = @_;
    my $i = 0;
    while ($i < scalar(@$instructions)) {
        my @parts = split(' ', $instructions->[$i]);
        if ($parts[0] eq "cpy") {
            my $val = getValue($parts[1], $registers);
            $registers->{$parts[2]} = $val;
            $i++;
        }
        elsif ($parts[0] eq "inc") {
            $registers->{$parts[1]}++;
            $i++;
        }
        elsif ($parts[0] eq "dec") {
            $registers->{$parts[1]}--;
            $i++;
        }
        elsif ($parts[0] eq "jnz") {
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
