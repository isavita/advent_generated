
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @instructions = map { chomp; $_ } <$fh>;
close($fh);

my %registers = ('a' => 1, 'b' => 0);
my $i = 0;

while ($i < scalar @instructions) {
    my @parts = split(' ', $instructions[$i]);

    if ($parts[0] eq 'hlf') {
        $registers{$parts[1]} /= 2;
    } elsif ($parts[0] eq 'tpl') {
        $registers{$parts[1]} *= 3;
    } elsif ($parts[0] eq 'inc') {
        $registers{$parts[1]}++;
    } elsif ($parts[0] eq 'jmp') {
        my $offset = $parts[1];
        $i += $offset - 1;
    } elsif ($parts[0] eq 'jie') {
        if ($registers{substr($parts[1], 0, 1)} % 2 == 0) {
            my $offset = $parts[2];
            $i += $offset - 1;
        }
    } elsif ($parts[0] eq 'jio') {
        if ($registers{substr($parts[1], 0, 1)} == 1) {
            my $offset = $parts[2];
            $i += $offset - 1;
        }
    } else {
        die "Unknown instruction: $parts[0]";
    }

    $i++;
}

print "$registers{'b'}\n";
