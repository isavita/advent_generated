use strict;
use warnings;

my @root = ("");
my %dirs;
my %files;
my @curr;

open(my $fh, "<", "input.txt") or die "Could not open file 'input.txt': $!";

while (my $line = <$fh>) {
    chomp $line;
    my @txt = split /\s+/, $line;
    if ($txt[0] eq '$') {
        if ($txt[1] eq 'cd') {
            if ($txt[2] eq '/') {
                @curr = @root;
            } elsif ($txt[2] eq '..') {
                pop @curr;
            } else {
                push @curr, $txt[2];
            }
            $dirs{join '/', @curr} = 0;
        }
    } else {
        if ($txt[0] ne 'dir') {
            $files{join '/', @curr, $txt[1]} = $txt[0];
        }
    }
}

close $fh;

foreach my $f (keys %files) {
    my @path = split /\//, $f;
    for (my $i = 1; $i < @path; $i++) {
        $dirs{join '/', @path[0..$i-1]} += $files{$f};
    }
}

my @sortedSizes = sort { $a <=> $b } values %dirs;
my $total = 70000000;
my $want = 30000000;
my $available = $total - $dirs{""};
my $index = 0;
$index++ while $index < @sortedSizes && $sortedSizes[$index] < $want - $available;

print $sortedSizes[$index], "\n";