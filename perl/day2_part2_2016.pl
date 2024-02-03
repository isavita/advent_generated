
use strict;
use warnings;

my $file = 'input.txt';
open(my $fh, '<', $file) or die "Could not open file '$file' $!";
my @instructions;
while (my $row = <$fh>) {
    chomp $row;
    push @instructions, $row;
}
close $fh;

my %keypad = (
    "1" => {'D' => "3"},
    "2" => {'R' => "3", 'D' => "6"},
    "3" => {'U' => "1", 'R' => "4", 'D' => "7", 'L' => "2"},
    "4" => {'L' => "3", 'D' => "8"},
    "5" => {'R' => "6"},
    "6" => {'U' => "2", 'R' => "7", 'D' => "A", 'L' => "5"},
    "7" => {'U' => "3", 'R' => "8", 'D' => "B", 'L' => "6"},
    "8" => {'U' => "4", 'R' => "9", 'D' => "C", 'L' => "7"},
    "9" => {'L' => "8"},
    "A" => {'U' => "6", 'R' => "B"},
    "B" => {'U' => "7", 'R' => "C", 'D' => "D", 'L' => "A"},
    "C" => {'U' => "8", 'L' => "B"},
    "D" => {'U' => "B"}
);
my $position = "5";
my $code = "";

foreach my $instruction (@instructions) {
    foreach my $move (split //, $instruction) {
        if (exists $keypad{$position}{$move}) {
            $position = $keypad{$position}{$move};
        } else {
            $position = $position;
        }
    }
    $code .= $position;
}

print $code;
