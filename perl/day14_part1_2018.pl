
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = <$fh>;
chomp $input;
close($fh);

my @scoreboard = (3, 7);
my ($elf1, $elf2) = (0, 1);

while (@scoreboard < $input + 10) {
    my $newScore = $scoreboard[$elf1] + $scoreboard[$elf2];
    if ($newScore >= 10) {
        push @scoreboard, int($newScore / 10);
    }
    push @scoreboard, $newScore % 10;

    $elf1 = ($elf1 + $scoreboard[$elf1] + 1) % scalar @scoreboard;
    $elf2 = ($elf2 + $scoreboard[$elf2] + 1) % scalar @scoreboard;
}

for my $i ($input..$input+9) {
    print $scoreboard[$i];
}
print "\n";
