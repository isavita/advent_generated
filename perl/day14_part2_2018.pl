open(my $fh, '<', 'input.txt') or die $!;
my $input = <$fh>;
chomp $input;

my @scoreboard = (3, 7);
my ($elf1, $elf2) = (0, 1);
my @inputSequence = split('', $input);

while (1) {
    my $newScore = $scoreboard[$elf1] + $scoreboard[$elf2];
    if ($newScore >= 10) {
        push @scoreboard, int($newScore / 10);
        last if checkSequence(\@scoreboard, \@inputSequence);
    }
    push @scoreboard, $newScore % 10;
    last if checkSequence(\@scoreboard, \@inputSequence);

    $elf1 = ($elf1 + $scoreboard[$elf1] + 1) % scalar @scoreboard;
    $elf2 = ($elf2 + $scoreboard[$elf2] + 1) % scalar @scoreboard;
}

print scalar @scoreboard - scalar @inputSequence;

sub checkSequence {
    my ($scoreboard, $sequence) = @_;
    return 0 if scalar @$scoreboard < scalar @$sequence;
    my $start = scalar @$scoreboard - scalar @$sequence;
    foreach my $i (0 .. $#{$sequence}) {
        return 0 if $scoreboard->[$start + $i] != $sequence->[$i];
    }
    return 1;
}