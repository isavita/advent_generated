open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my $mask = "";
my %mem;

while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ /^mask = (.*)$/) {
        $mask = $1;
    } else {
        if ($line =~ /mem\[(\d+)] = (\d+)/) {
            my $address = $1;
            my $value = $2;
            my @floating = ();
            my $addressMasked = $address;
            my @bits = split //, $mask;
            for my $i (0..$#bits) {
                my $bit = $bits[$#bits - $i];
                if ($bit eq '1') {
                    $addressMasked |= (1 << $i);
                } elsif ($bit eq 'X') {
                    push @floating, $i;
                }
            }
            my $count = 1 << scalar(@floating);
            for my $i (0..$count-1) {
                my $modAddress = $addressMasked;
                for my $j (0..$#floating) {
                    my $pos = $floating[$j];
                    if ($i & (1 << $j)) {
                        $modAddress |= (1 << $pos);
                    } else {
                        $modAddress &= ~(1 << $pos);
                    }
                }
                $mem{$modAddress} = $value;
            }
        }
    }
}

my $sum = 0;
foreach my $value (values %mem) {
    $sum += $value;
}

print "$sum\n";
close $fh;