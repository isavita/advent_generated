
#!/usr/bin/perl
use strict;
use warnings;

my $line = do {
    open my $fh, '<', 'input.txt' or die "Could not open input.txt: $!";
    local $/;
    <$fh>;
};
chomp $line;
my @stones = split ' ', $line;

for (1..25) {
    my @next;
    for my $s (@stones) {
        if ($s eq '0') {
            push @next, '1';
        } elsif (length($s) % 2 == 0) {
            my $mid = length($s) / 2;
            my $left = substr($s, 0, $mid);
            my $right = substr($s, $mid);
            $left =~ s/^0+//;
            $right =~ s/^0+//;
            push @next, ($left eq '' ? '0' : $left), ($right eq '' ? '0' : $right);
        } else {
            push @next, $s * 2024;
        }
    }
    @stones = @next;
}

print scalar @stones, "\n";
