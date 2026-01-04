
use strict;
use warnings;
use Math::BigInt;

my @lines;
my $maxw = 0;
open my $fh, '<', 'input.txt' or die $!;
while (my $line = <$fh>) {
    chomp $line;
    push @lines, $line;
    my $len = length $line;
    $maxw = $len if $len > $maxw;
}
close $fh;

sub is_sep {
    my ($col) = @_;
    for (@lines) {
        return 0 if $col < length $_ && substr($_, $col, 1) !~ /\s/;
    }
    return 1;
}

sub process_block {
    my ($sc, $ec, $grand_ref) = @_;
    my @nums;
    my $op = 0;
    for my $line (@lines) {
        my $seg = substr($line, $sc, $ec - $sc + 1);
        $seg =~ s/^\s+|\s+$//g;
        next unless length $seg;
        if ($seg eq '+') { $op = 1 }
        elsif ($seg eq '*') { $op = 2 }
        else { push @nums, $seg }
    }
    return unless @nums;
    my $acc;
    if ($op == 1) {
        $acc = Math::BigInt->new(0);
        $acc->badd($_) for map { Math::BigInt->new($_) } @nums;
    } elsif ($op == 2) {
        $acc = Math::BigInt->new(1);
        $acc->bmul($_) for map { Math::BigInt->new($_) } @nums;
    } else {
        $acc = Math::BigInt->new($nums[0]);
    }
    $$grand_ref->badd($acc);
}

my $grand = Math::BigInt->new(0);
my $inb = 0;
my $sc;
for my $x (0 .. $maxw - 1) {
    if (!is_sep($x)) {
        unless ($inb) { $inb = 1; $sc = $x }
    } else {
        if ($inb) { process_block($sc, $x - 1, \$grand); $inb = 0 }
    }
}
process_block($sc, $maxw - 1, \$grand) if $inb;

print "Grand total: $grand\n";
