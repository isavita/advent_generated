
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt lib => 'GMP';

my $data = do {
    open my $fh, '<', 'input.txt' or die $!;
    local $/;
    <$fh>;
};
$data =~ s/[\r\n\s]//g;
my @parts = grep { $_ ne '' } split /,/, $data;

my @ranges;
for my $p (@parts) {
    my ($s,$e) = split /-/, $p, 2;
    push @ranges, [ Math::BigInt->new($s), Math::BigInt->new($e) ];
}

my %found;
my %mult;
for my $k (1..20) {
    my $pow = Math::BigInt->new(10)->bpow($k);
    $mult{$k} = $pow->copy->badd(1);               # 10^k + 1
    $mult{$k.'_min'} = Math::BigInt->new(10)->bpow($k-1);   # 10^(k-1)
    $mult{$k.'_max'} = $pow->bsub(1);                         # 10^k - 1
}

for my $r (@ranges) {
    my ($start,$end) = @$r;
    my $maxk = int( length($end->bstr) / 2 );
    for my $k (1..$maxk) {
        my $M   = $mult{$k};
        my $minS = $mult{$k.'_min'};
        my $maxS = $mult{$k.'_max'};

        my $sMin = $start->copy->badd($M)->bsub(1)->bdiv($M);
        my $sMax = $end->copy->bdiv($M);

        $sMin = $minS if $sMin->bacmp($minS) < 0;
        $sMax = $maxS if $sMax->bacmp($maxS) > 0;
        next if $sMin->bacmp($sMax) > 0;

        for (my $seed = $sMin->copy; $seed->bacmp($sMax) <= 0; $seed->binc) {
            my $id = $seed->copy->bmul($M);
            $found{$id->bstr} = 1;
        }
    }
}

my $sum = Math::BigInt->new(0);
$sum->badd($_) for keys %found;
print "Sum of invalid IDs: $sum\n";
