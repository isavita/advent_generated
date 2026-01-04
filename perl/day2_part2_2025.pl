
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt lib=>'GMP';

my $data = do { local $/; open my $fh,'<','input.txt' or die $!; <$fh> };
$data =~ s/[\r\n]//g;
my @ranges;
for my $p (split /,/, $data) {
    next unless $p;
    my ($s,$e)=split /-/, $p;
    push @ranges, [Math::BigInt->new($s), Math::BigInt->new($e)];
}
my %found;
for my $r (@ranges) {
    my ($start,$end)=@$r;
    my $slen=length $start->bstr;
    my $elen=length $end->bstr;
    for my $totalLen ($slen..$elen) {
        for my $k (1..int($totalLen/2)) {
            next if $totalLen%$k;
            my $reps=$totalLen/$k;
            my $M=Math::BigInt->new(0);
            for my $i (0..$reps-1) { $M->badd(Math::BigInt->new(10)->bpow($i*$k)) }
            my $minSeed=Math::BigInt->new(10)->bpow($k-1);
            my $maxSeed=Math::BigInt->new(10)->bpow($k); $maxSeed->bsub(1);
            my $targetMin=$start->copy->badd($M)->bsub(1)->bdiv($M);
            my $targetMax=$end->copy->bdiv($M);
            my $seedStart=$targetMin->copy; $seedStart=$minSeed if $seedStart->bcmp($minSeed)<0;
            my $seedEnd=$targetMax->copy;   $seedEnd=$maxSeed   if $seedEnd->bcmp($maxSeed)>0;
            if ($seedStart->bcmp($seedEnd)<=0) {
                my $curr=$seedStart->copy;
                while ($curr->bcmp($seedEnd)<=0) {
                    my $id=$curr->copy->bmul($M);
                    $found{$id->bstr}=1;
                    $curr->binc;
                }
            }
        }
    }
}
my $total=Math::BigInt->new(0);
$total->badd($_) for keys %found;
print "Sum of invalid IDs: $total\n";
