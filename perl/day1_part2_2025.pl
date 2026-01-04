#!/usr/bin/perl
use strict;
use warnings;

sub floorDiv {
    my ($a,$b)=@_;
    my $q=int($a/$b);
    $q-- if $a<0 && $a%$b!=0;
    return $q;
}

my $file='input.txt';
open my $fh,'<',$file or die $!;
my $currentPos=50;
my $totalZeroHits=0;
my $dialSize=100;

while (my $line=<$fh>) {
    chomp $line;
    $line=~s/^\s+|\s+$//g;
    next unless $line;
    my $dir=substr($line,0,1);
    my $amt=substr($line,1);
    $amt+=0;
    if ($dir eq 'R') {
        my $hits=int(($currentPos+$amt)/$dialSize);
        $totalZeroHits+=$hits;
        $currentPos=($currentPos+$amt)%$dialSize;
    } elsif ($dir eq 'L') {
        my $hits=floorDiv($currentPos-1,$dialSize)-floorDiv($currentPos-$amt-1,$dialSize);
        $totalZeroHits+=$hits;
        $currentPos=($currentPos-$amt)%$dialSize;
        $currentPos+=$dialSize if $currentPos<0;
    } else {
        die "Unknown direction '$dir' in line '$line'\n";
    }
}
close $fh;
print "The password is: $totalZeroHits\n";