use strict;
use warnings;
my $total=0;
open my $fh,'<','input.txt' or die $!;
while(<$fh>){
    chomp;
    s/^\s+|\s+$//g;
    next if $_ eq '';
    my $max=0;
    for my $d1 (reverse 0..9){
        my $pos=index($_,$d1);
        next if $pos==-1||$pos==length($_)-1;
        my $sub=substr($_,$pos+1);
        my $d2=-1;
        while($sub=~/([0-9])/g){
            $d2=$1 if $1>$d2;
            last if $d2==9;
        }
        if($d2!=-1){
            $max=$d1*10+$d2;
            last;
        }
    }
    $total+=$max;
}
print "Total output joltage: $total\n";