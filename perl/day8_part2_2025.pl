#!/usr/bin/perl
use strict;
use warnings;
sub find {
    my ($p,$i)=@_;
    while ($p->[$i]!=$i){
        $p->[$i]=$p->[$p->[$i]];
        $i=$p->[$i];
    }
    $i
}
sub union {
    my ($p,$u,$v)=@_;
    my $ru=find($p,$u);
    my $rv=find($p,$v);
    return 0 if $ru==$rv;
    $p->[$rv]=$ru;
    1
}
sub main{
    open my $fh,'<','input.txt' or die $!;
    my @pts;
    while(<$fh>){
        chomp;
        s/^\s+|\s+$//g;
        next unless $_;
        my ($x,$y,$z)=split /,/;
        push @pts,[$x,$y,$z];
    }
    my $n=@pts;
    return if $n<2;
    my @edges;
    for my $i(0..$n-2){
        my($xi,$yi,$zi)=@{$pts[$i]};
        for my $j($i+1..$n-1){
            my($xj,$yj,$zj)=@{$pts[$j]};
            my $dx=$xi-$xj;
            my $dy=$yi-$yj;
            my $dz=$zi-$zj;
            my $d=$dx*$dx+$dy*$dy+$dz*$dz;
            push @edges,[$d,$i,$j];
        }
    }
    @edges=sort {$a->[0] <=> $b->[0]} @edges;
    my @parent=0..$n-1;
    my $comp=$n;
    for my $e(@edges){
        my($d,$u,$v)=@$e;
        if(union(\@parent,$u,$v)){
            $comp--;
            if($comp==1){
                my($x1,$y1,$z1)=@{$pts[$u]};
                my($x2,$y2,$z2)=@{$pts[$v]};
                print "Connected $x1,$y1,$z1 and $x2,$y2,$z2\n";
                print "Product of X coordinates: ".($x1*$x2)."\n";
                last;
            }
        }
    }
}
main();