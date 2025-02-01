#!/usr/bin/perl
use strict;
use warnings;
open(my $fh, "<", "input.txt") or die $!;
chomp(my $line = <$fh>);
close($fh);
my @program = split /,/, $line;
sub run_vm {
    my ($mem_ref, $inputs_ref) = @_;
    my @mem = @$mem_ref;
    my @inputs = @$inputs_ref;
    my @outputs;
    my $ip = 0;
    while (1) {
        my $instr = $mem[$ip];
        my $opcode = $instr % 100;
        my $m1 = int($instr/100) % 10;
        my $m2 = int($instr/1000) % 10;
        if($opcode==1){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $mem[$mem[$ip+3]] = $a+$b;
            $ip += 4;
        } elsif($opcode==2){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $mem[$mem[$ip+3]] = $a*$b;
            $ip += 4;
        } elsif($opcode==3){
            $mem[$mem[$ip+1]] = shift(@inputs);
            $ip += 2;
        } elsif($opcode==4){
            push @outputs, $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            $ip += 2;
        } elsif($opcode==5){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $ip = $a!=0 ? $b : $ip+3;
        } elsif($opcode==6){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $ip = $a==0 ? $b : $ip+3;
        } elsif($opcode==7){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $mem[$mem[$ip+3]] = $a<$b ? 1 : 0;
            $ip += 4;
        } elsif($opcode==8){
            my $a = $m1 ? $mem[$ip+1] : $mem[ $mem[$ip+1] ];
            my $b = $m2 ? $mem[$ip+2] : $mem[ $mem[$ip+2] ];
            $mem[$mem[$ip+3]] = $a==$b ? 1 : 0;
            $ip += 4;
        } elsif($opcode==99){
            last;
        } else {
            die "Unknown opcode: $opcode at $ip";
        }
    }
    return \@outputs;
}
sub permute {
    my ($arr, $l, $r, $res) = @_;
    if($l==$r){
        push @$res, [@$arr];
    } else {
        for my $i ($l..$r){
            @$arr[$l,$i] = @$arr[$i,$l];
            permute($arr, $l+1, $r, $res);
            @$arr[$l,$i] = @$arr[$i,$l];
        }
    }
}
my @phases = (0,1,2,3,4);
my @perms;
permute(\@phases, 0, $#phases, \@perms);
my $max = 0;
for my $phase (@perms) {
    my $signal = 0;
    for my $p (@$phase) {
        my $out_ref = run_vm(\@program, [$p, $signal]);
        $signal = $out_ref->[-1];
    }
    $max = $signal if $signal > $max;
}
print "$max\n";