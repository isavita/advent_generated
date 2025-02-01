#!/usr/bin/perl
use strict;
use warnings;
use Fcntl;
local $/ = undef;
open(my $fh, "<", "input.txt") or die $!;
my $data = <$fh>;
close $fh;
chomp $data;
my @mem = map { int($_) } split /,/, $data;
my @input;
for my $line ("NOT A J","NOT B T","OR T J","NOT C T","OR T J","AND D J","WALK") {
    push @input, map { ord } split //, $line;
    push @input, 10;
}
sub get { my $i = shift; return defined($mem[$i]) ? $mem[$i] : 0 }
sub addr {
    my ($p,$m,$rb) = @_;
    return $m==0 ? get($p) : $m==1 ? $p : $rb + get($p);
}
my ($ip, $rb) = (0,0);
while (1) {
    my $instr = get($ip);
    my $op = $instr % 100;
    my $m1 = int($instr/100)   % 10;
    my $m2 = int($instr/1000)  % 10;
    my $m3 = int($instr/10000) % 10;
    if($op==1){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $mem[addr($ip+3,$m3,$rb)] = $a + $b;
        $ip += 4;
    } elsif($op==2){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $mem[addr($ip+3,$m3,$rb)] = $a * $b;
        $ip += 4;
    } elsif($op==3){
        die "No input" unless @input;
        $mem[addr($ip+1,$m1,$rb)] = shift @input;
        $ip += 2;
    } elsif($op==4){
        my $out = get(addr($ip+1,$m1,$rb));
        if($out > 127) { print "$out\n"; exit }
        else { print chr($out) }
        $ip += 2;
    } elsif($op==5){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $ip = $a!=0 ? $b : $ip+3;
    } elsif($op==6){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $ip = $a==0 ? $b : $ip+3;
    } elsif($op==7){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $mem[addr($ip+3,$m3,$rb)] = $a < $b ? 1 : 0;
        $ip += 4;
    } elsif($op==8){
        my $a = get(addr($ip+1,$m1,$rb));
        my $b = get(addr($ip+2,$m2,$rb));
        $mem[addr($ip+3,$m3,$rb)] = $a == $b ? 1 : 0;
        $ip += 4;
    } elsif($op==9){
        $rb += get(addr($ip+1,$m1,$rb));
        $ip += 2;
    } elsif($op==99){
        last;
    } else {
        die "Unknown opcode: $op at position $ip\n";
    }
}