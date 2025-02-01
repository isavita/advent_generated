#!/usr/bin/perl
use strict; use warnings;
open(my $fh, "<", "input.txt") or die $!;
chomp(my $data = <$fh>);
close($fh);
my @prog = split /,/, $data;
$prog[0] = 2;
my $machine = { ip => 0, rel => 0, mem => {} };
$machine->{mem}{$_} = $prog[$_] for 0..$#prog;
my ($xBall, $xPaddle, $score) = (0,0,0);
my @outbuf;
sub get {
    my ($m, $i, $mode) = @_;
    my $v = exists $m->{mem}{$i} ? $m->{mem}{$i} : 0;
    return $mode == 0 ? (exists $m->{mem}{$v} ? $m->{mem}{$v} : 0)
         : $mode == 1 ? $v
         :              (exists $m->{mem}{ $m->{rel} + $v } ? $m->{mem}{ $m->{rel} + $v } : 0);
}
sub setv {
    my ($m, $i, $mode, $val) = @_;
    my $addr = $mode==0 ? $m->{mem}{$i} : $m->{rel} + $m->{mem}{$i};
    $m->{mem}{$addr} = $val;
}
while (1) {
    my $instr = exists($machine->{mem}{$machine->{ip}}) ? $machine->{mem}{$machine->{ip}} : 0;
    my $op = $instr % 100;
    my $m1 = int($instr/100) % 10;
    my $m2 = int($instr/1000) % 10;
    my $m3 = int($instr/10000) % 10;
    if($op==1){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        setv($machine, $machine->{ip}+3, $m3, $a+$b);
        $machine->{ip}+=4;
    } elsif($op==2){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        setv($machine, $machine->{ip}+3, $m3, $a*$b);
        $machine->{ip}+=4;
    } elsif($op==3){
        my $joy = $xPaddle < $xBall ? 1 : $xPaddle > $xBall ? -1 : 0;
        setv($machine, $machine->{ip}+1, $m1, $joy);
        $machine->{ip}+=2;
    } elsif($op==4){
        my $out = get($machine, $machine->{ip}+1, $m1);
        push @outbuf, $out;
        if(@outbuf==3){
            my ($x,$y,$tile) = @outbuf;
            @outbuf = ();
            if($x==-1){ $score = $tile }
            else {
                $xPaddle = $x if $tile==3;
                $xBall   = $x if $tile==4;
            }
        }
        $machine->{ip}+=2;
    } elsif($op==5){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        $machine->{ip} = $a!=0 ? $b : $machine->{ip}+3;
    } elsif($op==6){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        $machine->{ip} = $a==0 ? $b : $machine->{ip}+3;
    } elsif($op==7){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        setv($machine, $machine->{ip}+3, $m3, $a<$b);
        $machine->{ip}+=4;
    } elsif($op==8){
        my $a = get($machine, $machine->{ip}+1, $m1);
        my $b = get($machine, $machine->{ip}+2, $m2);
        setv($machine, $machine->{ip}+3, $m3, $a==$b);
        $machine->{ip}+=4;
    } elsif($op==9){
        my $a = get($machine, $machine->{ip}+1, $m1);
        $machine->{rel} += $a;
        $machine->{ip}+=2;
    } elsif($op==99){
        last;
    } else { die "Bad opcode: $op" }
}
print "$score\n";