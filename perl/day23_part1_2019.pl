#!/usr/bin/perl
use strict;
use warnings;
open my $fh, "<", "input.txt" or die $!;
chomp(my $data = <$fh>);
close $fh;
my $program = [ split /,/, $data ];
sub init_vm {
    my ($id) = @_;
    return { mem => [ @$program ], ip => 0, rb => 0, input => [ $id, -1 ], output => [], halted => 0 };
}
sub run_vm {
    my ($vm, $steps) = @_;
    my $m = $vm->{mem};
    my $ip = \$vm->{ip};
    while($steps-- > 0 and not $vm->{halted}) {
        my $ins = $m->[ $$ip ] // 0;
        my $op = $ins % 100;
        my $m1 = int($ins/100) % 10;
        my $m2 = int($ins/1000) % 10;
        my $m3 = int($ins/10000) % 10;
        if($op==1) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $m->[ get_addr($vm, $$ip+3, $m3) ] = $a + $b;
            $$ip += 4;
        } elsif($op==2) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $m->[ get_addr($vm, $$ip+3, $m3) ] = $a * $b;
            $$ip += 4;
        } elsif($op==3) {
            my $addr = get_addr($vm, $$ip+1, $m1);
            my $in = @{ $vm->{input} } ? shift(@{ $vm->{input} }) : -1;
            $m->[$addr] = $in;
            $$ip += 2;
        } elsif($op==4) {
            push @{ $vm->{output} }, get_val($vm, $$ip+1, $m1);
            $$ip += 2;
            return;
        } elsif($op==5) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $$ip = $a ? $b : $$ip+3;
        } elsif($op==6) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $$ip = ($a==0) ? $b : $$ip+3;
        } elsif($op==7) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $m->[ get_addr($vm, $$ip+3, $m3) ] = ($a < $b) ? 1 : 0;
            $$ip += 4;
        } elsif($op==8) {
            my $a = get_val($vm, $$ip+1, $m1);
            my $b = get_val($vm, $$ip+2, $m2);
            $m->[ get_addr($vm, $$ip+3, $m3) ] = ($a == $b) ? 1 : 0;
            $$ip += 4;
        } elsif($op==9) {
            $vm->{rb} += get_val($vm, $$ip+1, $m1);
            $$ip += 2;
        } elsif($op==99) {
            $vm->{halted} = 1;
            last;
        } else {
            die "bad op $op";
        }
    }
}
sub get_val {
    my ($vm, $i, $mode) = @_;
    my $m = $vm->{mem};
    return $mode==0 ? ($m->[ $m->[$i] // 0 ] // 0)
         : $mode==1 ? ($m->[$i] // 0)
         : $mode==2 ? ($m->[ $vm->{rb} + ($m->[$i] // 0) ] // 0)
         : die "bad mode $mode";
}
sub get_addr {
    my ($vm, $i, $mode) = @_;
    my $m = $vm->{mem};
    return $mode==0 ? ($m->[$i] // 0)
         : $mode==2 ? $vm->{rb} + ($m->[$i] // 0)
         : die "bad mode for addr $mode";
}
my @vms = map { init_vm($_) } 0..49;
while(1) {
    for my $vm (@vms) {
        run_vm($vm, 1000);
        while(@{ $vm->{output} } >= 3) {
            my $d = shift @{ $vm->{output} };
            my $x = shift @{ $vm->{output} };
            my $y = shift @{ $vm->{output} };
            if($d==255) { print "$y\n"; exit }
            push @{ $vms[$d]{input} }, $x, $y;
        }
    }
}