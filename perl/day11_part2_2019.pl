#!/usr/bin/perl
use strict;
use warnings;
use feature 'say';
use File::Slurp;

my %mem;
my $ip = 0;
my $rel = 0;

my ($data) = read_file("input.txt") =~ /(.*)/s;
chomp($data);
my @nums = split /,/, $data;
$mem{$_} = $nums[$_] for 0 .. $#nums;

sub read_val {
    my ($off, $mode) = @_;
    my $val = $mem{$ip+$off} // 0;
    return $mode == 0 ? ($mem{$val} // 0) : $mode == 1 ? $val : $mode == 2 ? ($mem{$rel + $val} // 0) : die "bad mode $mode";
}

sub write_addr {
    my ($off, $mode) = @_;
    my $val = $mem{$ip+$off} // 0;
    return $mode == 0 ? $val : $mode == 2 ? $rel + $val : die "bad mode for writing $mode";
}

sub run_vm_iteration {
    my ($input_val) = @_;
    my @outs;
    while (1) {
        my $instr = $mem{$ip} // 0;
        my $op = $instr % 100;
        my $modes = int($instr/100);
        if ($op == 1) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10; $modes = int($modes/10);
            my $m3 = $modes % 10;
            my $a = read_val(1,$m1);
            my $b = read_val(2,$m2);
            my $dest = write_addr(3,$m3);
            $mem{$dest} = $a + $b;
            $ip += 4;
        } elsif ($op == 2) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10; $modes = int($modes/10);
            my $m3 = $modes % 10;
            my $a = read_val(1,$m1);
            my $b = read_val(2,$m2);
            my $dest = write_addr(3,$m3);
            $mem{$dest} = $a * $b;
            $ip += 4;
        } elsif ($op == 3) {
            my $m = $modes % 10;
            my $dest = write_addr(1,$m);
            $mem{$dest} = $input_val;
            $ip += 2;
        } elsif ($op == 4) {
            my $m = $modes % 10;
            my $out = read_val(1,$m);
            $ip += 2;
            push @outs, $out;
            return \@outs if @outs == 2;
        } elsif ($op == 5) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10;
            my $a = read_val(1,$m1);
            if ($a != 0) {
                $ip = read_val(2,$m2);
            } else {
                $ip += 3;
            }
        } elsif ($op == 6) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10;
            my $a = read_val(1,$m1);
            if ($a == 0) {
                $ip = read_val(2,$m2);
            } else {
                $ip += 3;
            }
        } elsif ($op == 7) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10; $modes = int($modes/10);
            my $m3 = $modes % 10;
            my $a = read_val(1,$m1);
            my $b = read_val(2,$m2);
            my $dest = write_addr(3,$m3);
            $mem{$dest} = ($a < $b) ? 1 : 0;
            $ip += 4;
        } elsif ($op == 8) {
            my $m1 = $modes % 10; $modes = int($modes/10);
            my $m2 = $modes % 10; $modes = int($modes/10);
            my $m3 = $modes % 10;
            my $a = read_val(1,$m1);
            my $b = read_val(2,$m2);
            my $dest = write_addr(3,$m3);
            $mem{$dest} = ($a == $b) ? 1 : 0;
            $ip += 4;
        } elsif ($op == 9) {
            my $m = $modes % 10;
            $rel += read_val(1,$m);
            $ip += 2;
        } elsif ($op == 99) {
            return undef;
        } else {
            die "Unknown opcode $op at $ip\n";
        }
    }
}

my %grid;
$grid{"0,0"} = 1;
my ($x, $y, $dir) = (0, 0, 0);
while (1) {
    my $col = $grid{"$x,$y"} // 0;
    my $outs = run_vm_iteration($col);
    last unless defined $outs;
    my ($newcol, $turn) = @$outs;
    $grid{"$x,$y"} = $newcol;
    $dir = ($dir + ($turn ? 1 : 3)) % 4;
    if($dir==0){ $y++; }
    elsif($dir==1){ $x++; }
    elsif($dir==2){ $y--; }
    elsif($dir==3){ $x--; }
}
for my $j (reverse  -5..1) {
    my $line = "";
    for my $i (0..40) {
        $line .= ($grid{"$i,$j"} // 0) ? "█" : " ";
    }
    say $line;
}