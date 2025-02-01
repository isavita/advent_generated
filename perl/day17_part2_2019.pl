#!/usr/bin/env perl
use strict;
use warnings;
use integer;
use List::Util qw(min);

open(my $fh, "<", "input.txt") or die $!;
chomp(my $line = <$fh>);
close($fh);
my @program = split /,/, $line;
sub run_intcode {
    my ($prog_ref, $in_ref) = @_;
    my %mem;
    $mem{$_} = $prog_ref->[$_] for 0..$#$prog_ref;
    my $ip = 0;
    my $rel = 0;
    my @input = @$in_ref;
    my @output;
    my $get = sub {
        my ($off, $mode) = @_;
        if($mode==0){
            my $a = exists $mem{$ip+$off} ? $mem{$ip+$off} : 0;
            return exists $mem{$a} ? $mem{$a} : 0;
        } elsif($mode==1){
            return exists $mem{$ip+$off} ? $mem{$ip+$off} : 0;
        } elsif($mode==2){
            my $a = $rel + (exists $mem{$ip+$off}?$mem{$ip+$off}:0);
            return exists $mem{$a} ? $mem{$a} : 0;
        }
        die "Unknown mode in get";
    };
    my $set = sub {
        my ($off, $mode, $val) = @_;
        if($mode==0){
            my $a = exists $mem{$ip+$off}?$mem{$ip+$off}:0;
            $mem{$a} = $val;
        } elsif($mode==2){
            my $a = $rel + (exists $mem{$ip+$off}?$mem{$ip+$off}:0);
            $mem{$a} = $val;
        } else {
            die "Unknown mode in set";
        }
    };
    while (1) {
        my $code = exists $mem{$ip}?$mem{$ip}:0;
        my $op = $code % 100;
        my $modes = int($code/100);
        my $m0 = $modes % 10;
        my $m1 = int($modes/10) % 10;
        my $m2 = int($modes/100) % 10;
        if($op==1){
            my $a = $get->(1,$m0);
            my $b = $get->(2,$m1);
            $set->(3,$m2, $a+$b);
            $ip += 4;
        } elsif($op==2){
            my $a = $get->(1,$m0);
            my $b = $get->(2,$m1);
            $set->(3,$m2, $a*$b);
            $ip += 4;
        } elsif($op==3){
            my $v = shift @input;
            $set->(1,$m0,$v);
            $ip += 2;
        } elsif($op==4){
            push @output, $get->(1,$m0);
            $ip += 2;
        } elsif($op==5){
            $ip = $get->(1,$m0) != 0 ? $get->(2,$m1) : $ip+3;
        } elsif($op==6){
            $ip = $get->(1,$m0) == 0 ? $get->(2,$m1) : $ip+3;
        } elsif($op==7){
            $set->(3,$m2, $get->(1,$m0) < $get->(2,$m1) ? 1 : 0);
            $ip += 4;
        } elsif($op==8){
            $set->(3,$m2, $get->(1,$m0) == $get->(2,$m1) ? 1 : 0);
            $ip += 4;
        } elsif($op==9){
            $rel += $get->(1,$m0);
            $ip += 2;
        } elsif($op==99){
            last;
        } else { die "Unknown opcode $op at $ip"; }
    }
    return \@output;
}
sub parse_map {
    my ($prog_ref) = @_;
    my $out_ref = run_intcode($prog_ref, []);
    my $s = join('', map { chr($_) } @$out_ref);
    my %scaf;
    my ($robot, $dir);
    my @lines = split /\n/, $s;
    my $y = 0;
    for my $line (@lines) {
        for my $x (0 .. length($line)-1) {
            my $ch = substr($line,$x,1);
            if($ch =~ /^[#^v<>]$/) {
                $scaf{"$x,$y"} = 1;
                if($ch =~ /^[\^v<>]$/) {
                    $robot = [$x, $y];
                    $dir = $ch eq '^' ? 0 : $ch eq '>' ? 1 : $ch eq 'v' ? 2 : 3;
                }
            }
        }
        $y++;
    }
    return (\%scaf, $robot, $dir);
}
sub path {
    my ($scaf, $rob, $d) = @_;
    my @dR = ([0,-1],[1,0],[0,1],[-1,0]);
    my $p = [ $$rob[0], $$rob[1] ];
    my $dir = $d;
    my $dist = 0;
    my $turn;
    my @sections;
    while(1) {
        my ($nx, $ny) = ($p->[0] + $dR[$dir][0], $p->[1] + $dR[$dir][1]);
        if(exists $scaf->{"$nx,$ny"}) {
            $p = [$nx, $ny];
            $dist++;
            next;
        }
        if($dist > 0) {
            push @sections, "$turn,$dist";
        }
        my $rdir = ($dir+1)%4;
        ($nx, $ny) = ($p->[0] + $dR[$rdir][0], $p->[1] + $dR[$rdir][1]);
        if(exists $scaf->{"$nx,$ny"}) { $dir = $rdir; $p = [$nx,$ny]; $dist = 1; $turn = 'R'; next; }
        my $ldir = ($dir+3)%4;
        ($nx, $ny) = ($p->[0] + $dR[$ldir][0], $p->[1] + $dR[$ldir][1]);
        if(exists $scaf->{"$nx,$ny"}) { $dir = $ldir; $p = [$nx,$ny]; $dist = 1; $turn = 'L'; next; }
        last;
    }
    return join(",", @sections);
}
sub encode_path {
    my ($pth) = @_;
    my ($seq, $a, $b, $c);
    for my $i (2..21) {
        for my $j (2..21) {
            for my $k (2..21) {
                my $temp = $pth . ",";
                $a = substr($temp,0,$i);
                (my $t2 = $temp) =~ s/\Q$a\E//g;
                $b = substr($t2,0,$j);
                (my $t3 = $t2) =~ s/\Q$b\E//g;
                $c = substr($t3,0,$k);
                (my $t4 = $t3) =~ s/\Q$c\E//g;
                if($t4 eq ""){
                    goto FOUND;
                }
            }
        }
    }
  FOUND:
    for($a, $b, $c) { s/^,+//; s/,+$//; }
    $seq = $pth;
    $seq =~ s/\Q$a\E/A/g;
    $seq =~ s/\Q$b\E/B/g;
    $seq =~ s/\Q$c\E/C/g;
    $seq =~ s/^,+//; $seq =~ s/,+$//;
    return ($seq, $a, $b, $c);
}
sub dust {
    my ($prog_ref, $scaf, $rob, $d) = @_;
    my $pth = path($scaf, $rob, $d);
    my ($seq, $a, $b, $c) = encode_path($pth);
    my $inp_str = "$seq\n$a\n$b\n$c\nn\n";
    my @inp = map { ord($_) } split //, $inp_str;
    my @prog = @$prog_ref;
    $prog[0] = 2;
    my $out_ref = run_intcode(\@prog, \@inp);
    return $out_ref->[-1];
}
my ($scaf_ref, $robot, $dir) = parse_map(\@program);
print dust(\@program, $scaf_ref, $robot, $dir), "\n";