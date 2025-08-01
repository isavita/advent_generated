#!/usr/bin/env bash
set -euo pipefail

readfile() { perl -0777 -ne 'print' input.txt; }

scale_up() {
  perl -0777 -ne '
    my $s=$_; my($map,$moves)=split(/\n\n/,$s,2);
    $map =~ s/#/##/g; $map =~ s/\./../g; $map =~ s/O/[]/g; $map =~ s/@/@./g;
    print $map,"\n\n",$moves;
  ' <<<"$1"
}

solve() {
  perl -0777 -ne '
    use integer;
    my $s=$_; my($map,$moves)=split(/\n\n/,$s,2);
    my @g=map{[split //]} split /\n/,$map; my $h=@g; my $w=0; $w=@$_>$w?@$_:$w for @g;
    for(@g){ $_->[$w-1]//=q{ } for 0..$w-1 }
    my($rx,$ry)=(0,0);
    for my $y (0..$h-1){ for my $x (0..$w-1){ if($g[$y][$x] eq "@"){($rx,$ry)=($x,$y)}}}
    my %d = ( "^"=>[0,-1], "v"=>[0,1], "<"=>[-1,0], ">"=>[1,0] );

    sub inb { my($x,$y,$w,$h)=@_; return $x>=0 && $x<$w && $y>=0 && $y<$h }
    sub deep_copy {
      my($g)=@_; my @n; for my $r (@$g){ push @n, [@$r] } return \@n
    }
    sub try_step {
      my($g,$w,$h,$x,$y,$dx,$dy)=@_;
      return 0 if !inb($x,$y,$w,$h);
      my $orig = deep_copy($g);
      my $ch = $g->[$y][$x];
      my ($nx,$ny)=($x+$dx,$y+$dy);
      return 0 if !inb($nx,$ny,$w,$h);
      my $ok=0;
      if($ch eq "."){ $ok=1 }
      elsif($ch eq "#"){ $ok=0 }
      elsif($ch eq "O" || $ch eq "@"){
        if(try_step($g,$w,$h,$nx,$ny,$dx,$dy)){
          $g->[$ny][$nx]=$ch; $g->[$y][$x]="."; $ok=1
        }
      } elsif($ch eq "["){
        my($rx2,$ry2)=($x+1,$y);
        if($rx2<$w && $g->[$ry2][$rx2] eq "]"){
          if($dx==-1 && $dy==0){
            my($lx,$ly)=($x-1,$y);
            if(try_step($g,$w,$h,$lx,$ly,$dx,$dy)){
              $g->[$ly][$lx] = "["; $g->[$y][$x] = "]"; $g->[$ry2][$rx2]="."; $ok=1
            }
          } elsif($dx==1 && $dy==0){
            my($fx,$fy)=($rx2+1,$ry2);
            if(try_step($g,$w,$h,$fx,$fy,$dx,$dy)){
              $g->[$fy][$fx] = "]"; $g->[$ry2][$rx2] = "["; $g->[$y][$x]="."; $ok=1
            }
          } else {
            my($lx,$ly)=($x+$dx,$y+$dy); my($rx3,$ry3)=($rx2+$dx,$ry2+$dy);
            if(try_step($g,$w,$h,$lx,$ly,$dx,$dy) && try_step($g,$w,$h,$rx3,$ry3,$dx,$dy)){
              $g->[$ly][$lx]="["; $g->[$ry3][$rx3]="]"; $g->[$y][$x]="."; $g->[$ry2][$rx2]="."; $ok=1
            }
          }
        } else { $ok=0 }
      } elsif($ch eq "]"){
        my($lx,$ly)=($x-1,$y);
        if($lx>=0 && $g->[$ly][$lx] eq "["){
          if(try_step($g,$w,$h,$lx,$ly,$dx,$dy)){ $ok=1 } else { $ok=0 }
        } else { $ok=0 }
      }
      if(!$ok){ for my $y2 (0..$h-1){ $g->[$y2]=[ @{$orig->[$y2]} ] } }
      return $ok
    }

    $moves =~ s/\s+//g;
    for my $c (split //,$moves){
      my($dx,$dy)=@{$d{$c}}; my($nx,$ny)=($rx+$dx,$ry+$dy);
      next if !inb($nx,$ny,$w,$h);
      my $t = $g[$ny][$nx];
      next if $t eq "#";
      my $gg = deep_copy(\@g);
      if(try_step($gg,$w,$h,$nx,$ny,$dx,$dy)){
        $gg->[$ny][$nx]="@"; $gg->[$ry][$rx]=".";
        @g = map { [@$_] } @$gg; ($rx,$ry)=($nx,$ny);
      }
    }
    my $score=0;
    for my $y (0..$h-1){ for my $x (0..$w-1){ my $c=$g[$y][$x]; if($c eq "[" || $c eq "O"){ $score += $x + 100*$y } } }
    print $score,"\n";
  ' <<<"$1"
}

orig="$(readfile)"
solve "$orig"
scaled="$(scale_up "$orig")"
solve "$scaled"