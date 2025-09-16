#!/usr/bin/perl
use strict;
use warnings;

sub is_key { $_[0] ge 'a' && $_[0] le 'z' }
sub is_door { $_[0] ge 'A' && $_[0] le 'Z' }

sub bfs_from_poi {
  my ($grid, $rows, $cols, $start_pos, $pois) = @_;
  my ($sr,$sc) = @$start_pos;
  my %visited_dist;
  my @queue = ( [$sr,$sc,0,0] );
  my $qi = 0;
  my %reachable;
  while ($qi < @queue) {
    my ($r,$c,$dist,$req) = @{$queue[$qi++]};
    my $pos = "$r,$c";
    if (exists $pois->{$pos} && !($r==$sr && $c==$sc)) {
      my $poi = $pois->{$pos};
      my $id = $poi->{id};
      if (!exists $reachable{$id} || $dist < $reachable{$id}->[0]) {
        $reachable{$id} = [$dist, $req];
      }
    }
    foreach my $mv ([0,1],[0,-1],[1,0],[-1,0]) {
      my ($dr,$dc) = @$mv;
      my $nr = $r + $dr;
      my $nc = $c + $dc;
      next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;
      my $cell = $grid->[$nr][$nc];
      next if $cell eq '#';
      my $new_req = $req;
      if (is_door($cell)) {
        $new_req |= (1 << (ord($cell) - ord('A')));
      }
      my $new_dist = $dist + 1;
      my $key = "$nr,$nc";
      if (!exists $visited_dist{$key} || $new_dist < $visited_dist{$key}) {
        $visited_dist{$key} = $new_dist;
        push @queue, [$nr,$nc,$new_dist,$new_req];
      }
    }
  }
  return \%reachable;
}

sub heap_push { my ($heap, $item) = @_; push @$heap, $item; my $i = scalar(@$heap) - 1; while ($i > 0) { my $p = int(($i-1)/2); if ($heap->[$p]->[0] <= $heap->[$i]->[0]) { last; } else { my $tmp = $heap->[$i]; $heap->[$i] = $heap->[$p]; $heap->[$p] = $tmp; $i = $p; } } }

sub heap_pop { my ($heap) = @_; return undef unless @$heap; my $top = $heap->[0]; my $last = pop @$heap; if (@$heap) { $heap->[0] = $last; my $i = 0; my $n = scalar(@$heap); while (1) { my $l = 2*$i+1; my $r = $l+1; last if $l >= $n; my $m = $l; if ($r < $n && $heap->[$r]->[0] < $heap->[$l]->[0]) { $m = $r; } if ($heap->[$i]->[0] <= $heap->[$m]->[0]) { last; } else { my $tmp = $heap->[$i]; $heap->[$i] = $heap->[$m]; $heap->[$m] = $tmp; $i = $m; } } } return $top; }

sub solve_part2 {
  my ($grid) = @_;
  my $rows = scalar @$grid;
  my $cols = scalar @{$grid->[0]};

  my ($sr,$sc);
  for my $r (0..$rows-1) {
    for my $c (0..$cols-1) {
      if ($grid->[$r][$c] eq '@') { $sr=$r; $sc=$c; last; }
    }
    last if defined $sr;
  }

  die "no start" unless defined $sr;

  if ($sr-1 >= 0 && $sc-1 >= 0) { $grid->[$sr-1][$sc-1] = '@'; }
  if ($sr-1 >= 0 && $sc   < $cols) { $grid->[$sr-1][$sc] = '#'; }
  if ($sr-1 >= 0 && $sc+1 < $cols) { $grid->[$sr-1][$sc+1] = '@'; }
  if ($sc-1 >= 0) { $grid->[$sr][$sc-1] = '#'; }
  $grid->[$sr][$sc] = '#';
  if ($sc+1 < $cols) { $grid->[$sr][$sc+1] = '#'; }
  if ($sr+1 < $rows && $sc-1 >= 0) { $grid->[$sr+1][$sc-1] = '@'; }
  if ($sr+1 < $rows) { $grid->[$sr+1][$sc] = '#'; }
  if ($sr+1 < $rows && $sc+1 < $cols) { $grid->[$sr+1][$sc+1] = '@'; }

  my %pois;
  my @start_positions;
  my %keys;
  my $poi_id_counter = 0;

  for my $r (0..$rows-1) {
    for my $c (0..$cols-1) {
      my $cell = $grid->[$r][$c];
      my $pos = "$r,$c";
      if ($cell eq '@') {
        push @start_positions, [$r,$c];
        $pois{$pos} = { id => $poi_id_counter, char => '@', pos => [$r,$c] };
        $poi_id_counter++;
      } elsif (is_key($cell)) {
        $keys{$cell} = [$r,$c];
        $pois{$pos} = { id => $poi_id_counter, char => $cell, pos => [$r,$c] };
        $poi_id_counter++;
      }
    }
  }

  my %id_to_poi;
  foreach my $pos (keys %pois) { $id_to_poi{$pois{$pos}{id}} = $pois{$pos}; }

  my @start_ids;
  foreach my $coord (@start_positions) {
    my $pos = "$coord->[0],$coord->[1]";
    push @start_ids, $pois{$pos}{id};
  }
  @start_ids = sort { $a <=> $b } @start_ids;

  my %adj;
  foreach my $pos (keys %pois) {
    my $p = $pois{$pos};
    $adj{$p->{id}} = bfs_from_poi($grid, $rows, $cols, $p->{pos}, \%pois);
  }

  my $num_keys = scalar(keys %keys);
  my $target_keys_mask = (1 << $num_keys) - 1;

  my $start_state = join(',', @start_ids) . '|0';
  my %dist;
  my @heap;
  $dist{$start_state} = 0;
  heap_push(\@heap, [0, $start_state]);
  my $min_dist = -1;

  while (@heap) {
    my $top = heap_pop(\@heap);
    my ($d, $state) = @$top;
    next if $d != $dist{$state};
    my ($robot_str, $mask_str) = split /\|/, $state;
    my @robot_ids = split /,/, $robot_str;
    my $mask = int($mask_str);

    if ($mask == $target_keys_mask) {
      $min_dist = $d;
      last;
    }

    for my $i (0..$#robot_ids) {
      my $curr = $robot_ids[$i];
      next unless exists $adj{$curr};
      foreach my $to_id (keys %{ $adj{$curr} }) {
        my ($edge_dist, $req_keys) = @{ $adj{$curr}{$to_id} };
        next unless (($req_keys & $mask) == $req_keys);
        my $next_poi = $id_to_poi{$to_id};
        my $char = $next_poi->{char};
        my $mask2 = $mask;
        if (is_key($char)) {
          my $bit = 1 << (ord($char) - ord('a'));
          $mask2 |= $bit;
        }
        my @new_robot_ids = @robot_ids;
        $new_robot_ids[$i] = $to_id;
        my $new_state = join(',', @new_robot_ids) . '|'.$mask2;
        my $new_dist = $d + $edge_dist;
        if (!defined $dist{$new_state} || $new_dist < $dist{$new_state}) {
          $dist{$new_state} = $new_dist;
          heap_push(\@heap, [$new_dist, $new_state]);
        }
      }
    }
  }

  return $min_dist;
}

open my $fh, '<', 'input.txt' or die "input.txt not found";
my @lines = <$fh>;
close $fh;
chomp @lines;
@lines = grep { length($_) > 0 } @lines;

my @grid;
foreach my $line (@lines) { push @grid, [ split //, $line ]; }

my $answer = solve_part2(\@grid);
print "$answer\n";