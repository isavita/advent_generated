#!/usr/bin/perl
use strict;
use warnings;

sub read_weights {
  my ($filename) = @_;
  open my $fh, '<', $filename or die "Cannot open $filename: $!";
  my @weights;
  while (my $line = <$fh>) {
    chomp $line;
    next if $line eq '';
    push @weights, 0 + $line;
  }
  close $fh;
  return \@weights;
}

sub sum {
  my ($arr_ref) = @_;
  my $total = 0;
  foreach my $v (@$arr_ref) { $total += $v; }
  return $total;
}

sub product {
  my ($arr_ref) = @_;
  my $prod = 1;
  foreach my $v (@$arr_ref) { $prod *= $v; }
  return $prod;
}

sub combinations {
  my ($weights_ref, $target, $group) = @_;
  my @weights = @$weights_ref;
  my @results;
  my @selection;

  my $recurse;
  $recurse = sub {
    my ($start, $remaining, $depth) = @_;
    if ($depth > $group) { return; }
    if ($remaining == 0 && $depth == $group) {
      push @results, [ @selection ];
      return;
    }
    for (my $i = $start; $i < scalar @weights; $i++) {
      my $w = $weights[$i];
      if ($w <= $remaining) {
        push @selection, $w;
        $recurse->($i + 1, $remaining - $w, $depth + 1);
        pop @selection;
      }
    }
  };
  $recurse->(0, $target, 0);
  return \@results;
}

sub find_optimal_combination {
  my ($weights_ref, $groups) = @_;
  my @weights = @$weights_ref;
  @weights = sort { $b <=> $a } @weights; # descending
  my $total = sum(\@weights);
  return undef if $total % $groups != 0;
  my $target = int($total / $groups);

  my $min_length = scalar @weights;
  my $min_qe = 1e308;
  my $best_combination;

  for (my $group_size = 1; $group_size <= scalar @weights; $group_size++) {
    my $combs_ref = combinations(\@weights, $target, $group_size);
    foreach my $comb (@$combs_ref) {
      my $qe = product($comb);
      my $len = scalar @$comb;
      if (!defined $best_combination || $len < $min_length || ($len == $min_length && $qe < $min_qe)) {
        $min_length = $len;
        $min_qe = $qe;
        $best_combination = $comb;
      }
    }
    if (defined $best_combination) { last; }
  }
  return ($best_combination, $min_qe);
}

sub main {
  my $weights = read_weights('input.txt');

  my ($best_group_3, $qe_3) = find_optimal_combination($weights, 3);
  if (defined $best_group_3) {
    print "Part 1: Quantum Entanglement (3 groups) = $qe_3\n";
  } else {
    print "No valid combination found for 3 groups.\n";
  }

  my ($best_group_4, $qe_4) = find_optimal_combination($weights, 4);
  if (defined $best_group_4) {
    print "Part 2: Quantum Entanglement (4 groups) = $qe_4\n";
  } else {
    print "No valid combination found for 4 groups.\n";
  }
}

main();