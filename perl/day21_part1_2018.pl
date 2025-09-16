#!/usr/bin/perl
use strict;
use warnings;

my $filename = 'input.txt';
open my $fh, '<', $filename or die "Error: Unable to open file";

my @lines = <$fh>;
close $fh;
chomp @lines;

my $ip_reg = 0;
my @instructions;
foreach my $line (@lines) {
  if ($line =~ /^\s*#ip\s+(\d+)\s*$/) {
    $ip_reg = int($1);
    next;
  }
  if ($line =~ /^\s*([A-Za-z]+)\s+(\d+)\s+(\d+)\s+(\d+)\s*$/) {
    push @instructions, { name => $1, abc => [ int($2), int($3), int($4) ] };
  }
}

my @regs = (0, 0, 0, 0, 0, 0);

my %ops = (
  addr => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] + $r->[$a->[1]]; },
  addi => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] + $a->[1]; },
  mulr => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] * $r->[$a->[1]]; },
  muli => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] * $a->[1]; },
  banr => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] & $r->[$a->[1]]; },
  bani => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] & $a->[1]; },
  borr => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] | $r->[$a->[1]]; },
  bori => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]] | $a->[1]; },
  setr => sub { my ($r, $a) = @_; $r->[$a->[2]] = $r->[$a->[0]]; },
  seti => sub { my ($r, $a) = @_; $r->[$a->[2]] = $a->[0]; },
  gtir => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($a->[0] > $r->[$a->[1]]) ? 1 : 0; },
  gtri => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($r->[$a->[0]] > $a->[1]) ? 1 : 0; },
  gtrr => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($r->[$a->[0]] > $r->[$a->[1]]) ? 1 : 0; },
  eqir => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($a->[0] == $r->[$a->[1]]) ? 1 : 0; },
  eqri => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($r->[$a->[0]] == $a->[1]) ? 1 : 0; },
  eqrr => sub { my ($r, $a) = @_; $r->[$a->[2]] = ($r->[$a->[0]] == $r->[$a->[1]]) ? 1 : 0; },
);

sub tick {
  my ($regs, $ip_reg, $instructions, $ops) = @_;
  if ($regs->[$ip_reg] >= scalar(@$instructions)) {
    return 1;
  }
  my $instIndex = $regs->[$ip_reg];
  my $inst = $instructions->[$instIndex];
  my $name = $inst->{name};
  my $abc  = $inst->{abc};
  my $f = $ops->{$name};
  $f->($regs, $abc);
  $regs->[$ip_reg]++;

  if ($regs->[$ip_reg] >= scalar(@$instructions)) {
    return 1;
  }
  return 0;
}

while (1) {
  last if tick(\@regs, $ip_reg, \@instructions, \%ops);
  last if $regs[$ip_reg] == 28;
}

print $regs[5], "\n";