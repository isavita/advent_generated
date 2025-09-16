#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(first);
use Scalar::Util qw(looks_like_number);

# ---------------- Intcode emulator (step-wise) ----------------

use constant {
  HALTED => 0,
  OUTPUT => 1,
  WAIT   => 2,
};

sub emu_new {
  my ($program) = @_;
  my %mem; @mem{0..$#$program} = @$program; # sparse memory
  return {
    mem   => \%mem,
    ip    => 0,
    rb    => 0,
    input => [],
  };
}
sub memget { my ($emu, $a) = @_; return $emu->{mem}{$a} // 0 }
sub memset { my ($emu, $a, $v) = @_; $emu->{mem}{$a} = $v }
sub emu_write_str {
  my ($emu, $s) = @_;
  push @{$emu->{input}}, map { ord($_) } split //, $s;
}
sub rd {
  my ($emu, $idx, $mode) = @_;
  my $v = memget($emu, $emu->{ip} + $idx);
  return $mode==0 ? memget($emu, $v)
       : $mode==1 ? $v
       : $mode==2 ? memget($emu, $emu->{rb} + $v)
       : die "bad mode $mode";
}
sub wa {
  my ($emu, $idx, $mode) = @_;
  my $v = memget($emu, $emu->{ip} + $idx);
  return $mode==0 ? $v
       : $mode==2 ? $emu->{rb} + $v
       : die "bad write mode $mode";
}
sub emulate {
  my ($emu) = @_;
  while (1) {
    my $op = memget($emu, $emu->{ip});
    my $oc = $op % 100;
    my $m1 = int($op/100)   % 10;
    my $m2 = int($op/1000)  % 10;
    my $m3 = int($op/10000) % 10;

    if ($oc == 1) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); my $c = wa($emu,3,$m3);
      memset($emu,$c,$a+$b); $emu->{ip} += 4;
    } elsif ($oc == 2) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); my $c = wa($emu,3,$m3);
      memset($emu,$c,$a*$b); $emu->{ip} += 4;
    } elsif ($oc == 3) {
      my $a = wa($emu,1,$m1);
      if (@{$emu->{input}}) { memset($emu,$a,shift @{$emu->{input}}); $emu->{ip} += 2; }
      else { return (undef, WAIT) }
    } elsif ($oc == 4) {
      my $a = rd($emu,1,$m1); $emu->{ip} += 2; return ($a, OUTPUT);
    } elsif ($oc == 5) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); $emu->{ip} = ($a!=0) ? $b : ($emu->{ip}+3);
    } elsif ($oc == 6) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); $emu->{ip} = ($a==0) ? $b : ($emu->{ip}+3);
    } elsif ($oc == 7) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); my $c = wa($emu,3,$m3);
      memset($emu,$c, ($a<$b)?1:0 ); $emu->{ip} += 4;
    } elsif ($oc == 8) {
      my $a = rd($emu,1,$m1); my $b = rd($emu,2,$m2); my $c = wa($emu,3,$m3);
      memset($emu,$c, ($a==$b)?1:0 ); $emu->{ip} += 4;
    } elsif ($oc == 9) {
      my $a = rd($emu,1,$m1); $emu->{rb} += $a; $emu->{ip} += 2;
    } elsif ($oc == 99) {
      return (undef, HALTED);
    } else {
      die "bad opcode $oc";
    }
  }
}

# ---------------- Adventure solver ----------------

sub is_bad_item {
  my ($it) = @_;
  return ($it eq 'photons' || $it eq 'escape pod' || $it eq 'molten lava' ||
          $it eq 'infinite loop' || $it eq 'giant electromagnet');
}
sub opposite {
  my ($d) = @_;
  return $d eq 'north' ? 'south'
       : $d eq 'south' ? 'north'
       : $d eq 'west'  ? 'east'
       : $d eq 'east'  ? 'west'
       : '';
}

sub find_path {
  my ($from, $to, $world) = @_;
  my @q = ($from); my %seen = ($from=>1); my %prev;
  while (@q) {
    my $u = shift @q;
    last if $u == $to;
    my $conns = $world->[$u]{connections};
    for my $dir (keys %$conns) {
      my $v = $conns->{$dir};
      next unless defined $v;
      next if $seen{$v}++;
      $prev{$v} = $u;
      push @q, $v;
    }
  }
  return undef unless $seen{$to};
  my @path = ($to);
  my $cur = $to;
  while ($cur != $from) { $cur = $prev{$cur}; push @path, $cur; }
  @path = reverse @path;
  return \@path;
}

sub parse_result_code {
  my ($text) = @_;
  if ($text =~ /You should be able to get in by typing (\d+) /) {
    return $1;
  }
  return undef;
}

# ---------------- Main ----------------

open my $fh, '<', 'input.txt' or die "input.txt: $!";
my $raw = do { local $/; <$fh> }; close $fh;
$raw =~ s/\s+$//;
my @program = map { 0+$_ } split /,/, $raw;

my $emu = emu_new(\@program);

# World graph: array of rooms: { name => ..., connections => { dir => maybe idx } }
my @world;
my %by_name;
my $current_idx;

# Player/inventory and mode
my %inventory; # item => 1/0
use constant { EXPLORE=>0, NAVIGATE=>1, TEST=>2 };
my $mode = EXPLORE;
my @path_stack;                # for DFS/backtracking during explore
my $checkpoint_idx;            # room index
my $test_dir = '';             # direction from checkpoint to floor
my @navigate_path;             # room indices to follow in NAVIGATE

# "last" move context (to wire connections)
my $last_room_idx;
my @last_items;
my $last_dir = '';

# TEST state
my @available_items;           # inventory items to test
my $item_mask = 0;

my $out_buf = '';

sub send_cmd { my ($s) = @_; emu_write_str($emu, $s) }

while (1) {
  my ($val, $st) = emulate($emu);
  if ($st == OUTPUT) {
    $out_buf .= chr($val) if defined $val;
    next;
  }
  if ($st == HALTED) {
    if (my $code = parse_result_code($out_buf)) { print "$code\n"; }
    last;
  }

  # WAIT: parse accumulated text block
  my $text = $out_buf; $out_buf = '';

  if (my $code = parse_result_code($text)) { print "$code\n"; last; }

  my @items_here;
  my @lines = split /\n/, $text;
  for (my $i=0; $i<@lines; ) {
    my $line = $lines[$i]; $line =~ s/\s+$//;
    if ($line eq '' || $line eq 'Command?') { $i++; next; }

    # Room header
    if ($line =~ /^== (.+) ==$/) {
      my $name = $1;
      $i++;
      $i++ while $i<@lines && $lines[$i] !~ /^\s*$/; # skip desc
      my $idx = exists $by_name{$name} ? $by_name{$name} : do {
        my $ni = scalar @world;
        push @world, { name => $name, connections => {} };
        $by_name{$name} = $ni; $ni
      };
      $current_idx = $idx;
      @items_here = ();
      next;
    }

    # Doors
    if ($line eq 'Doors here lead:') {
      $i++;
      while ($i<@lines && $lines[$i] =~ /^\s*-\s*(\w+)/) {
        my $dir = $1;
        $world[$current_idx]{connections}{$dir} //= undef;
        $i++;
      }
      next;
    }

    # Items
    if ($line eq 'Items here:') {
      $i++;
      while ($i<@lines && $lines[$i] =~ /^\s*-\s*(.+)$/) {
        push @items_here, $1;
        $i++;
      }
      next;
    }

    # Take/drop echoes
    if ($line =~ /^You take the (.+)\.$/) {
      my $it = $1; $inventory{$it}=1;
      @last_items = grep { $_ ne $it } @last_items if defined $last_room_idx;
      $i++; next;
    }
    if ($line =~ /^You drop the (.+)\.$/) {
      my $it = $1; $inventory{$it}=0;
      push @last_items, $it if defined $last_room_idx;
      $i++; next;
    }

    # Alert/ejection: learn checkpoint and floor direction
    if ($line =~ /^A loud, robotic voice says "Alert!/) {
      if ($mode == EXPLORE) {
        pop @path_stack if @path_stack;
        if (defined $last_room_idx && defined $current_idx && $last_dir ne '') {
          $checkpoint_idx = $last_room_idx;
          $test_dir = $last_dir;
          $world[$last_room_idx]{connections}{$last_dir} = $current_idx;
          my $rev = opposite($last_dir);
          $world[$current_idx]{connections}{$rev} = $last_room_idx;
        }
      }
      $last_room_idx = undef; @last_items=(); $last_dir='';
      $i++; next;
    }

    $i++;
  }

  # Wire the last movement edge if applicable
  if (defined $last_room_idx && defined $current_idx && $last_dir ne '') {
    if (!defined $world[$last_room_idx]{connections}{$last_dir}) {
      $world[$last_room_idx]{connections}{$last_dir} = $current_idx;
      my $rev = opposite($last_dir);
      $world[$current_idx]{connections}{$rev} = $last_room_idx;
    }
  }
  # Update last context
  $last_room_idx = $current_idx;
  @last_items = @items_here;
  $last_dir = '';

  # Decide next action (send exactly one command)
  if ($mode == EXPLORE) {
    # take one safe item if available
    my $pick = first { !is_bad_item($_) } @items_here;
    if (defined $pick) { send_cmd("take $pick\n"); next; }

    # go to an unexplored door if any
    if (defined $current_idx) {
      my ($dir) = grep { !defined $world[$current_idx]{connections}{$_} } keys %{$world[$current_idx]{connections}};
      if (defined $dir) {
        push @path_stack, $current_idx;
        $last_dir = $dir;
        send_cmd("$dir\n"); next;
      }
    }

    # backtrack if possible
    if (@path_stack) {
      my $target = pop @path_stack;
      # find dir to target
      my ($dir_back) = grep {
        defined $world[$current_idx]{connections}{$_}
          && $world[$current_idx]{connections}{$_} == $target
      } keys %{$world[$current_idx]{connections}};
      if (defined $dir_back) {
        $last_dir = $dir_back;
        send_cmd("$dir_back\n"); next;
      }
    }

    # navigate to checkpoint if known
    if (defined $checkpoint_idx && defined $current_idx && $current_idx != $checkpoint_idx) {
      if (my $p = find_path($current_idx, $checkpoint_idx, \@world)) {
        @navigate_path = @$p; shift @navigate_path; # drop current
        $mode = NAVIGATE; next;
      }
    } elsif (defined $checkpoint_idx && defined $current_idx && $current_idx == $checkpoint_idx) {
      # already there
      $mode = TEST;
      @available_items = sort grep { $inventory{$_} } keys %inventory;
      $item_mask = 0;
    }
    next;
  }

  if ($mode == NAVIGATE) {
    if (@navigate_path) {
      my $next_room = shift @navigate_path;
      # find direction to next_room
      my ($dir) = grep {
        defined $world[$current_idx]{connections}{$_}
          && $world[$current_idx]{connections}{$_} == $next_room
      } keys %{$world[$current_idx]{connections}};
      if (defined $dir) {
        $last_dir = $dir;
        send_cmd("$dir\n"); next;
      } else {
        $mode = EXPLORE; next;
      }
    } else {
      $mode = TEST;
      @available_items = sort grep { $inventory{$_} } keys %inventory;
      $item_mask = 0;
      next;
    }
  }

  if ($mode == TEST) {
    # Ensure inventory matches current mask by changing at most one item
    for (my $i=0; $i<@available_items; $i++) {
      my $it = $available_items[$i];
      my $want = (($item_mask >> $i) & 1) ? 1 : 0;
      my $have = $inventory{$it} ? 1 : 0;
      if ($want != $have) {
        my $action = $want ? 'take' : 'drop';
        send_cmd("$action $it\n"); next;
      }
    }
    # If we reach here, inventory == mask; try the floor
    if ($test_dir ne '') {
      send_cmd("$test_dir\n");
      $item_mask++;
      next;
    } else {
      # discover test_dir by probing any door (should be known already)
      my ($some_dir) = keys %{$world[$current_idx]{connections}};
      $some_dir //= 'north';
      $last_dir = $some_dir;
      send_cmd("$some_dir\n"); next;
    }
  }
}
