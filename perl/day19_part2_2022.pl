#!/usr/bin/perl
use strict; use warnings;
open(my $fh, "<", "input.txt") or die $!;
my @blueprints;
while(<$fh>){
	chomp;
	if(/^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian\./){
		push @blueprints, {
			id               => $1+0,
			oreCost          => $2+0,
			clayOreCost      => $3+0,
			obsidianOreCost  => $4+0,
			obsidianClayCost => $5+0,
			geodeOreCost     => $6+0,
			geodeObsidianCost=> $7+0
		};
	}
}
close $fh;
sub max { my $m = shift; for (@_) { $m = $_ if $_>$m } return $m }
sub max_geode {
	my ($b, $init) = @_;
	my $max = 0;
	my @q = ($init);
	my %vis;
	while(@q){
		my $s = shift @q;
		$max = $s->{geode} if $s->{geode} > $max;
		next if $s->{timeLeft} == 0;
		my $o = max($b->{oreCost}, $b->{clayOreCost}, $b->{obsidianOreCost}, $b->{geodeOreCost});
		$s->{oreRobots} = $o if $s->{oreRobots} > $o;
		$s->{clayRobots} = $b->{obsidianClayCost} if $s->{clayRobots} > $b->{obsidianClayCost};
		$s->{obsidianRobots} = $b->{geodeObsidianCost} if $s->{obsidianRobots} > $b->{geodeObsidianCost};
		my $maxOre = $s->{timeLeft} * $o - $s->{oreRobots} * ($s->{timeLeft}-1);
		$s->{ore} = $maxOre if $s->{ore} > $maxOre;
		my $maxClay = $s->{timeLeft} * $b->{obsidianClayCost} - $s->{clayRobots} * ($s->{timeLeft}-1);
		$s->{clay} = $maxClay if $s->{clay} > $maxClay;
		my $maxObsidian = $s->{timeLeft} * $b->{geodeObsidianCost} - $s->{obsidianRobots} * ($s->{timeLeft}-1);
		$s->{obsidian} = $maxObsidian if $s->{obsidian} > $maxObsidian;
		my $key = join ",", $s->{ore},$s->{clay},$s->{obsidian},$s->{geode},$s->{oreRobots},$s->{clayRobots},$s->{obsidianRobots},$s->{geodeRobots},$s->{timeLeft};
		next if $vis{$key}++;
		my $nt = $s->{timeLeft}-1;
		push @q, {
			ore           => $s->{ore} + $s->{oreRobots},
			clay          => $s->{clay} + $s->{clayRobots},
			obsidian      => $s->{obsidian} + $s->{obsidianRobots},
			geode         => $s->{geode} + $s->{geodeRobots},
			oreRobots     => $s->{oreRobots},
			clayRobots    => $s->{clayRobots},
			obsidianRobots=> $s->{obsidianRobots},
			geodeRobots   => $s->{geodeRobots},
			timeLeft      => $nt,
		};
		if($s->{ore} >= $b->{oreCost}){
			push @q, {
				ore           => $s->{ore} - $b->{oreCost} + $s->{oreRobots},
				clay          => $s->{clay} + $s->{clayRobots},
				obsidian      => $s->{obsidian} + $s->{obsidianRobots},
				geode         => $s->{geode} + $s->{geodeRobots},
				oreRobots     => $s->{oreRobots}+1,
				clayRobots    => $s->{clayRobots},
				obsidianRobots=> $s->{obsidianRobots},
				geodeRobots   => $s->{geodeRobots},
				timeLeft      => $nt,
			};
		}
		if($s->{ore} >= $b->{clayOreCost}){
			push @q, {
				ore           => $s->{ore} - $b->{clayOreCost} + $s->{oreRobots},
				clay          => $s->{clay} + $s->{clayRobots},
				obsidian      => $s->{obsidian} + $s->{obsidianRobots},
				geode         => $s->{geode} + $s->{geodeRobots},
				oreRobots     => $s->{oreRobots},
				clayRobots    => $s->{clayRobots}+1,
				obsidianRobots=> $s->{obsidianRobots},
				geodeRobots   => $s->{geodeRobots},
				timeLeft      => $nt,
			};
		}
		if($s->{ore} >= $b->{obsidianOreCost} && $s->{clay} >= $b->{obsidianClayCost}){
			push @q, {
				ore           => $s->{ore} - $b->{obsidianOreCost} + $s->{oreRobots},
				clay          => $s->{clay} - $b->{obsidianClayCost} + $s->{clayRobots},
				obsidian      => $s->{obsidian} + $s->{obsidianRobots},
				geode         => $s->{geode} + $s->{geodeRobots},
				oreRobots     => $s->{oreRobots},
				clayRobots    => $s->{clayRobots},
				obsidianRobots=> $s->{obsidianRobots}+1,
				geodeRobots   => $s->{geodeRobots},
				timeLeft      => $nt,
			};
		}
		if($s->{ore} >= $b->{geodeOreCost} && $s->{obsidian} >= $b->{geodeObsidianCost}){
			push @q, {
				ore           => $s->{ore} - $b->{geodeOreCost} + $s->{oreRobots},
				clay          => $s->{clay} + $s->{clayRobots},
				obsidian      => $s->{obsidian} - $b->{geodeObsidianCost} + $s->{obsidianRobots},
				geode         => $s->{geode} + $s->{geodeRobots},
				oreRobots     => $s->{oreRobots},
				clayRobots    => $s->{clayRobots},
				obsidianRobots=> $s->{obsidianRobots},
				geodeRobots   => $s->{geodeRobots}+1,
				timeLeft      => $nt,
			};
		}
	}
	return $max;
}
my $prod = 1;
my $init = { ore=>0, clay=>0, obsidian=>0, geode=>0, oreRobots=>1, clayRobots=>0, obsidianRobots=>0, geodeRobots=>0, timeLeft=>32 };
for my $i (0..2){
	$prod *= max_geode($blueprints[$i], $init);
}
print "$prod\n";