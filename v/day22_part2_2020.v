
import os

struct Deck {
mut:
	cards []int
}

fn (mut d Deck) push(card int) {
	d.cards << card
}

fn (mut d Deck) pop() int {
	return d.cards.pop()
}

fn (mut d Deck) shift() int {
	x := d.cards[0]
	d.cards = d.cards[1..]
	return x
}

fn (d &Deck) score() i64 {
	mut s := i64(0)
	for i, c in d.cards {
		s += i64(c) * (d.cards.len - i)
	}
	return s
}

fn play(mut p1 Deck, mut p2 Deck) bool {
	mut seen := map[string]bool{}
	for p1.cards.len > 0 && p2.cards.len > 0 {
		key := '${p1.cards}|${p2.cards}'
		if seen[key] {
			return true
		}
		seen[key] = true
		a := p1.shift()
		b := p2.shift()
		mut awin := a > b
		if p1.cards.len >= a && p2.cards.len >= b {
			mut sp1 := Deck{cards: p1.cards[..a].clone()}
			mut sp2 := Deck{cards: p2.cards[..b].clone()}
			awin = play(mut sp1, mut sp2)
		}
		if awin {
			p1.push(a)
			p1.push(b)
		} else {
			p2.push(b)
			p2.push(a)
		}
	}
	return p1.cards.len > 0
}

fn main() {
	txt := os.read_file('input.txt') or { panic(err) }
	lines := txt.split_into_lines().filter(it.len > 0)
	mut p1 := Deck{}
	mut p2 := Deck{}
	mut cur := &p1
	for l in lines {
		if l == 'Player 2:' {
			cur = &p2
			continue
		}
		if l.starts_with('Player') {
			continue
		}
		cur.push(l.int())
	}
	play(mut p1, mut p2)
	mut win := p2
	if p1.cards.len > 0 {
		win = p1
	}
	println(win.score())
}
