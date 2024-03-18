import 'dart:io';
import 'dart:collection';

class Cart {
  int x, y;
  String direction;
  int turns = 0;

  Cart(this.x, this.y, this.direction);
}

List<List<String>> tracks = [];
List<Cart> carts = [];

void main() {
  File('input.txt').readAsLines().then((lines) {
    for (int y = 0; y < lines.length; y++) {
      List<String> trackLine = [];
      for (int x = 0; x < lines[y].length; x++) {
        String c = lines[y][x];
        switch (c) {
          case '>':
          case '<':
          case '^':
          case 'v':
            carts.add(Cart(x, y, c));
            if (c == '>' || c == '<') {
              trackLine.add('-');
            } else {
              trackLine.add('|');
            }
            break;
          default:
            trackLine.add(c);
        }
      }
      tracks.add(trackLine);
    }

    while (carts.length > 1) {
      carts.sort((a, b) => a.y == b.y ? a.x.compareTo(b.x) : a.y.compareTo(b.y));
      Set<Cart> toRemove = {};

      for (var cart in carts) {
        if (!toRemove.contains(cart)) {
          moveCart(cart);
          int? crashIndex = checkCrash(cart);
          if (crashIndex != null) {
            toRemove.add(cart);
            toRemove.add(carts[crashIndex]);
          }
        }
      }

      carts.removeWhere((c) => toRemove.contains(c));
    }

    print('${carts.first.x},${carts.first.y}');
  });
}

void moveCart(Cart cart) {
  switch (cart.direction) {
    case '>':
      cart.x++;
      break;
    case '<':
      cart.x--;
      break;
    case '^':
      cart.y--;
      break;
    case 'v':
      cart.y++;
      break;
  }

  String track = tracks[cart.y][cart.x];
  switch (track) {
    case '+':
      turnCart(cart);
      break;
    case '/':
    case '\\':
      changeDirection(cart, track);
      break;
  }
}

void turnCart(Cart cart) {
  if (cart.turns % 3 == 0) {
    if (cart.direction == '>') {
      cart.direction = '^';
    } else if (cart.direction == '<') {
      cart.direction = 'v';
    } else if (cart.direction == '^') {
      cart.direction = '<';
    } else {
      cart.direction = '>';
    }
  } else if (cart.turns % 3 == 2) {
    if (cart.direction == '>') {
      cart.direction = 'v';
    } else if (cart.direction == '<') {
      cart.direction = '^';
    } else if (cart.direction == '^') {
      cart.direction = '>';
    } else {
      cart.direction = '<';
    }
  }
  cart.turns++;
}

void changeDirection(Cart cart, String track) {
  if (track == '/') {
    if (cart.direction == '>') {
      cart.direction = '^';
    } else if (cart.direction == '<') {
      cart.direction = 'v';
    } else if (cart.direction == '^') {
      cart.direction = '>';
    } else {
      cart.direction = '<';
    }
  } else if (track == '\\') {
    if (cart.direction == '>') {
      cart.direction = 'v';
    } else if (cart.direction == '<') {
      cart.direction = '^';
    } else if (cart.direction == '^') {
      cart.direction = '<';
    } else {
      cart.direction = '>';
    }
  }
}

int? checkCrash(Cart cart) {
  for (int i = 0; i < carts.length; i++) {
    if (carts[i] != cart && carts[i].x == cart.x && carts[i].y == cart.y) {
      return i;
    }
  }
  return null;
}