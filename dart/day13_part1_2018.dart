
import 'dart:io';

void main() {
  List<List<String>> track = [];
  List<Cart> carts = [];

  List<String> lines = File('input.txt').readAsLinesSync();
  for (int i = 0; i < lines.length; i++) {
    track.add([]);
    for (int j = 0; j < lines[i].length; j++) {
      switch (lines[i][j]) {
        case '>':
          track[i].add('-');
          carts.add(Cart(x: j, y: i, dir: '>'));
          break;
        case '<':
          track[i].add('-');
          carts.add(Cart(x: j, y: i, dir: '<'));
          break;
        case '^':
          track[i].add('|');
          carts.add(Cart(x: j, y: i, dir: '^'));
          break;
        case 'v':
          track[i].add('|');
          carts.add(Cart(x: j, y: i, dir: 'v'));
          break;
        default:
          track[i].add(lines[i][j]);
          break;
      }
    }
  }

  bool collision = false;
  while (!collision) {
    for (int i = 0; i < carts.length; i++) {
      switch (carts[i].dir) {
        case '>':
          carts[i] = movingRight(track, carts[i]);
          break;
        case '<':
          carts[i] = movingLeft(track, carts[i]);
          break;
        case '^':
          carts[i] = movingUp(track, carts[i]);
          break;
        case 'v':
          carts[i] = movingDown(track, carts[i]);
          break;
        default:
          print('error not valid cart');
          break;
      }
    }

    for (int i = 0; i < carts.length; i++) {
      for (int j = i + 1; j < carts.length; j++) {
        if (carts[i].x == carts[j].x && carts[i].y == carts[j].y) {
          collision = true;
          print('${carts[i].x},${carts[i].y}');
        }
      }
    }
  }
}

void printTrack(List<List<String>> track, List<Cart> carts) {
  List<List<String>> h = [];

  for (int i = 0; i < track.length; i++) {
    h.add(List<String>.filled(track[i].length, ''));
    h[i].setAll(0, track[i]);
  }

  for (Cart cart in carts) {
    h[cart.y][cart.x] = cart.dir;
  }

  for (List<String> row in h) {
    for (String s in row) {
      stdout.write(s);
    }
    print('');
  }
}

Cart movingDown(List<List<String>> track, Cart cart) {
  switch (track[cart.y + 1][cart.x]) {
    case '/':
      cart.dir = '<';
      break;
    case '\\':
      cart.dir = '>';
      break;
    case '+':
      if (cart.turn == 0) {
        cart.dir = '>';
        cart.turn = 1;
      } else if (cart.turn == 1) {
        cart.turn = 2;
      } else if (cart.turn == 2) {
        cart.dir = '<';
        cart.turn = 0;
      }
      break;
    case '|':
      break;
    default:
      print('Error on track cart can\'t move : ${cart.x},${cart.y - 1} ${track[cart.y - 1][cart.x]}');
  }
  cart.y = cart.y + 1;
  return cart;
}

Cart movingUp(List<List<String>> track, Cart cart) {
  switch (track[cart.y - 1][cart.x]) {
    case '/':
      cart.dir = '>';
      break;
    case '\\':
      cart.dir = '<';
      break;
    case '+':
      if (cart.turn == 0) {
        cart.dir = '<';
        cart.turn = 1;
      } else if (cart.turn == 1) {
        cart.turn = 2;
      } else if (cart.turn == 2) {
        cart.dir = '>';
        cart.turn = 0;
      }
      break;
    case '|':
      break;
    default:
      print('Error on track cart can\'t move : ${cart.x},${cart.y - 1} ${track[cart.y - 1][cart.x]}');
  }
  cart.y = cart.y - 1;
  return cart;
}

Cart movingLeft(List<List<String>> track, Cart cart) {
  switch (track[cart.y][cart.x - 1]) {
    case '/':
      cart.dir = 'v';
      break;
    case '\\':
      cart.dir = '^';
      break;
    case '+':
      if (cart.turn == 0) {
        cart.dir = 'v';
        cart.turn = 1;
      } else if (cart.turn == 1) {
        cart.turn = 2;
      } else if (cart.turn == 2) {
        cart.dir = '^';
        cart.turn = 0;
      }
      break;
    case '-':
      break;
    default:
      print('Error on track cart can\'t move : ${cart.x - 1},${cart.y} ${track[cart.y][cart.x - 1]}');
  }
  cart.x = cart.x - 1;
  return cart;
}

Cart movingRight(List<List<String>> track, Cart cart) {
  switch (track[cart.y][cart.x + 1]) {
    case '\\':
      cart.dir = 'v';
      break;
    case '/':
      cart.dir = '^';
      break;
    case '+':
      if (cart.turn == 0) {
        cart.dir = '^';
        cart.turn = 1;
      } else if (cart.turn == 1) {
        cart.turn = 2;
      } else if (cart.turn == 2) {
        cart.dir = 'v';
        cart.turn = 0;
      }
      break;
    case '-':
      break;
    default:
      print('Error on track cart can\'t move : ${cart.x + 1},${cart.y} ${track[cart.y][cart.x + 1]}');
  }
  cart.x = cart.x + 1;
  return cart;
}

class Cart {
  int x;
  int y;
  String dir;
  int turn;

  Cart({required this.x, required this.y, required this.dir, this.turn = 0});
}
