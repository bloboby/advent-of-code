use std::collections::HashMap;

const UP: i32 = 0;
const RIGHT: i32 = 1;
const DOWN: i32 = 2;
const LEFT: i32 = 3;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Cart {
  yx: (i32, i32),
  dir: i32,
  next_turn: i32,
}

fn preprocess_cart(cart: char) -> Option<(i32, char)> {
  match cart {
    '^' => Some((UP, '|')),
    'v' => Some((DOWN, '|')),
    '<' => Some((LEFT, '-')),
    '>' => Some((RIGHT, '-')),
    _ => None,
  }
}

fn preprocess(contents: &String) -> (Vec<Vec<char>>, Vec<Cart>) {
  let mut grid = vec![];
  let mut carts = vec![];

  let mut y = 0;
  for line in contents.lines() {
    let mut row = vec![];
    let mut x = 0;
    for c in line.chars() {
      match preprocess_cart(c) {
        Some((dir, track)) => {
          carts.push(Cart {
            yx: (y, x),
            dir: dir,
            next_turn: -1,
          });
          row.push(track);
        }
        None => {
          row.push(c);
        }
      }
      x += 1;
    }
    grid.push(row);
    y += 1;
  }

  (grid, carts)
}

fn make_cart(cart: &Cart, dir: i32) -> Cart {
  let (y, x) = cart.yx;
  let new_yx = match dir {
    UP => (y - 1, x),
    RIGHT => (y, x + 1),
    DOWN => (y + 1, x),
    LEFT => (y, x - 1),
    _ => unreachable!(),
  };

  Cart {
    yx: new_yx,
    dir: dir,
    next_turn: cart.next_turn,
  }
}

fn rerange(n: i32, min: i32, max: i32) -> i32 {
  let d = max - min;
  if n < min {
    n + d
  } else if n >= max {
    n - d
  } else {
    n
  }
}

fn move_cart(cart: Cart, grid: &Vec<Vec<char>>) -> Cart {
  let y = cart.yx.0 as usize;
  let x = cart.yx.1 as usize;
  match (grid[y][x], cart.dir) {
    ('|', _) | ('-', _) => make_cart(&cart, cart.dir),
    ('/', UP) => make_cart(&cart, RIGHT),
    ('/', RIGHT) => make_cart(&cart, UP),
    ('/', DOWN) => make_cart(&cart, LEFT),
    ('/', LEFT) => make_cart(&cart, DOWN),
    ('\\', UP) => make_cart(&cart, LEFT),
    ('\\', RIGHT) => make_cart(&cart, DOWN),
    ('\\', DOWN) => make_cart(&cart, RIGHT),
    ('\\', LEFT) => make_cart(&cart, UP),
    ('+', _) => {
      let new_dir = rerange(cart.dir + cart.next_turn, 0, 4);
      let new_turn = rerange(cart.next_turn + 1, -1, 2);
      let mut new_cart = make_cart(&cart, new_dir);
      new_cart.next_turn = new_turn;
      new_cart
    }
    _ => unreachable!(),
  }
}

pub fn solve(contents: &String) -> (i32, i32) {
  let (grid, mut carts) = preprocess(contents);
  loop {
    if carts.len() == 1 {
      let (y, x) = carts.iter().next().unwrap().yx;
      return (x, y);
    }

    let mut new_carts = HashMap::new();
    for cart in carts {
      if new_carts.contains_key(&cart.yx) {
        new_carts.remove(&cart.yx);
        continue;
      }
      let new_cart = move_cart(cart, &grid);
      if new_carts.contains_key(&new_cart.yx) {
        new_carts.remove(&new_cart.yx);
      } else {
        new_carts.insert(new_cart.yx, new_cart);
      }
    }
    carts = new_carts.into_values().collect();
    carts.sort();
  }
}
