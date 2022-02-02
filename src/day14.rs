const INPUT: usize = 286051;

fn get_digits(mut n: usize) -> Vec<usize> {
  let mut v = Vec::new();
  while n > 0 {
    v.push(n % 10);
    n /= 10;
  }
  v.reverse();
  v
}

fn add(
  n: usize,
  board: &mut Vec<usize>,
  digits: &Vec<usize>,
  i: &mut usize,
) -> bool {
  board.push(n);
  if digits[*i] == n {
    *i += 1;
  } else {
    *i = 0;
  }
  *i == digits.len()
}

pub fn solve(_contents: &String) -> usize {
  let mut board = vec![3, 7];
  let mut a = 0;
  let mut b = 1;

  let digits = get_digits(INPUT);
  let mut i = 0;
  loop {
    let sum = board[a] + board[b];
    if sum >= 10 {
      if add(1, &mut board, &digits, &mut i) {
        return board.len() - digits.len();
      }
    }
    if add(sum % 10, &mut board, &digits, &mut i) {
      return board.len() - digits.len();
    }

    a = (a + board[a] + 1) % board.len();
    b = (b + board[b] + 1) % board.len();
  }
}
