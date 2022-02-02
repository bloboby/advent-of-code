use std::collections::{HashMap, VecDeque};

const PLAYERS: i32 = 478;
const MARBLES: i64 = 71240 * 100;

pub fn rotate_ccw(marbles: &mut VecDeque<i64>, n: i32) {
  for _i in 0..n {
    let m = marbles.pop_back().unwrap();
    marbles.push_front(m);
  }
}

pub fn rotate_cw(marbles: &mut VecDeque<i64>, n: i32) {
  for _i in 0..n {
    let m = marbles.pop_front().unwrap();
    marbles.push_back(m);
  }
}

pub fn solve(_contents: &String) -> i64 {
  let mut marbles = VecDeque::from([0]);
  let mut scores = HashMap::new();
  let mut player = 1;

  for m in 1..MARBLES + 1 {
    if m % 23 == 0 {
      rotate_ccw(&mut marbles, 7);
      let score = scores.entry(player).or_insert(0);
      *score += m + marbles.pop_front().unwrap();
    } else {
      rotate_cw(&mut marbles, 2);
      marbles.push_front(m);
    }

    player += 1;
    if player > PLAYERS {
      player = 1;
    }
  }

  *scores.values().max().unwrap()
}
