use std::collections::{HashSet, VecDeque};

type State = (VecDeque<char>, i64);
type Rules = HashSet<VecDeque<char>>;

const GENS: i64 = 50000000000;

fn parse(contents: &String) -> (State, Rules) {
  let input: Vec<&str> = contents.split("\n\n").collect();

  let mut v = VecDeque::new();
  for x in input[0].split(" ").last().unwrap().chars() {
    v.push_back(x);
  }

  let mut l = 0;
  while v[l as usize] == '.' {
    l += 1;
  }

  let mut rules = HashSet::new();
  for line in input[1].lines() {
    let v: Vec<&str> = line.split(" ").collect();
    let pattern = VecDeque::from(v[0].chars().collect::<Vec<char>>());
    if v[2] == "#" {
      rules.insert(pattern);
    }
  }

  ((v, l), rules)
}

fn evolve(state: &State, rules: &Rules) -> State {
  let (mut v, mut l) = state.clone();

  // Pad the ends.
  while v[0] == '#' || v[1] == '#' {
    v.push_front('.');
    l -= 1;
  }
  while v[0] == '.' && v[1] == '.' {
    v.pop_front();
    l += 1;
  }
  while v[v.len() - 1] == '#' || v[v.len() - 2] == '#' {
    v.push_back('.');
  }

  let mut acc = VecDeque::from(['.', '.', '.', v[0], v[1]]);
  for i in 0..v.len() {
    // Update the accumulator.
    acc.pop_front();
    if i < v.len() - 2 {
      acc.push_back(v[i + 2]);
    } else {
      acc.push_back('.');
    }

    if rules.contains(&acc) {
      v[i] = '#';
    } else {
      v[i] = '.';
    }
  }

  (v, l)
}

fn sum(state: &State) -> i64 {
  let mut sum = 0;
  let mut l = state.1;
  for c in state.0.iter() {
    if *c == '#' {
      sum += l;
    }
    l += 1;
  }
  sum
}

pub fn solve(contents: &String) -> i64 {
  let (mut state, rules) = parse(contents);
  for gen in 0..GENS {
    let new_state = evolve(&state, &rules);
    if new_state.0 == state.0 {
      let (new_sum, sum) = (sum(&new_state), sum(&state));
      let m = new_sum - sum;
      let c = new_sum - (gen + 1) * m;
      return GENS * m + c;
    }
    state = new_state;
  }
  unreachable!();
}
