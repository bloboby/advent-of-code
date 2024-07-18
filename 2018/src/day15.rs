use std::collections::{HashSet, VecDeque};
use Cell::*;

type Pt = usize;

#[derive(Clone)]
enum Cell {
  Wall,
  Space,
  Elf(i32),
  Goblin(i32),
}

impl Cell {
  fn is_unit(&self) -> bool {
    matches!(self, Elf(_) | Goblin(_))
  }

  fn is_enemy(&self, other: &Self) -> bool {
    matches!((self, other), (Elf(_), Goblin(_)) | (Goblin(_), Elf(_)))
  }

  fn hp(&self) -> i32 {
    match self {
      Elf(hp) => *hp,
      Goblin(hp) => *hp,
      _ => {
        unreachable!();
      }
    }
  }
}

#[derive(Clone)]
struct State {
  grid: Vec<Cell>,
  num_rows: usize,

  units: HashSet<Pt>,
  num_elves: i32,
  num_goblins: i32,
}

impl State {
  fn parse(contents: &String) -> Self {
    let mut grid = vec![];
    let mut units = HashSet::new();
    let mut num_elves = 0;
    let mut num_goblins = 0;

    let mut i = 0;
    for line in contents.lines() {
      for c in line.chars() {
        match c {
          '#' => {
            grid.push(Wall);
          }
          '.' => {
            grid.push(Space);
          }
          'E' => {
            grid.push(Elf(200));
            units.insert(i);
            num_elves += 1;
          }
          'G' => {
            grid.push(Goblin(200));
            units.insert(i);
            num_goblins += 1;
          }
          _ => {
            unreachable!();
          }
        }
        i += 1;
      }
    }

    State {
      grid: grid,
      num_rows: contents.lines().count(),
      units: units,
      num_elves: num_elves,
      num_goblins: num_goblins,
    }
  }

  fn get_nbrs(&self, pt: Pt) -> Vec<Pt> {
    let n = self.num_rows;
    let x = pt / n;
    let y = pt % n;

    let mut nbrs = vec![];
    if x > 0 {
      nbrs.push((x - 1) * n + y);
    }
    if y > 0 {
      nbrs.push(x * n + y - 1);
    }
    if y + 1 < self.grid.len() / n {
      nbrs.push(x * n + y + 1);
    }
    if x + 1 < n {
      nbrs.push((x + 1) * n + y);
    }
    nbrs
  }

  fn get_units(&self) -> Vec<Pt> {
    let mut lineup = vec![];
    for unit in &self.units {
      lineup.push(*unit);
    }
    lineup.sort();
    lineup
  }

  fn get_dest(&self, pt: Pt) -> Option<Pt> {
    let mut bfs = VecDeque::new();
    let mut seen = HashSet::from([pt]);

    for nbr in self.get_nbrs(pt) {
      if self.grid[pt].is_enemy(&self.grid[nbr]) {
        return None;
      } else if let Space = self.grid[nbr] {
        bfs.push_back((nbr, nbr));
      }
    }

    while !bfs.is_empty() {
      let (current, dest) = bfs.pop_front().unwrap();
      if seen.contains(&current) {
        continue;
      }
      seen.insert(current);

      for nbr in self.get_nbrs(current) {
        if self.grid[pt].is_enemy(&self.grid[nbr]) {
          return Some(dest);
        } else if let Space = self.grid[nbr] {
          bfs.push_back((nbr, dest));
        }
      }
    }
    None
  }

  fn swap(&mut self, a: Pt, b: Pt) {
    self.grid.swap(a, b);
    self.units.remove(&a);
    self.units.insert(b);
  }

  fn remove(&mut self, pt: Pt) {
    if let Elf(_) = self.grid[pt] {
      self.num_elves -= 1;
    } else if let Goblin(_) = self.grid[pt] {
      self.num_goblins -= 1;
    }
    self.grid[pt] = Space;
    self.units.remove(&pt);
  }

  fn get_hp_sum(&self) -> i32 {
    let mut sum = 0;
    for unit in &self.units {
      sum += self.grid[*unit].hp();
    }
    sum
  }
}

fn get_outcome(mut st: State, power: i32) -> Option<i32> {
  let mut round = 0;
  loop {
    for mut unit in st.get_units() {
      if !st.grid[unit].is_unit() {
        // Unit already died.
        continue;
      }
      if st.num_goblins == 0 {
        return Some(round * st.get_hp_sum());
      }

      // Move.
      if let Some(dest) = st.get_dest(unit) {
        st.swap(unit, dest);
        unit = dest;
      }

      // Find target enemy.
      let mut target_hp = i32::MAX;
      let mut target = None;
      for nbr in st.get_nbrs(unit) {
        if !st.grid[unit].is_enemy(&st.grid[nbr]) {
          continue;
        }
        let hp = st.grid[nbr].hp();
        if hp < target_hp {
          target_hp = hp;
          target = Some(nbr);
        }
      }

      // Attack.
      if let Some(nbr) = target {
        if let Elf(ref mut hp) = st.grid[nbr] {
          if *hp <= 3 {
            return None;
          } else {
            *hp -= 3;
          }
        } else if let Goblin(ref mut hp) = st.grid[nbr] {
          if *hp <= power {
            st.remove(nbr);
          } else {
            *hp -= power;
          }
        }
      }
    }
    round += 1;
  }
}

pub fn solve(contents: &String) -> i32 {
  let state = State::parse(contents);

  let mut outcome = None;
  let mut power = 3;
  while outcome.is_none() {
    outcome = get_outcome(state.clone(), power);
    power += 1;
  }
  outcome.unwrap()
}
