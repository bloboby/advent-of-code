use std::collections::HashSet;

pub fn solve(contents: &String) -> i32 {
  let mut freq = 0;
  let mut seen = HashSet::new();
  loop {
    for line in contents.lines() {
      let (sign, num) = line.split_at(1);
      let n: i32 = num.parse().unwrap();

      if sign == "+" {
        freq += n;
      } else {
        freq -= n;
      }

      if seen.contains(&freq) {
        return freq;
      }
      seen.insert(freq);
    }
  }
}
