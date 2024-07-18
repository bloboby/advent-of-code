use std::collections::HashMap;

fn create_histogram(id: &str) -> HashMap<char, i32> {
  let mut counts = HashMap::new();
  for c in id.chars() {
    let count = counts.entry(c).or_insert(0);
    *count += 1;
  }
  return counts;
}

fn count_if(counts: &HashMap<char, i32>, target: i32, acc: &mut i32) {
  for count in counts.values() {
    if *count == target {
      *acc += 1;
      return;
    }
  }
}

fn part1(contents: &String) -> i32 {
  let mut num_pairs = 0;
  let mut num_triples = 0;
  for id in contents.lines() {
    let histogram = create_histogram(id);
    count_if(&histogram, 2, &mut num_pairs);
    count_if(&histogram, 3, &mut num_triples);
  }
  num_pairs * num_triples
}

fn maybe_match(s1: &str, s2: &str) -> Option<String> {
  let mut dist = 0;
  let mut common = String::new();
  for (c1, c2) in s1.chars().zip(s2.chars()) {
    if c1 != c2 {
      dist += 1;
      if dist > 1 {
        return None;
      }
    } else {
      common.push(c1);
    }
  }
  Some(common)
}

fn part2(contents: &String) -> String {
  let mut seen = Vec::<&str>::new();
  for id in contents.lines() {
    for other in &seen {
      if let Some(common) = maybe_match(id, other) {
        return common;
      }
    }
    seen.push(id);
  }
  unreachable!()
}

pub fn solve(contents: &String) -> (i32, String) {
  (part1(&contents), part2(&contents))
}
