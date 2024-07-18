use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};

const EXTRA_SECONDS: i32 = 60;
const NUM_WORKERS: i32 = 5;

pub fn solve(contents: &String) -> i32 {
  // Preprocess graph.
  let mut edges = HashMap::new();
  let mut refs = HashMap::new();
  for line in contents.lines() {
    let x = line.chars().nth(5).unwrap();
    let y = line.chars().nth(36).unwrap();

    edges.entry(x).or_insert(Vec::new()).push(y);
    refs.entry(x).or_insert(0);
    *refs.entry(y).or_insert(0) += 1;
  }

  let mut node_queue = BinaryHeap::new();
  for (n, count) in &refs {
    if *count == 0 {
      node_queue.push(Reverse(*n));
    }
  }

  // Topological sort.
  let mut last_time = 0;
  let mut workers = NUM_WORKERS - 1;
  let mut event_queue = BinaryHeap::from([Reverse((0, '.'))]);

  while let Some(Reverse((time, n))) = event_queue.pop() {
    last_time = time;
    workers += 1;

    for m in edges.entry(n).or_default() {
      let count = refs.entry(*m).or_default();
      *count -= 1;
      if *count == 0 {
        node_queue.push(Reverse(*m));
      }
    }

    while workers > 0 && !node_queue.is_empty() {
      let Reverse(m) = node_queue.pop().unwrap();
      let new_time = (m as i32) - ('A' as i32) + 1 + EXTRA_SECONDS + time;
      event_queue.push(Reverse((new_time, m)));
      workers -= 1;
    }
  }

  last_time
}
