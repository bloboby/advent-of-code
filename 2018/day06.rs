use std::collections::{HashMap, HashSet};

type Pt = (i32, i32);

fn parse(contents: &String) -> Vec<Pt> {
  let mut points = Vec::new();
  for line in contents.lines() {
    let mut xy = Vec::<i32>::new();
    for num in line.split(", ") {
      xy.push(num.parse().unwrap());
    }
    points.push((xy[0], xy[1]));
  }
  points
}

fn find_closest(
  point_dists: &HashMap<Pt, HashMap<Pt, i32>>,
  coord: Pt,
) -> Option<Pt> {
  let mut min_point = (0, 0);
  let mut min_dist = i32::MAX;
  let mut doubled = false;

  for (point, dist) in point_dists {
    let d = dist.get(&coord).unwrap();
    if *d < min_dist {
      min_point = *point;
      min_dist = *d;
      doubled = false;
    } else if *d == min_dist {
      doubled = true
    }
  }

  if doubled {
    None
  } else {
    Some(min_point)
  }
}

fn part1(point_dists: &HashMap<Pt, HashMap<Pt, i32>>, dx: i32, dy: i32) -> i32 {
  // Find the closest point for each coordinate.
  let mut point_sizes = HashMap::new();
  let mut infinite = HashSet::new();
  for i in 0..dx {
    for j in 0..dy {
      if let Some(closest) = find_closest(&point_dists, (i, j)) {
        if i == 0 || i == dx - 1 || j == 0 || j == dy - 1 {
          infinite.insert(closest);
        } else {
          let size = point_sizes.entry(closest).or_insert(0);
          *size += 1;
        }
      }
    }
  }

  // Find the largest size.
  let mut max_size = 0;
  for (point, size) in point_sizes {
    if infinite.contains(&point) {
      continue;
    }
    if size > max_size {
      max_size = size;
    }
  }
  max_size
}

/*
fn part2(
  point_dists: &HashMap<Pt, HashMap<Pt, i32>>,
  dx: i32,
  dy: i32,
) -> i32 {
  for i in 0..dx {
    for j in 0..dy {
      let mut total_dist = 0;
      for dist in point_dists.values() {

      }
    }
  }
  0 // TODO: need to iterate from like -10000 to +10000
}
*/

pub fn solve(contents: &String) -> i32 {
  let points = parse(contents);

  let min_x = points.iter().min_by(|a, b| a.0.cmp(&b.0)).unwrap().0;
  let min_y = points.iter().min_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
  let max_x = points.iter().max_by(|a, b| a.0.cmp(&b.0)).unwrap().0;
  let max_y = points.iter().max_by(|a, b| a.1.cmp(&b.1)).unwrap().1;
  let dx = max_x - min_x + 1;
  let dy = max_y - min_y + 1;

  // Calculate the distance matrix for each point.
  let mut point_dists = HashMap::new();
  for (x, y) in points {
    let mut dist = HashMap::new();
    for i in 0..dx {
      for j in 0..dy {
        let dist_x = ((x - min_x) - i).abs();
        let dist_y = ((y - min_y) - j).abs();
        dist.insert((i, j), dist_x + dist_y);
      }
    }
    point_dists.insert((x, y), dist);
  }

  part1(&point_dists, dx, dy)
}
