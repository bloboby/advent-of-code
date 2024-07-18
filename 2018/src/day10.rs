use std::collections::HashSet;

type Pt = (i32, i32, i32, i32);

fn parse_line(line: &str, points: &mut Vec<Pt>) {
  let mut d = Vec::new();
  for (i, c) in line.char_indices() {
    if c == '<' || c == ',' || c == '>' {
      d.push(i);
    }
  }

  let f = |i| -> i32 {
    line
      .get(d[i] + 1..d[i + 1])
      .unwrap()
      .trim()
      .parse()
      .unwrap()
  };
  points.push((f(0), f(1), f(3), f(4)));
}

fn evolve(points: &Vec<Pt>) -> Vec<Pt> {
  let mut new_points = Vec::new();
  for (x, y, dx, dy) in points.iter() {
    new_points.push((*x + *dx, *y + *dy, *dx, *dy));
  }
  new_points
}

fn get_bounds(points: &Vec<Pt>) -> [i32; 4] {
  let mut min_x = i32::MAX;
  let mut min_y = i32::MAX;
  let mut max_x = 0;
  let mut max_y = 0;
  for (x, y, _, _) in points {
    if *x < min_x {
      min_x = *x;
    }
    if *x > max_x {
      max_x = *x;
    }
    if *y < min_y {
      min_y = *y;
    }
    if *y > max_y {
      max_y = *y;
    }
  }
  [min_x, min_y, max_x, max_y]
}

fn get_size(points: &Vec<Pt>) -> i32 {
  let [min_x, min_y, max_x, max_y] = get_bounds(points);
  max_x - min_x + max_y - min_y
}

fn display(points: &Vec<Pt>) {
  let mut points_set = HashSet::new();
  for (x, y, _, _) in points {
    points_set.insert((x, y));
  }

  let [min_x, min_y, max_x, max_y] = get_bounds(points);
  for y in min_y..max_y + 1 {
    let mut line = String::new();
    for x in min_x..max_x + 1 {
      if points_set.contains(&(&x, &y)) {
        line.push('#');
      } else {
        line.push('.');
      }
    }
    println!("{}", line);
  }
}

pub fn solve(contents: &String) -> i32 {
  let mut points = Vec::new();
  for line in contents.lines() {
    parse_line(line, &mut points);
  }

  let mut time = 0;
  let mut size = get_size(&points);
  loop {
    let next_points = evolve(&points);
    let next_size = get_size(&next_points);
    if next_size < size {
      time += 1;
      points = next_points;
      size = next_size;
    } else {
      display(&points);
      return time;
    }
  }
}
