use std::cmp::min;

const N: usize = 300;
const SERIAL: i32 = 9221;

pub fn solve(_contents: &String) -> [usize; 3] {
  // Calculate all the power levels.
  let mut cells = Vec::new();
  for x in 1..(N + 1) as i32 {
    let mut row = Vec::new();
    for y in 1..(N + 1) as i32 {
      let id = x + 10;
      let mut power = id * y + SERIAL;
      power = (power * id / 100) % 10 - 5;
      row.push(power);
    }
    cells.push(row);
  }

  // Sum the rectangle up to each point.
  let mut dp = Vec::from([Vec::from([0; N + 1])]);
  for x in 0..N {
    let mut row = Vec::from([0]);
    let mut row_sum = 0;
    for y in 0..N {
      row_sum += cells[x][y];
      row.push(dp[x][y + 1] + row_sum);
    }
    dp.push(row);
  }

  // Find the largest grid.
  let mut max_power = i32::MIN;
  let mut max_id = [0, 0, 0];
  for x in 1..N + 1 {
    for y in 1..N + 1 {
      for size in 1..min(x, y) + 1 {
        let (a, b) = (x - size, y - size);
        let power = dp[x][y] - dp[a][y] - dp[x][b] + dp[a][b];
        if power > max_power {
          max_power = power;
          max_id = [a + 1, b + 1, size]
        }
      }
    }
  }
  max_id
}
