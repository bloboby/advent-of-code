use nom::{
  bytes::complete::{tag, take_while1},
  combinator::map_res,
  IResult,
};
use std::collections::{HashMap, HashSet};

fn parse_int(input: &str) -> IResult<&str, i32> {
  map_res(take_while1(|c: char| c.is_digit(10)), |s: &str| s.parse())(input)
}

fn parse_line(input: &str) -> IResult<&str, [i32; 5]> {
  let (input, _) = tag("#")(input)?;
  let (input, id) = parse_int(input)?;
  let (input, _) = tag(" @ ")(input)?;
  let (input, x) = parse_int(input)?;
  let (input, _) = tag(",")(input)?;
  let (input, y) = parse_int(input)?;
  let (input, _) = tag(": ")(input)?;
  let (input, dx) = parse_int(input)?;
  let (input, _) = tag("x")(input)?;
  let (input, dy) = parse_int(input)?;

  Ok((input, [id, x, y, dx, dy]))
}

pub fn solve(contents: &String) -> HashSet<i32> {
  let mut coord_ids = HashMap::new();
  let mut candidates = HashSet::new();
  for line in contents.lines() {
    let [id, x, y, dx, dy] = parse_line(line).unwrap().1;
    candidates.insert(id);
    for i in x..x + dx {
      for j in y..y + dy {
        let ids = coord_ids.entry((i, j)).or_insert(HashSet::new());
        ids.insert(id);
      }
    }
  }

  // Remove ids that have overlaps.
  for ids in coord_ids.values() {
    if ids.len() > 1 {
      for id in ids {
        candidates.remove(id);
      }
    }
  }
  candidates
}
