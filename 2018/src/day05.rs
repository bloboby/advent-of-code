fn to_lower(c: char) -> char {
  c.to_lowercase().next().unwrap()
}

fn should_react(a: char, b: char) -> bool {
  let same_type = to_lower(a) == to_lower(b);
  let opp_polarity = a.is_lowercase() != b.is_lowercase();
  same_type && opp_polarity
}

fn try_react(input: &String) -> Option<String> {
  let mut output = String::new();
  let mut reacted = false;
  let mut prev_char = None;

  for c in input.chars() {
    if let Some(prev) = prev_char {
      if should_react(prev, c) {
        reacted = true;
        prev_char = None;
      } else {
        prev_char = Some(c);
        output.push(prev);
      }
    } else {
      prev_char = Some(c);
    }
  }
  if let Some(c) = prev_char {
    output.push(c);
  }

  if reacted {
    Some(output)
  } else {
    None
  }
}

pub fn solve(contents: &String) -> usize {
  let polymer = contents.trim().to_string();

  let mut min_length = usize::MAX;
  for letter in "abcdefghijklmnopqrstuvwxyz".chars() {
    let mut new_polymer = String::new();
    for c in polymer.chars() {
      if to_lower(c) != letter {
        new_polymer.push(c);
      }
    }

    while let Some(next) = try_react(&new_polymer) {
      new_polymer = next;
    }

    if new_polymer.len() < min_length {
      min_length = new_polymer.len();
    }
  }
  min_length
}
