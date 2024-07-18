use chrono::{naive::*, Duration, Timelike};
use std::collections::HashMap;

#[derive(Debug)]
enum Event {
  Guard(u32),
  Sleep,
  Wake,
}

#[derive(Debug)]
struct Log {
  time: NaiveDateTime,
  event: Event,
}

#[derive(Debug)]
struct Naps {
  total: i64,
  minute_count: HashMap<u32, i32>,
}

fn parse_line(input: &str) -> Log {
  let format = "%Y-%m-%d %H:%M";
  let time = NaiveDateTime::parse_from_str(&input[1..17], format).unwrap();

  let words: Vec<&str> = input[19..].split(' ').collect();
  let event = match words[0] {
    "Guard" => {
      let id = words[1][1..].parse().unwrap();
      Event::Guard(id)
    }
    "falls" => Event::Sleep,
    "wakes" => Event::Wake,
    _ => unreachable!(),
  };

  Log {
    time: time,
    event: event,
  }
}

fn get_max(kv: &HashMap<u32, i32>) -> (u32, i32) {
  let mut max_k = 0;
  let mut max_v = 0;
  for (k, v) in kv {
    if v > &max_v {
      max_k = *k;
      max_v = *v;
    }
  }
  (max_k, max_v)
}

pub fn solve(contents: &String) -> u32 {
  let mut events = Vec::new();
  for line in contents.lines() {
    let event = parse_line(line);
    events.push(event);
  }
  events.sort_by(|a, b| a.time.cmp(&b.time));

  let mut guard_naps = HashMap::new();
  let mut guard_id = None;
  let mut sleep_time = None;
  for event in events {
    match event.event {
      Event::Guard(id) => {
        guard_id = Some(id);
      }
      Event::Sleep => {
        sleep_time = Some(event.time);
      }
      Event::Wake => {
        let naps = guard_naps.entry(guard_id.unwrap()).or_insert(Naps {
          total: 0,
          minute_count: HashMap::new(),
        });

        let delta = event.time - sleep_time.unwrap();
        naps.total += delta.num_minutes();

        let mut t = sleep_time.unwrap();
        while t < event.time {
          let minute = t.minute();
          let count = naps.minute_count.entry(minute).or_insert(0);
          *count += 1;
          t += Duration::minutes(1);
        }

        sleep_time = None;
      }
    }
  }

  let mut max_guard = 0;
  let mut max_minute = 0;
  let mut max_count = 0;
  for (guard, naps) in guard_naps {
    let (m, c) = get_max(&naps.minute_count);
    if c > max_count {
      max_guard = guard;
      max_minute = m;
      max_count = c;
    }
  }
  max_guard * max_minute
}
