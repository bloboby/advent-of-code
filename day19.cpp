#include <algorithm>  // max
#include <cctype>     // isdigit
#include <iostream>
#include <queue>  // priority_queue
#include <sstream>
#include <string>
#include <vector>

using namespace std;

struct State {
  State(int id, int a, int b, int c, int d, int e, int f)
      : id(id), a(a), b(b), c(c), d(d), e(e), f(f) {
    max_ore_bots = max(a, max(b, max(c, e)));
  }

  int score() const {
    // Make 1 geode bot every second until the end.
    int t = end_time - time;
    return geodes + geode_bots * t + t * (t - 1) / 2;
  }

  const bool operator<(const State& other) const {
    if (this->score() == other.score()) {
      if (this->geodes == other.geodes) return this->time > other.time;
      return this->geodes < other.geodes;
    }
    return this->score() < other.score();
  }

  void collect() {
    ++time;
    ore += ore_bots;
    clay += clay_bots;
    obsidian += obsidian_bots;
    geodes += geode_bots;
  }

  vector<State> get_nbrs() const {
    // Wait until a bot can be produced, or simply produce geodes until the end.
    vector<State> nbrs;

    if (ore_bots < max_ore_bots) {
      State copy = *this;
      while (copy.ore < a) copy.collect();
      copy.ore -= a;
      copy.collect();
      ++copy.ore_bots;
      nbrs.push_back(copy);
    }
    if (clay_bots < d) {
      State copy = *this;
      while (copy.ore < b) copy.collect();
      copy.ore -= b;
      copy.collect();
      ++copy.clay_bots;
      nbrs.push_back(copy);
    }
    if (clay_bots > 0 && obsidian_bots < f) {
      State copy = *this;
      while (copy.ore < c || copy.clay < d) copy.collect();
      copy.ore -= c;
      copy.clay -= d;
      copy.collect();
      ++copy.obsidian_bots;
      nbrs.push_back(copy);
    }
    if (obsidian_bots > 0) {
      State copy = *this;
      while (copy.ore < e || copy.obsidian < f) copy.collect();
      copy.ore -= e;
      copy.obsidian -= f;
      copy.collect();
      ++copy.geode_bots;
      nbrs.push_back(copy);
    }
    if (geode_bots > 0) {
      State copy = *this;
      while (copy.time < end_time) copy.collect();
      nbrs.push_back(copy);
    }

    return nbrs;
  }

  int id, a, b, c, d, e, f;
  int max_ore_bots;
  int end_time;
  int time = 0;

  int ore = 0;
  int clay = 0;
  int obsidian = 0;
  int geodes = 0;

  int ore_bots = 1;
  int clay_bots = 0;
  int obsidian_bots = 0;
  int geode_bots = 0;
};

int find_best(State start, int end_time) {
  start.end_time = end_time;
  priority_queue<State> pq;
  pq.push(start);

  int best = 0;
  while (true) {
    State s = pq.top();
    pq.pop();

    if (s.time > end_time) continue;
    if (s.score() <= best) break;
    if (s.time == end_time) {
      if (s.geodes > best) best = s.geodes;
      continue;
    }

    for (auto nbr : s.get_nbrs()) pq.push(nbr);
  }

  return best;
}

int main() {
  vector<State> blueprints;
  string line;
  while (getline(cin, line)) {
    string filtered;
    for (char c : line) {
      if (isdigit(c) || c == ' ') {
        filtered.push_back(c);
      }
    }
    stringstream ss(filtered);
    int id, a, b, c, d, e, f;
    ss >> id >> a >> b >> c >> d >> e >> f;
    State s(id, a, b, c, d, e, f);
    blueprints.push_back(s);
  }

  int part1 = 0;
  for (const State& bp : blueprints) {
    int best = find_best(bp, 24);
    part1 += bp.id * best;
  }
  cout << part1 << endl;

  int part2 = 1;
  for (int i = 0; i < 3; ++i) {
    int best = find_best(blueprints[i], 32);
    part2 *= best;
  }
  cout << part2 << endl;

  return 0;
}