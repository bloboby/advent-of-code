#include <iostream>
#include <map>
#include <queue>
#include <sstream>
#include <string>
#include <utility>  // pair
#include <vector>

using namespace std;

enum ModuleType { BROADCAST, FLIP, CON };

struct Module {
  ModuleType type;
  vector<string> dests;
  bool state = false;     // Flip
  map<string, bool> mem;  // Con

  void reset() {
    this->state = false;
    for (const auto& kv : this->mem) this->mem[kv.first] = false;
  }

  Module() = default;
  Module(ModuleType type, const vector<string>& dests)
      : type(type), dests(dests) {}
};

struct Signal {
  bool pulse;
  string from;
  string to;
};

bool push(map<string, Module>& modules, const string& target) {
  // vector<int> count = {1, 0};
  deque<Signal> q;
  q.push_back({0, "button", "broadcaster"});

  bool high = false;

  while (!q.empty()) {
    Signal signal = q.front();
    q.pop_front();

    if (!modules.count(signal.to)) continue;
    Module& m = modules[signal.to];

    bool in = signal.pulse;
    bool out;
    if (m.type == ModuleType::BROADCAST) {
      out = in;
    } else if (m.type == ModuleType::FLIP) {
      if (in) continue;
      m.state = !m.state;
      out = m.state;
    } else if (m.type == ModuleType::CON) {
      m.mem[signal.from] = in;
      out = false;
      for (const auto& kv : m.mem) {
        if (!kv.second) out = true;
      }
    }

    for (const string& dest : m.dests) q.push_back({out, signal.to, dest});

    // count[out] += m.dests.size();
    if (signal.to == target && out) high = true;
  }

  return high;
}

int main() {
  map<string, Module> modules;
  string line;
  while (getline(cin, line)) {
    string name, arrow, dest;
    vector<string> dests;

    stringstream ss(line);
    ss >> name >> arrow;
    while (ss >> dest) {
      if (dest.back() == ',') dest.pop_back();
      dests.push_back(dest);
    }

    if (name == "broadcaster") {
      modules[name] = Module(ModuleType::BROADCAST, dests);
    } else {
      ModuleType type = name[0] == '%' ? ModuleType::FLIP : ModuleType::CON;
      name.erase(0, 1);
      modules[name] = Module(type, dests);
    }
  }

  // Initialise all conjunction modules.
  for (const auto& kv : modules) {
    for (const string& dest : kv.second.dests) {
      if (modules[dest].type != ModuleType::CON) continue;
      modules[dest].mem[kv.first] = false;
    }
  }

  // Part 1
  // vector<int> count = {0, 0};
  // for (int i = 0; i < 1000; ++i) {
  //   auto delta = push(modules);
  //   count[0] += delta[0];
  //   count[1] += delta[1];
  // }
  // cout << count[0] * count[1] << endl;

  // Part 2
  // Hardcoded. These need to all send HIGH to CON cl.
  vector<string> targets = {"js", "qs", "dt", "ts"};
  for (const string& target : targets) {
    for (auto& kv : modules) kv.second.reset();

    int pushes = 0;
    cout << target;
    for (int i = 0; i < 2; ++i) {
      // Run twice to check that it loops.
      while (true) {
        ++pushes;
        if (push(modules, target)) {
          cout << " " << pushes;
          break;
        }
      }
    }
    cout << endl;
  }

  return 0;
}