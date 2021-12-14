#include <iostream>
#include <fstream>
#include <string>
#include <cerrno>
#include <vector>
#include <set>
#include <queue>
#include <algorithm>

using namespace std;

std::string get_file_contents(const char *filename)
{
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (!in)
    {
      throw(errno);
    }

  std::string contents;
  in.seekg(0, std::ios::end);
  contents.resize(in.tellg());
  in.seekg(0, std::ios::beg);

  in.read(&contents[0], contents.size());
  in.close();
  return contents;
}

using heightmap_t = vector<vector<int>>;

heightmap_t parse_height_map(const char* input, int len){
  vector<vector<int>> output;
  vector<int> currRow;
  for (int i = 0; i < len; i++) {
    if (input[i] == '\n') {
      output.push_back(currRow);
      currRow = vector<int>();
    } else {
      currRow.push_back(input[i] - '0');
    }
  }
  if (currRow.size() > 0) {
    output.push_back(currRow);
  }

  return output;
}

bool isLowPoint(const heightmap_t &heightmap, int x, int y) {
  int point = heightmap[y][x];
  for (int dx = -1; dx < 2; dx++) {
    for (int dy = -1; dy < 2; dy++) {
      int checkX = x + dx;
      int checkY = y + dy;
      if ((checkX == x && checkY == y) ||
          checkX < 0 || checkY < 0 ||
          checkY >= heightmap.size() || checkX >= heightmap[checkY].size())
        continue;
      if (heightmap[checkY][checkX] <= point) {
        return false;
      }
    }
  }
  return true;
}

struct coord {
  int x;
  int y;


  coord(int x, int y) {
    this->x = x;
    this->y = y;
  }

  int hash() const {
    return this->x << 16 | this->y;
  }

  bool operator==(const coord &a) const {
    return a.x == x && a.y == y;
  }

  bool operator<(const coord &a) const {
    return this->hash() < a.hash();
  }
};

int findBasinSize(const heightmap_t &heightmap, int startX, int startY) {
  set<coord> visited;
  queue<coord> queue;
  queue.emplace(startX, startY);

  int totalSize = 0;
  while(!queue.empty()) {
    int x = queue.front().x;
    int y = queue.front().y;
    queue.pop();
    if (x < 0 || y < 0 || y >= heightmap.size() || x >= heightmap[y].size() || heightmap[y][x] == 9 || visited.count({x, y})) {
      continue;
    }

    totalSize++;
    visited.insert({x, y});

    for (int dx = -1; dx < 2; dx++) {
      for (int dy = -1; dy < 2; dy++) {
        int checkX = x + dx;
        int checkY = y + dy;
        if (dx != 0 && dy != 0) // dont allow diagonals / {x, y}
          continue;
        queue.emplace(checkX, checkY);
      }
    }
  }

  return totalSize;
}

int main() {
  std::string input = get_file_contents("input");
  auto heightmap = parse_height_map(input.c_str(), input.length());

  int lowPointCount = 0;
  int totalRisk = 0;
  int topBasins[] = {0, 0, 0};
  for (int y = 0; y < heightmap.size(); y++) {
    for (int x = 0; x < heightmap[y].size(); x++) {
      if (isLowPoint(heightmap, x, y)) {
        lowPointCount++;
        totalRisk += heightmap[y][x] + 1;

        int basinSize = findBasinSize(heightmap, x, y);
        for (int i = 0; i < 3; i++) {
          if (topBasins[i] < basinSize) {
            topBasins[i] = basinSize;
            break;
          }
        }
        sort(topBasins, topBasins + 3);
      }
    }
  }

  int acc = 1;
  for (int i = 0; i < 3; i++) {
    acc *= topBasins[i];
  }
  printf("Total low points: %d\n", lowPointCount);
  printf("Total risk: %d\n", totalRisk);
  printf("Part B answer: %d\n", acc);
}
