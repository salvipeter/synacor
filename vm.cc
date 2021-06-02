#include <array>
#include <fstream>
#include <iostream>
#include <stack>

class VM {
  std::array<uint16_t, 8> regs;
  std::array<uint16_t, 32768> memory;
  std::stack<uint16_t> stack;
  uint16_t ip = 0;
  std::string buffer;
  size_t buffer_index = 0;
public: 
  void loadImage(std::string);
  void run();
private:
  uint16_t get(uint16_t i) const { 
    uint16_t n = memory[i];
    return n < 32768 ? n : regs[n-32768]; 
  }
  uint16_t &reg(uint16_t i) { return regs[memory[i]-32768]; }
};

void VM::loadImage(std::string filename) {
  std::ifstream f(filename, std::ios::binary);
  static char block[2];
  size_t i = 0;
  while (!f.eof()) {
    f.read(block, 2);
    memory[i++] = *reinterpret_cast<uint16_t *>(block);
  }
}

void VM::run() {
  bool stopped = false;
  while (!stopped)
    switch(memory[ip++]) {
      case 0: stopped = true; break;
      case 1: reg(ip) = get(ip + 1); ip += 2; break;
      case 2: stack.push(get(ip++)); break;
      case 3: reg(ip++) = stack.top(); stack.pop(); break;
      case 4: reg(ip) = get(ip + 1) == get(ip + 2) ? 1 : 0; ip += 3; break;
      case 5: reg(ip) = get(ip + 1) > get(ip + 2) ? 1 : 0; ip += 3; break;
      case 6: ip = get(ip); break;
      case 7: ip = get(ip) != 0 ? get(ip + 1) : ip + 2; break;
      case 8: ip = get(ip) == 0 ? get(ip + 1) : ip + 2; break;
      case 9: reg(ip) = (get(ip + 1) + get(ip + 2)) % 32768; ip += 3; break;
      case 10: reg(ip) = (get(ip + 1) * get(ip + 2)) % 32768; ip += 3; break;
      case 11: reg(ip) = get(ip + 1) % get(ip + 2); ip += 3; break;
      case 12: reg(ip) = get(ip + 1) & get(ip + 2); ip += 3; break;
      case 13: reg(ip) = get(ip + 1) | get(ip + 2); ip += 3; break;
      case 14: reg(ip) = ~get(ip + 1) & 0x7fff; ip += 2; break;
      case 15: reg(ip) = memory[get(ip + 1)]; ip += 2; break;
      case 16: memory[get(ip)] = get(ip + 1); ip += 2; break;
      case 17: stack.push(ip + 1); ip = get(ip); break;
      case 18: ip = stack.top(); stack.pop(); break;
      case 19: std::cout << (char)get(ip++); break; 
      case 20: if (buffer_index == buffer.size()) {
                 std::cin >> buffer;
                 buffer_index = 0;
               }
               reg(ip++) = buffer[buffer_index++];
               break;
      case 21: break;
    }
}

int main(int argc, char **argv) {
  std::string filename = "challenge.bin";
  if (argc > 1)
    filename = argv[1];
  VM vm;
  vm.loadImage(filename);
  vm.run();
}

