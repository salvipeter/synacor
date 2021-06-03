#include <array>
#include <fstream>
#include <iostream>
#include <stack>
#include <vector>

class VM {
  std::array<uint16_t, 8> regs;
  std::array<uint16_t, 32768> memory;
  std::stack<uint16_t> stack;
  uint16_t ip = 0;
  std::string buffer;
  size_t buffer_index = 0;
  bool show_cmds = false;
public: 
  void loadImage(std::string);
  void run();
private:
  uint16_t get(uint16_t i) const { 
    uint16_t n = memory[i];
    return n < 32768 ? n : regs[n-32768]; 
  }
  uint16_t &reg(uint16_t i) { return regs[memory[i]-32768]; }
  void showCommand(uint16_t k, bool show_registers) const;
};

void VM::loadImage(std::string filename) {
  std::ifstream f(filename, std::ios::binary);
  static char block[2];
  size_t i = 0;
  while (!f.eof()) {
    f.read(block, 2);
    memory[i++] = *reinterpret_cast<uint16_t *>(block);
  }
  // Print the whole program
  // for (size_t k = 0; k < i; ++k)
  //   showCommand(k, false);
}

// For reverse engineering
void VM::showCommand(uint16_t k, bool show_registers) const {
  std::vector<std::string> cmds = {
    "halt", "set", "push", "pop",
    "eq", "gt", "jmp", "jt", "jf",
    "add", "mul", "mod", "and", "or", "not",
    "rmem", "wmem", "call", "ret",
    "out", "in", "noop"
  };
  std::vector<size_t> args = {
    0, 2, 1, 1, 3, 3, 1, 2, 2, 3, 3, 3, 3, 3, 2, 2, 2, 1, 0, 1, 1, 0
  };
  auto writeParam = [&](uint16_t i) {
    uint16_t n = memory[i];
    if (n >= 32768) {
      std::cout << 'r' << n - 32767;
      if (show_registers)
        std::cout << "[=" << regs[n-32768] << "]";
    } else
      std::cout << n;
  };
  std::cout << k;
  auto cmd = memory[k];
  if (cmd <= 21) {
    std::cout << '\t' << cmds[memory[k]] << '\t';
    for (size_t i = 1; i <= args[memory[k]]; ++i) {
      std::cout << ' ';
      writeParam(k + i);
    }
  }
  std::cout << std::endl;
}

void VM::run() {
  bool stopped = false;
  while (!stopped) {
    if (show_cmds)
      showCommand(ip, true);
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
                 std::getline(std::cin, buffer);
                 buffer += '\n';
                 buffer_index = 0;
                 if (buffer == "use teleporter\n") {
                   // show_cmds = true;
                   regs[7] = 25734;
                   memory[5485] = 6;  // simulate correct computation
                   memory[5489] = 21; // noops instead of calling
                   memory[5490] = 21; // the recursive function in 6027
                 }
               }
               reg(ip++) = buffer[buffer_index++];
               break;
      case 21: break;
    }
  }
}

int main(int argc, char **argv) {
  VM vm;
  vm.loadImage("challenge.bin");
  vm.run();
}

