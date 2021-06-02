#include <array>
#include <fstream>
#include <iostream>
#include <stack>

class VM {
  std::array<uint16_t, 8> regs;
  std::array<uint16_t, 32768> memory;
  std::stack<uint16_t> stack;
  uint16_t ip = 0;
public: 
  void loadImage(std::string);
  void run();
private:
  uint16_t get(uint16_t i) const { 
    uint16_t n = memory[i];
    return n < 32768 ? n : regs[n-32768]; 
  }
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
      case 19: std::cout << (char)get(ip++); break; 
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

