all: vm

FLAGS=-std=c++17 -O3 -Wall -pedantic

vm: vm.cc
	$(CXX) $(FLAGS) -o $@ $<
