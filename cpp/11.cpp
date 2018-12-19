#include <iostream>
#include <fstream>
#include <array>

typedef std::array<std::array<int, 300>, 300> grid;

inline int getHundredDigit(int val){
	val = val < 0 ? -val : val;
	if(val < 100) return 0;
	return ((val -(val %100)) /100) %10;
}

inline int getPowerLevel(int x, int y, int serial){
	int rack = x +10,
		power = rack *y;
	power += serial;
	power *= rack;
	power = getHundredDigit(power);
	return power -5;
}

void fillGrid(grid& arr, int serial){
	for(int x = 0; x < 300; ++x)
		for(int y = 0; y < 300; ++y)
			arr[x][y] = getPowerLevel(x +1, y +1, serial);
}

int getTotal(const grid& arr, int x, int y, int size){
	int total = 0;
	for(int i = 0; i < size; ++i)
		for(int j = 0; j < size; ++j)
			total += arr[x+i][y+j];
	return total;
}

int findLargestSquare(const grid& arr, int& X, int& Y, int size=3){
	int largest = 0, range = 300 -size +1;
	for(int x = 0; x < range; ++x){
		for(int y = 0; y < range; ++y){
			int curr = getTotal(arr, x, y, size);
			if(curr <= largest) continue;
			largest = curr;
			X = x;
			Y = y;
		}
	}
	++X;
	++Y;
	return largest;
}

void solve(int serial){
	grid arr;
	int lx = 0, ly = 0;
	int x = 0, y = 0;
	int largest = 0;
	int size = 1;

	fillGrid(arr, serial);
	for(int i = 1; i <= 50; ++i){
		int curr = findLargestSquare(arr, x, y, i);
		if(curr > largest)
			lx = x, ly = y, size = i, largest = curr;
	}

	std::cout << lx << ',' << ly << ',' << size << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	std::ifstream contents(argv[1]);
	int serial;
	contents >> serial;
	solve(serial);

	return 0;
}
