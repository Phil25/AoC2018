#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

void pushDigits(std::vector<int>& list, int num){
	if(num > 9){
		list.push_back(1);
		list.push_back(num -10);
	}else list.push_back(num);
}

int cycle(int val, int by, int max){
	val += by %max;
	val -= val >= max ? max : 0;
	return val;
}

void iteration(std::vector<int>& list, int& e1, int& e2){
	pushDigits(list, list[e1] +list[e2]);
	int size = list.size();
	e1 = cycle(e1, list[e1] +1, size);
	e2 = cycle(e2, list[e2] +1, size);
}

void printSub(const std::vector<int>& list, int from, int to){
	for(int i = from; i < to; ++i)
		std::cout << list[i];
	std::cout << std::endl;
}

void part1(int n){
	std::vector<int> list{3,7};
	unsigned int max = n +10;
	int e1 = 0, e2 = 1;

	do iteration(list, e1, e2);
	while(list.size() < max);

	printSub(list, n, max);
}

bool containsBetween(const std::vector<int>& list, const std::vector<int>& val, int from, int to){
	int max = to -from;
	for(int i = 0; i < max; ++i)
		if(list[from +i] != val[i])
			return false;
	return true;
}

bool containsFromBack(const std::vector<int>& list, const std::vector<int>& val, int& beginsAt){
	int len1 = list.size();
	int len2 = val.size();
	if(len2 > len1) return false;

	beginsAt = len1 -len2;
	if(containsBetween(list, val, beginsAt, len1))
		return true;

	--beginsAt;
	return containsBetween(list, val, beginsAt, len1 -1);
}

std::vector<int> toDigitVec(int n){
	std::vector<int> vec;
	while(n > 0){
		vec.push_back(n %10);
		n /= 10;
	}
	std::reverse(vec.begin(), vec.end());
	return vec;
}

void part2(int n){
	std::vector<int> list{3,7};
	std::vector<int> val = toDigitVec(n);
	int e1 = 0, e2 = 1;

	int beginsAt;

	do iteration(list, e1, e2);
	while(!containsFromBack(list, val, beginsAt));
	std::cout << beginsAt << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	int n;
	std::ifstream contents(argv[1]);
	contents >> n;
	part1(n);
	part2(n);

	return 0;
}
