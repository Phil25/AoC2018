#include <iostream>
#include <fstream>
#include <vector>

typedef std::vector<int>::iterator iter;

class node{
	std::vector<node*> children;
	std::vector<int> metadata;
	int childCount, metaCount;

public:
	node(iter begin, iter end):
		childCount(*begin),
		metaCount(*(begin +1))
	{
		int i = metaCount +1;
		while(--i) metadata.push_back(*(--end));
		parseChildren(begin +2, end);
	}

	int sumMeta() const{
		int sum = 0;
		for(auto i : this->metadata)
			sum += i;
		for(const node* n : this->children)
			sum += n->sumMeta();
		return sum;
	}

	int getValue() const{
		if(this->childCount == 0)
			return this->sumMeta();

		int val = 0;
		for(auto i : this->metadata)
			if(1 <= i && i <= this->childCount)
				val += this->children[--i]->getValue();

		return val;
	}

	static int getChildLen(iter begin){
		int len = 2 + *(begin +1);
		if(*begin == 0) return len;

		int i = *begin, subLen = 0;
		begin += 2;
		while(--i >= 0){
			subLen = getChildLen(begin);
			len += subLen;
			begin += subLen;
		}

		return len;
	}

private:
	void parseChildren(iter begin, iter end){
		while(begin != end){
			iter childEnd = begin +getChildLen(begin);
			children.push_back(new node(begin, childEnd));
			begin = childEnd;
		}
	}
};

void part1and2(const std::string& data){
	std::ifstream contents(data);
	std::vector<int> nums;
	int i;

	while(contents >> i)
		nums.push_back(i);

	node root(nums.begin(), nums.end());
	std::cout << root.sumMeta() << std::endl;
	std::cout << root.getValue() << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	part1and2(argv[1]);
	return 0;
}
