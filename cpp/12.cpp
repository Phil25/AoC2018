#include <iostream>
#include <fstream>
#include <vector>
#include <array>

struct pot;
struct rule;

typedef std::array<char, 5> pattern;

struct pot{
	int number;
	bool plant;
	bool nextPlant;

	pot* next;
	pot* prev;

	pot(int number, bool plant):
		number(number), plant(plant), nextPlant(false),
		next(nullptr), prev(nullptr)
	{}

	pot* insertEnd(pot* p){
		if(next) next->insertEnd(p);
		else{
			this->next = p;
			p->prev = this;
		}
		return p;
	}

	pot* insertFront(pot* p){
		if(prev) prev->insertFront(p);
		else{
			this->prev = p;
			p->next = this;
		}
		return p;
	}

	bool getLeft(int off){
		if(!off) return plant;
		if(!prev){
			if(plant){
				pot* left = this->insertFront(new pot(number -1, false));
				left->insertFront(new pot(number -2, false));
			}
			return false;
		}
		return prev->getLeft(off -1);
	}

	bool getRight(int off){
		if(!off) return plant;
		if(!next){
			if(plant){
				pot* right = this->insertEnd(new pot(number +1, false));
				right->insertEnd(new pot(number +2, false));
			}
			return false;
		}
		return next->getRight(off -1);
	}

	int sumNums() const{
		if(!next) return plant ? number : 0;
		return next->sumNums() + (plant ? number : 0);
	}

	void print() const{
		std::cout << (plant ? '#' : '.');
		if(next) next->print();
	}

	static pot* make(int i, const std::string& data){
		return new pot(i, data[i] == '#');
	}

	static pot* parse(const std::string& data){
		int size = data.size();
		if(!size) return nullptr;

		pot* root = pot::make(0, data);

		for(int i = 1; i < size; ++i)
			root->insertEnd(pot::make(i, data));

		return root;
	}
};

struct rule{
	pattern p;

	rule(std::string& str){
		for(int i = 0; i < 5; ++i)
			p[i] = str[i] == '#';
	}

	bool match(pot* m) const{
		bool pred[5] = {
			m->getLeft(2),
			m->getLeft(1),
			m->plant,
			m->getRight(1),
			m->getRight(2)
		};

		bool res = true;
		for(int i = 0; i < 5; ++i)
			res &= p[i] == pred[i];

		return res;
	}
};

pot* findLeftmost(pot* root){
	return root->prev ? findLeftmost(root->prev) : root;
}

pot* evolve(pot* root, const std::vector<rule*>& rv){
	bool nextGen = false;
	for(const rule* r : rv)
		nextGen |= r->match(root);

	root->nextPlant = nextGen;
	if(root->next)
		evolve(root->next, rv);

	root->plant = root->nextPlant;
	return findLeftmost(root);
}

void parseRules(std::ifstream& contents, std::vector<rule*>& r){
	std::string patternStr;

	while(contents >> patternStr)
		r.push_back(new rule(patternStr));
}

void solve(std::ifstream& contents){
	std::string init;
	contents >> init;

	pot* root = pot::parse(init);

	std::vector<rule*> rv;
	parseRules(contents, rv);

	for(int i = 1; i <= 20; ++i)
		root = evolve(root, rv);

	std::cout << root->sumNums() << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	std::ifstream contents(argv[1]);
	solve(contents);

	return 0;
}
