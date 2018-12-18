#include <iostream>
#include <fstream>
#include <vector>

class marble{
	int num;
	marble* next;
	marble* prev;

public:
	marble(int num):
		num(num),
		next(this),
		prev(this)
	{}

	int getNum() const{
		return this->num;
	}

	marble* getNext() const{
		return this->next;
	}

	void setNext(marble* next){
		this->next = next;
	}

	marble* getPrev() const{
		return this->prev;
	}

	void setPrev(marble* prev){
		this->prev = prev;
	}
};

class marbleList{
	marble* curr;
	marble* first;

public:
	marbleList() : curr(new marble(0)){
		this->first = curr;
	}

	unsigned int place(int num){
		if(num % 23 != 0){
			this->insert(new marble(num));
			return 0;
		}
		unsigned int score = this->remove7th();
		return num +score;
	}

private:
	void insert(marble* m){
		marble* l = curr->getNext();
		marble* r = l->getNext();

		l->setNext(m);
		r->setPrev(m);

		m->setPrev(l);
		m->setNext(r);

		curr = m;
	}

	unsigned int remove7th(){
		int i = 7;
		marble* trg = this->curr;
		while(trg = trg->getPrev(), --i > 0);
		unsigned int score = trg->getNum();

		marble* l = trg->getPrev();
		marble* r = trg->getNext();
		delete trg;

		l->setNext(r);
		r->setPrev(l);
		this->curr = r;

		return score;
	}
};

unsigned int getHighScore(int playerCount, unsigned int marbleCount){
	std::vector<int> players(playerCount);
	marbleList list;

	unsigned int i = 0;
	while(++i <= marbleCount)
		players[(i-1) % playerCount] += list.place(i);

	unsigned int highScore = 0;
	for(unsigned int score : players)
		if(score > highScore)
			highScore = score;

	return highScore;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	std::ifstream contents(argv[1]);
	int playerCount, marbleCount;
	contents >> playerCount >> marbleCount;

	std::cout << getHighScore(playerCount, marbleCount) << std::endl;
	std::cout << getHighScore(playerCount, marbleCount *100) << std::endl;

	return 0;
}
