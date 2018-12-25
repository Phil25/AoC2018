#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

class track{
	std::vector<std::vector<char>> rails;
	int X, Y;

public:
	track(const std::string& data) : X(0), Y(0){
		std::ifstream contents(data);
		std::string line;
		while(std::getline(contents, line))
			this->pushLayer(line);
	}

	void pushLayer(const std::string& line){
		std::vector<char> layer(line.begin(), line.end());

		for(char& c : layer)
			c = stripCarts(c);

		this->rails.push_back(layer);
		X = !X ? layer.size() : X, ++Y;
	}

	int getWidth() const{
		return X;
	}

	int getHeight() const{
		return Y;
	}

	char getRail(int x, int y) const{
		return rails[y][x];
	}

	std::vector<std::vector<char>> getRails() const{
		return rails;
	}

	void print() const{
		for(const auto& layer : rails){
			for(char c : layer)
				std::cout << c;
			std::cout << std::endl;
		}
	}

	static char stripCarts(char c){
		if(c == '>' || c == '<')
			return '-';
		else if(c == 'v' || c == '^')
			return '|';
		return c;
	}
};

class cart{
	char c;
	int x, y;
	int nextTurn;
	bool crashed;

public:
	cart(char c, int x, int y):
		c(c), x(x), y(y), nextTurn(0), crashed(false)
	{}

	char getCart() const{
		return c;
	}

	int getX() const{
		return x;
	}

	int getY() const{
		return y;
	}

	bool getCrashed() const{
		return crashed;
	}

	bool move(std::vector<cart*>& carts, const track& t){
		translate();
		markCrashed(carts);
		c = turn(t.getRail(x, y));
		return true;
	}

private:
	void translate(){
		switch(c){
			case '<': --x; break;
			case '>': ++x; break;
			case '^': --y; break;
			case 'v': ++y; break;
		}
	}

	void markCrashed(std::vector<cart*>& carts){
		for(cart* other : carts)
			if(checkCrash(this, other))
				this->crashed = other->crashed = true;
	}

	char turn(char rail){
		switch(rail){
			case '/': switch(c){
				case '<': return 'v';
				case '>': return '^';
				case '^': return '>';
				case 'v': return '<';
			}

			case '\\': switch(c){
				case '<': return '^';
				case '>': return 'v';
				case '^': return '<';
				case 'v': return '>';
			}

			case '+':
				int turn = nextTurn;
				++nextTurn %= 3;

				if(turn != 1) switch(c){
					case '<': return !turn ? 'v' : '^';
					case '>': return !turn ? '^' : 'v';
					case '^': return !turn ? '<' : '>';
					case 'v': return !turn ? '>' : '<';
				}
		}
		return c;
	}

public:
	static bool compare(const cart* c1, const cart* c2){
		return c1->y != c2->y
			? c1->y < c2->y // compare against Y
			: c1->x < c2->x;// compare against X
	}

	static bool checkCrash(const cart* c1, const cart* c2){
		return c1 != c2
			&& c1->x == c2->x
			&& c1->y == c2->y;
	}
};

class cartManager{
	std::vector<cart*> carts;

public:
	cartManager(const std::string& data){
		std::ifstream contents(data);
		std::string line;
		int y = -1;
		while(++y, std::getline(contents, line)){
			int size = line.size();
			for(int x = 0; x < size; ++x)
				if(isCart(line[x]))
					carts.push_back(new cart(line[x], x, y));
		}
	}

	int getCartsSize() const{
		return carts.size();
	}

	const cart* getCart(int i) const{
		return carts[i];
	}

	bool tick(const track& t){
		std::sort(carts.begin(), carts.end(), cart::compare);
		for(cart* c : carts)
			c->move(carts, t);

		removeCrashed();

		return true;
	}

	void removeCrashed(){
		auto it = carts.begin();
		while(it != carts.end())
			if((*it)->getCrashed())
				it = carts.erase(it);
			else ++it;
	}

	void print(const track& t) const{
		std::vector<std::vector<char>> blocks = t.getRails();
		for(const cart* c : carts)
			blocks[c->getY()][c->getX()] = c->getCart();

		for(const auto& layer : blocks){
			for(char c : layer)
				std::cout << c;
			std::cout << std::endl;
		}
	}

	inline static bool isCart(char c){
		return c == '<' || c == '>' || c == '^' || c == 'v';
	}
};

void solve(const std::string& data){
	track t(data);
	cartManager man(data);

	while(man.tick(t), man.getCartsSize() > 1);
	const cart* c = man.getCart(0);
	std::cout << "Last cart at " << c->getX() << ',' << c->getY() << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	solve(argv[1]);

	return 0;
}
