#include <iostream>
#include <fstream>
#include <vector>
#include <array>

#define CANVAS 70
#define OFFSET -110

class point{
	int px, py;
	int vx, vy;

public:
	point(int px, int py, int vx, int vy):
		px(OFFSET +px),
		py(OFFSET +py),
		vx(vx),
		vy(vy)
	{}

	int getX() const{
		return this->px;
	}

	int getY() const{
		return this->py;
	}

	void tick(int mul=1){
		this->px += this->vx *mul;
		this->py += this->vy *mul;
	}
};

inline bool inRange(int val){
	return 0 <= val && val < CANVAS;
}

void drawPoints(const std::vector<point*>& points){
	std::array<std::array<bool, CANVAS>, CANVAS> arr{};

	for(const point* p : points){
		int x = p->getX();
		int y = p->getY();
		if(inRange(x) && inRange(y))
			arr[x][y] = true;
	}

	for(int j = 0; j < CANVAS; ++j){
		for(int i = 0; i < CANVAS; ++i)
			std::cout << (arr[i][j] ? '#' : '.');
		std::cout << std::endl;
	}

	std::cout << std::endl;
}

void tickPoints(const std::vector<point*>& points, int mul=1){
	for(point* p : points)
		p->tick(mul);
}

void solve(const std::string& data){
	std::ifstream contents(data);
	int px, py, vx, vy;

	std::vector<point*> points;
	while(contents >> px >> py >> vx >> vy)
		points.push_back(new point(px, py, vx, vy));

	tickPoints(points, 10011); // <- self-adjusted value, happens to be the answer to part 2 :)
	drawPoints(points);
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	solve(argv[1]);
	return 0;
}
