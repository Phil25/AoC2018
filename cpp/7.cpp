#include <iostream>
#include <fstream>
#include <set>

class action;
typedef std::set<action*> actionset;

class action{
	actionset deps;
	char id;

public:
	action(char id) : id(id){}

	char getId(){
		return this->id;
	}

	const actionset& getDeps(){
		return this->deps;
	}

	void addDep(action* dep){
		this->deps.insert(dep);
	}

	void removeDep(char c){
		for(auto dep : this->deps)
			if(dep->getId() == c){
				this->deps.erase(dep);
				return;
			}
	}

	bool noDeps(){
		return this->deps.size() == 0;
	}

	static action* findOrCreate(actionset& actions, char id){
		for(auto act : actions)
			if(act->getId() == id)
				return act;
		action* act = new action(id);
		actions.insert(act);
		return act;
	}
};

void getActions(const std::string& data, actionset& actions){
	std::ifstream contents(data);
	char depId, id;
	action* act = nullptr;
	action* dep = nullptr;

	while(contents >> depId >> id){
		act = action::findOrCreate(actions, id);
		dep = action::findOrCreate(actions, depId);
		act->addDep(dep);
	}
}

char getFirstDependencyless(actionset& actions){
	std::set<char> ids;
	for(const auto& act : actions)
		if(act->noDeps())
			ids.insert(act->getId());
	return *ids.begin();
}

void doAction(actionset& actions, char c){
	auto it = actions.begin();
	while(it != actions.end()){
		if((*it)->getId() == c){
			it = actions.erase(it);
		}else{
			(*it)->removeDep(c);
			++it;
		}
	}
}

void part1(actionset& actions){
	while(actions.size() > 0){
		char c = getFirstDependencyless(actions);
		doAction(actions, c);
		std::cout << c;
	}
	std::cout << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	actionset actions;
	getActions(argv[1], actions);

	part1(actions);

	return 0;
}
