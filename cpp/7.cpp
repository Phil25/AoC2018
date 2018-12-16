#include <iostream>
#include <fstream>
#include <array>
#include <set>

class action;
typedef std::set<action*> actionset;

class action{
	actionset deps;
	char id;
	char inProgress;

public:
	action(char id) : id(id), inProgress(false){}

	char getId() const{
		return this->id;
	}

	const actionset& getDeps() const{
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

	bool noDeps() const{
		return this->deps.size() == 0;
	}

	void accept(){
		this->inProgress = true;
	}

	bool isAccepted() const{
		return this->inProgress;
	}

	static void complete(actionset& actions, char id){
		auto it = actions.begin();
		while(it != actions.end()){
			if((*it)->getId() == id){
				it = actions.erase(it);
			}else{
				(*it)->removeDep(id);
				++it;
			}
		}
	}

	static action* find(actionset& actions, char id){
		for(auto act : actions)
			if(act->getId() == id)
				return act;
		return nullptr;
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

class worker{
	int timeLeft;
	char jobId;

public:
	static actionset* all;

	worker() : timeLeft(0), jobId('0'){}

	bool addJob(action* act){
		if(!act || this->timeLeft > 0 || act->isAccepted())
			return false;

		this->timeLeft = 60 +act->getId() -'A' +1;
		this->jobId = act->getId();
		act->accept();
		return true;
	}

	void tick(){
		--this->timeLeft;
		if(timeLeft == 0)
			action::complete(*worker::all, this->jobId);
	}
};

actionset* worker::all = nullptr;

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

void getDependencyless(const actionset& all, std::set<char>& undone){
	for(const auto& act : all)
		if(act->noDeps())
			undone.insert(act->getId());
}

char getFirstDependencyless(actionset& actions){
	std::set<char> ids;
	getDependencyless(actions, ids);
	return *ids.begin();
}

void part1(const std::string& path){
	actionset actions;
	getActions(path, actions);

	while(actions.size() > 0){
		char c = getFirstDependencyless(actions);
		action::complete(actions, c);
		std::cout << c;
	}
	std::cout << std::endl;
}

void part2(const std::string& path){
	std::array<worker*, 5> workers;
	for(worker*& w : workers)
		w = new worker();

	actionset actions;
	worker::all = &actions;
	int sec = 0;
	getActions(path, actions);

	while(actions.size()){
		std::set<char> undone;
		getDependencyless(actions, undone);

		for(auto worker : workers){
			auto it = undone.begin();
			while(it != undone.end()){
				action* act = action::find(actions, *it);
				it = worker->addJob(act) ? undone.erase(it) : ++it;
			}
			worker->tick();
		}
		++sec;
	}
	std::cout << sec << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	part1(argv[1]);
	part2(argv[1]);

	return 0;
}
