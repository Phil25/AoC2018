#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>

enum EventType{
	BeginShift, FallAsleep, WakeUp
};

int dayInMo[13] = {0,31,28,31,30,31,30,31,31,30,31,30,31};

class event{
	int timestamp = 0; // number of minutes since start of year
	int minute = 0;
	int guard = -1;
	EventType type = BeginShift;

	EventType getEventType(const std::string& what){
		if(what[1] == 'G')
			return BeginShift;
		if(what[1] == 'f')
			return FallAsleep;
		return WakeUp;
	}

	int parseGuard(const std::string& what){
		std::istringstream iss(what);
		std::string word;
		iss >> word >> word; // extract "Guard" then "#<num>"
		return std::stoi(word.substr(1));
	}

	int getPassedDays(int mo){
		int acc = 0;
		while(--mo >= 0)
			acc += dayInMo[mo];
		return acc;
	}

	int makeTimestamp(int mo, int d, int h, int m){
		return m + h*60 + d*24*60 + this->getPassedDays(mo)*24*60;
	}

public:
	event(int mo, int d, int h, int m, const std::string& what){
		this->timestamp = this->makeTimestamp(mo, d, h, m);
		this->minute = m;
		this->type = this->getEventType(what);

		if(this->type == BeginShift)
			this->guard = this->parseGuard(what);
	}

	int getTimestamp(){
		return this->timestamp;
	}

	int getGuard(){
		return this->guard;
	}

	EventType getType(){
		return this->type;
	}

	int getMinute(){
		return this->minute;
	}
};

void getEvents(const std::string& data, std::map<int, event*>& events){
	std::ifstream contents(data);
	int mo, d, h, m;
	std::string what;
	event* e = nullptr;

	while(contents >> mo >> d >> h >> m, std::getline(contents, what)){
		e = new event(mo, d, h, m, what);
		events.insert(std::make_pair(e->getTimestamp(), e));
	}
}

void getSleepMinutes(std::map<int, std::vector<std::vector<int>*>>& sleepMinutes, const std::map<int, event*>& events){
	int guard = -1, beganSleep = 0;
	for(auto const& e : events){
		switch(e.second->getType()){
			case BeginShift:
				guard = e.second->getGuard();
				break;

			case FallAsleep:
				beganSleep = e.second->getMinute();
				break;

			case WakeUp:
				int wokenUp = e.second->getMinute();
				std::vector<int>* minuteVec = new std::vector<int>();
				while(--wokenUp >= beganSleep)
					minuteVec->push_back(wokenUp);

				if(sleepMinutes.find(guard) == sleepMinutes.end())
					sleepMinutes.emplace(guard, std::vector<std::vector<int>*>());

				sleepMinutes.at(guard).push_back(minuteVec);
				break;
		}
	}
}

int countSleepMinutes(std::vector<std::vector<int>*> sleepMinutes){
	int acc = 0;
	for(auto const& m : sleepMinutes)
		acc += m->size();
	return acc;
}

int getSleepmostGuard(std::map<int, std::vector<std::vector<int>*>>& sleepMinutes){
	int guard = -1, longestSleep = 0;;
	for(auto const& sleep : sleepMinutes){
		int thisSleepMinutes = countSleepMinutes(sleep.second);
		if(thisSleepMinutes <= longestSleep)
			continue;
		guard = sleep.first;
		longestSleep = thisSleepMinutes;
	}
	return guard;
}

int getSleepmostMinute(int guard, std::map<int, std::vector<std::vector<int>*>>& sleepMinutes){
	int count[60]{0};
	std::vector<std::vector<int>*> guardMinutes = sleepMinutes.at(guard);

	for(auto const& minuteVec : guardMinutes)
		for(auto const& minute : *minuteVec)
			count[minute]++;

	int m = 0, i = 0;
	while(m = count[i] > count[m] ? i : m, ++i < 60);
	return m;
}

void part1(const std::map<int, event*>& events){
	std::map<int, std::vector<std::vector<int>*>> sleepMinutes;
	getSleepMinutes(sleepMinutes, events);

	int guard = getSleepmostGuard(sleepMinutes);
	int minute = getSleepmostMinute(guard, sleepMinutes);

	std::cout << guard * minute << std::endl;
}

void getSleepPerMinute(std::map<int, std::array<int, 60>>& sleepPerMinute, const std::map<int, std::vector<std::vector<int>*>>& sleepMinutes){
	for(auto const& sleep : sleepMinutes){
		std::array<int, 60> minutes{0};
		for(auto const& interval : sleep.second)
			for(auto const& m : *interval)
				minutes[m]++;
		sleepPerMinute.emplace(sleep.first, minutes);
	}
}

void part2(const std::map<int, event*>& events){
	std::map<int, std::vector<std::vector<int>*>> sleepMinutes;
	getSleepMinutes(sleepMinutes, events);

	std::map<int, std::array<int, 60>> sleepPerMinute;
	getSleepPerMinute(sleepPerMinute, sleepMinutes);

	int minute = 0, minuteFreq = 0, guard = -1;
	for(auto const& guardSleepMinute : sleepPerMinute){
		for(int i = 0; i < 60; ++i){
			if(guardSleepMinute.second[i] <= minuteFreq)
				continue;
			minute = i;
			minuteFreq = guardSleepMinute.second[i];
			guard = guardSleepMinute.first;
		}
	}

	std::cout << guard * minute << std::endl;
}

int main(int argc, char* argv[]){
	if(argc < 2){
		std::cerr << "No data provided." << std::endl;
		return 1;
	}

	std::map<int, event*> events = std::map<int, event*>();
	getEvents(argv[1], events);

	part1(events);
	part2(events);

	return 0;
}
