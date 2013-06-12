#ifndef _TIMER_H_

class Timer
{
public:
	Timer();

	bool				IsValid() { return _valid; }
	
	void				Start();
	double				Milliseconds();
	
private:
	bool				_valid;
	__int64				_freq;
	__int64				_start;
};

#endif