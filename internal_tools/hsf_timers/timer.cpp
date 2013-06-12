
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "timer.h"

Timer::Timer()
	: _valid(false), _freq(0), _start(0)
{
	LARGE_INTEGER		freq;

	if (QueryPerformanceFrequency(&freq) != 0)
	{
		_valid = true;
		_freq = freq.QuadPart;
	}
}

void Timer::Start()
{
	LARGE_INTEGER		count;
	
	QueryPerformanceCounter(&count);
	_start = count.QuadPart;
}

double Timer::Milliseconds()
{
	LARGE_INTEGER		count;

	QueryPerformanceCounter(&count);
	return (count.QuadPart - _start) * (1000.0 / _freq);
}

