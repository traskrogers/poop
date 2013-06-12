#pragma once

#include <iostream>
#include <string>

#include "Point.h"


class CmdOpts {
public:
	bool help;
	unsigned int numTriangles;
	unsigned int seed;
	std::string filename;
	float sceneExtent;
	float triangleExtent;
public:
	CmdOpts ();
	bool parse (int argc, char const * const * argv, std::string & errMsg);
	static void printHelp (std::string const & programName, std::ostream & os);
};




