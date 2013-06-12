#include "CmdOpts.h"

#include <set>



CmdOpts::CmdOpts ()
	: help(false)
	, numTriangles(0)
	, seed(0)
	, sceneExtent(1.0f)
	, triangleExtent(0.0f)
{}


void CmdOpts::printHelp (std::string const & programName, std::ostream & os) {
	os << programName << "[options]" << std::endl;
	os << "--help: Show this help menu." << std::endl;
	os << "--out FILE: Write hsf to FILE." << std::endl;
	os << "--num-triangles N: Generate N triangles." << std::endl;
	os << "--scene-extent F: How much unit space the scene can cover. Default is 1." << std::endl;
	os << "--triangle-extent F: How much unit space a triangle can cover. A value of 0 ignores this option. Default is 0." << std::endl;
	os << "--seed N: Seed random number generated with N. Default is 0." << std::endl;
}


bool CmdOpts::parse (int argc, char const * const * argv, std::string & errMsg) {
	std::set<std::string> parsedOpts;

	for (int i = 1; i < argc; ++i) {
		if (parsedOpts.find(argv[i]) != parsedOpts.end()) {
			errMsg += "Too many ";
			errMsg += argv[i];
			errMsg += " options";
			return false;
		}
		parsedOpts.insert(argv[i]);

		if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
			help = true;
		}
		else if (strcmp(argv[i], "--out") == 0) {
			filename = argv[++i];
		}
		else if (strcmp(argv[i], "--num-triangles") == 0) {
			if (sscanf(argv[++i], "%u", &numTriangles) != 1) {
				errMsg += "Bad --num-triangles argument";
				return false;
			}
		}
		else if (strcmp(argv[i], "--scene-extent") == 0) {
			if (sscanf(argv[++i], "%f", &sceneExtent) != 1) {
				errMsg += "Bad --scene-extent argument";
				return false;
			}
			if (sceneExtent <= 0.0f) {
				errMsg += "Bad --scene-extent argument";
				return false;
			}
		}
		else if (strcmp(argv[i], "--triangle-extent") == 0) {
			if (sscanf(argv[++i], "%f", &triangleExtent) != 1) {
				errMsg += "Bad --triangle-extent argument";
				return false;
			}
			if (sceneExtent < 0.0f) {
				errMsg += "Bad --triangle-extent argument";
				return false;
			}
		}
		else if (strcmp(argv[i], "--seed") == 0) {
			if (sscanf(argv[++i], "%u", &seed) != 1) {
				errMsg += "Bad --seed argument";
				return false;
			}

		}
		else {
			errMsg += "Unknown option: %s", argv[i];
			return false;
		}
	}

	if (numTriangles == 0) {
		errMsg += "Missing --num-triangles N";
		return false;
	}
	if (filename.empty()) {
		errMsg += "Missing --out FILE";
		return false;
	}
	if (sceneExtent < triangleExtent) {
		errMsg += "Bad --scene-extent must be larger than --triangle-extent";
		return false;
	}

	return true;
}












