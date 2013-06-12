#include <stdio.h>

#include <hps.h>

#include "timer.h"

static void usage()
{
	printf(
		"\n"
		"usage: \n"
		"\n"
		"3df_hsf_timer [opts] hsffile\n"
		"Options:\n"
		"	-m		Materials Directory\n"
		);
}

static bool process_args(int argc, char *argv[], const char * &filename, HPS::UTF8 & materials_dir)
{
	if (argc < 2)
	{
		printf("Error: Invalid arg count.\n");
		usage();
		return false;
	}

	int argi = 1;
	while (argi < argc)
	{
		if (argv[argi][0] == '-')
		{
			switch (argv[argi][1])
			{
			case 'm':
				argi++;
				if (argi >= argc)
				{
					printf("Error: Invalid arg count.\n");
					usage();
					return false;
				}
				materials_dir = HPS::UTF8(argv[argi]);
				break;
			}
		}
		else
		{
			break;
		}

		argi++;
	}

	if (argi >= argc)
	{
		printf("Error: Invalid arg count.\n");
		usage();
		return false;
	}
	filename = argv[argi];

	FILE *			fp;

	if (fopen_s(&fp, filename, "r") != 0)
	{
		printf("Error opening file: %s\n", filename);
		return false;
	}
	fclose(fp);

	return true;
}

int main(int argc, char *argv[])
{
	HPS::World						world("license");		// Destructor must be invoked for proper cleanup; thus this needs to be up top

	const char *					filename;
	HPS::UTF8						materialsDir;
	if (!process_args(argc, argv, filename, materialsDir))
		return -1;

	if (materialsDir.IsValid())
		world.SetMaterialDirectory(materialsDir.GetBytes());

	Timer				t;
	if (t.IsValid() == false)
	{
		printf("Error creating timer!\n");
		return -1;
	}

	HPS::StandAloneWindowKey		win = HPS::Database::CreateStandAloneWindow();
	double							ms = -1;

	HPS::Stream::ImportOptionsKit		importOptions;
	HPS::Stream::ImportNotifier notifier;
	importOptions.SetSegment(win);
	try {
		t.Start();
		notifier = HPS::Stream::File::Import(filename, importOptions);
		notifier.Wait();
		ms = t.Milliseconds();
	} catch (HPS::Stream::IOException) {
		fprintf(stderr, "Error occurred reading file: %s\n", filename);
	} catch (HPS::InvalidObjectException e) {
		fprintf(stderr, "Error occurred reading file: %s: %s\n", filename, e.what());
	}

	if (notifier.Status() != HPS::Stream::IOResult::Failure)
		printf("%f", ms);
	else
		fprintf(stderr, "Error occurred reading file: %s\n", filename);

	return 0;
}

