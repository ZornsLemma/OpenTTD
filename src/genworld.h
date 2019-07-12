/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file genworld.h Functions related to world/map generation. */

#ifndef GENWORLD_H
#define GENWORLD_H

#include "company_type.h"
#include "map_func.h"
#include "strings_type.h"
#include <map>
#include <vector>

/** Constants related to world generation */
enum LandscapeGenerator {
	/* Order of these enums has to be the same as in lang/english.txt
	 * Otherwise you will get inconsistent behaviour. */
	LG_ORIGINAL     = 0,  ///< The original landscape generator
	LG_TERRAGENESIS = 1,  ///< TerraGenesis Perlin landscape generator
};

static const uint GENERATE_NEW_SEED = UINT_MAX; ///< Create a new random seed

/** Modes for GenerateWorld */
enum GenWorldMode {
	GWM_NEWGAME   = 0, ///< Generate a map for a new game
	GWM_EMPTY     = 1, ///< Generate an empty map (sea-level)
	GWM_RANDOM    = 2, ///< Generate a random map for SE
	GWM_HEIGHTMAP = 3, ///< Generate a newgame from a heightmap
};

/** Smoothness presets. */
enum TgenSmoothness {
	TGEN_SMOOTHNESS_BEGIN,      ///< First smoothness value.
	TGEN_SMOOTHNESS_VERY_SMOOTH = TGEN_SMOOTHNESS_BEGIN, ///< Smoothness preset 'very smooth'.
	TGEN_SMOOTHNESS_SMOOTH,     ///< Smoothness preset 'smooth'.
	TGEN_SMOOTHNESS_ROUGH,      ///< Smoothness preset 'rough'.
	TGEN_SMOOTHNESS_VERY_ROUGH, ///< Smoothness preset 'very rough'.

	TGEN_SMOOTHNESS_END,        ///< Used to iterate.
};

static const uint CUSTOM_SEA_LEVEL_NUMBER_DIFFICULTY = 4; ///< Value for custom sea level in difficulty settings.
static const uint CUSTOM_SEA_LEVEL_MIN_PERCENTAGE = 1;    ///< Minimum percentage a user can specify for custom sea level.
static const uint CUSTOM_SEA_LEVEL_MAX_PERCENTAGE = 90;   ///< Maximum percentage a user can specify for custom sea level.

typedef void GWDoneProc();  ///< Procedure called when the genworld process finishes
typedef void GWAbortProc(); ///< Called when genworld is aborted

/** Properties of current genworld process */
struct GenWorldInfo {
	bool abort;            ///< Whether to abort the thread ASAP
	bool quit_thread;      ///< Do we want to quit the active thread
	bool threaded;         ///< Whether we run _GenerateWorld threaded
	GenWorldMode mode;     ///< What mode are we making a world in
	CompanyID lc;          ///< The local_company before generating
	uint size_x;           ///< X-size of the map
	uint size_y;           ///< Y-size of the map
	GWDoneProc *proc;      ///< Proc that is called when done (can be NULL)
	GWAbortProc *abortp;   ///< Proc that is called when aborting (can be NULL)
	class ThreadObject *thread; ///< The thread we are in (can be NULL)
};

/** Current stage of world generation process */
enum GenWorldProgress {
	GWP_MAP_INIT,    ///< Initialize/allocate the map, start economy
	GWP_LANDSCAPE,   ///< Create the landscape
	GWP_RIVER,       ///< Create the rivers
	GWP_RAINFALL_REMOVE_SMALL_BASINS, ///< Rainfall river generator: Remove small basins as preparation step
	GWP_RAINFALL_NUMBER_OF_LOWER,     ///< Rainfall river generator: Number of lower tiles measure
	GWP_RAINFALL_CALCULATE_FLOW,      ///< Rainfall river generator: Flow calculation
	GWP_RAINFALL_MODIFY_FLOW,         ///< Rainfall river generator: modifying flow
	GWP_RAINFALL_DEFINE_LAKES,        ///< Rainfall river generator: defining lakes
	GWP_RAINFALL_PREPARE_WATER,       ///< Rainfall river generator: terraforming for rivers and lakes
	GWP_RAINFALL_WIDER_RIVERS,        ///< Rainfall river generator: wider rivers
	GWP_RAINFALL_LOCAL_TERRAFORM,     ///< Rainfall river generator: local terraforming
   GWP_RAINFALL_BAD_INCLINED,        ///< Rainfall river generator: fixing bad inclined slopes
   GWP_RAINFALL_DERIVE_RIVERS,       ///< Rainfall river generator: derive logical rivers
   GWT_RAINFALL_UPWARDS_RIVERS,      ///< Rainfall river generator: fix upwards rivers
	GWP_RAINFALL_FINETUNING,          ///< Rainfall river generator: final terraforming
	GWP_ROUGH_ROCKY, ///< Make rough and rocky areas
	GWP_TOWN,        ///< Generate towns
	GWP_INDUSTRY,    ///< Generate industries
	GWP_OBJECT,      ///< Generate objects (radio tower, light houses)
	GWP_TREE,        ///< Generate trees
	GWP_GAME_INIT,   ///< Initialize the game
	GWP_RUNTILELOOP, ///< Runs the tile loop 1280 times to make snow etc
	GWP_RUNSCRIPT,   ///< Runs the game script at most 2500 times, or when ever the script sleeps
	GWP_GAME_START,  ///< Really prepare to start the game
	GWP_CLASS_COUNT
};

/* genworld.cpp */
bool IsGenerateWorldThreaded();
void GenerateWorldSetCallback(GWDoneProc *proc);
void GenerateWorldSetAbortCallback(GWAbortProc *proc);
void WaitTillGeneratedWorld();
void GenerateWorld(GenWorldMode mode, uint size_x, uint size_y, bool reset_settings = true);
void AbortGeneratingWorld();
bool IsGeneratingWorldAborted();
void HandleGeneratingWorldAbortion();

/* genworld_gui.cpp */
void SetNewLandscapeType(byte landscape);
void SetGeneratingWorldProgress(GenWorldProgress cls, uint total);
void SetNoTotalGeneratingWorldProgress(GenWorldProgress cls);
void IncreaseGeneratingWorldProgress(GenWorldProgress cls);
void PrepareGenerateWorldProgress();
void ShowGenerateWorldProgress();
void StartNewGameWithoutGUI(uint seed);
void ShowCreateScenario();
void StartScenarioEditor();

extern bool _generating_world;

/** River generators */
enum RiverGeneratorEnum {
	/* Order of these enums has to be the same as in lang/english.txt
	 * Otherwise you will get inconsistent behaviour. */
	RG_ORIGINAL     = 0,  ///< The original river generator
	RG_RAINFALL     = 1,  ///< Rainfall River Generator
};

/* Just a small base class for RiverGenerators. */
struct RiverGenerator {
public:
	virtual ~RiverGenerator() {}

	virtual void GenerateRivers() = 0;
};

/** Definition of a parameter for a town placer.
 */
struct TownPlacerParameter {
	StringID name;
	StringID tooltip;
	int min_value;
	int max_value;
	int default_value;

	TownPlacerParameter() {}
	TownPlacerParameter(const TownPlacerParameter &other) { this->name = other.name; this->tooltip = other.tooltip; this->min_value = other.min_value; this->max_value = other.max_value;
													  this->default_value = other.default_value; }
	TownPlacerParameter(StringID name, StringID tooltip, int min_value, int max_value, int default_value)
				  : name(name), tooltip(tooltip), min_value(min_value), max_value(max_value), default_value(default_value) {}

	inline StringID GetName() { return this->name; }
	inline StringID GetTooltip() { return this->tooltip; }
	inline int GetMinValue() { return this->min_value; }
	inline int GetMaxValue() { return this->max_value; }
	inline int GetDefaultValue() { return this->default_value; }
};

/** Town related score for a rectangular section of the map, e.g. a 16x16 grid.
 */
struct TownScore {
	/** The maximum river flow in the grid. */
	int max_river_flow;

	/** The maximum river flow in the grid divided by the maximum river flow in the neighborhood (grid of size 5*16 tiles in each direction),
	 *  scaled on a range 0...1000.
	 */
	int river_size_score;

	/** The maximum value (flow_one / flow_two) * (branch_flow / maximum_map_flow) for any
	 *  river branch (where two or more rivers join in a tile) in the grid, scaled to a range
	 *  0...1000.
	 *  flow_one and flow_two are the flows of the incoming rivers (switched such that flow_one <= flow_two),
	 *  if more than two rivers merge the two biggest flows, branch_flow is the first flow of the merged river,
	 *  maximum_map_flow is the maximum river flow in the neighborhood (grid of size 5 * 16 tiles in each direction).
	 *
	 *  The idea is that this value is the bigger the more flow the merging rivers have, and the more
	 *  equal-sized the two rivers are.
	 */
	int river_branch_score;

	/** The percentage of lake in the grid, scale to a range 0..1000
	 */
	int lake_amount_score;

	/** The percentage of ocean in the grid, scaled to a range 0...1000
	 */
	int ocean_amount_score;

	/** The size of the biggest lake intersecting the grid.
	  */
	int max_lake_size;

	/** The size of the biggest lake intersecting the grid, divided by the size of the biggest lake on map.
	 */
	int lake_size_score;

	/** The flow of the biggest river ending up in a lake or the ocean, divided by the maximum river flow on
	 *  map, scaled on a range 0...1000
	 */
	int river_end_score;

	/** The amount of SLOPE_FLAT tiles in the grid, divided by the total number of tiles in the grid, scaled
	 *  to a range 0...1000.  River, lake and ocean tiles don´t count.
	 */
	int flat_score;

	/** Minimum heightlevel in the grid.
	 */
	int min_height;

	/** Average heightlevel in the grid.  River, lake and ocean tiles don´t count.  If no other tile is present,
	 *  -1 will be calculated.  Multiplied by 1000 for the sake of getting more detailed values, i.e. average
	 *  heightlevel 2.4 gives 2400.
	 */
	int average_height;

	/** Maximum heightlevel in the grid.
	 */
	int max_height;
};

typedef int TownGridIndex;

static const uint TOWN_GRID_SIZE = 16;
static const uint TOWN_GRID_LOG = 4;

inline TownGridIndex TownGridXY(uint x, uint y) { return y * (MapSizeX() / TOWN_GRID_SIZE) + x; }
inline uint TownGridX(TownGridIndex c) { return c % (MapSizeX() / TOWN_GRID_SIZE); }
inline uint TownGridY(TownGridIndex c) { return c / (MapSizeX() / TOWN_GRID_SIZE); }
inline uint GetNumberOfTownGrids() { return (MapSizeX() * MapSizeY()) / (TOWN_GRID_SIZE * TOWN_GRID_SIZE); }

/** Unique keys for TownPlacers.  Used when (de)serializing the config, and for communication between the GUI components.
 */
enum TownPlacerKey {
	TPK_INVALID = -1,           ///< Constant for "None chosen"
};

/** A TownPlacer evaluates positions for founding a town or town.  Its job is not finding out wether
 *  a particular position is valid for a town or city, but wether the algorithm should found a town or
 *  city on a valid position.
 */
struct TownPlacer {

protected:
	TownPlacerKey key;
	std::map<int, TownPlacerParameter> parameters;
	StringID name;
	StringID description;

public:
	TownPlacer() { this->parameters = std::map<int, TownPlacerParameter>(); }
	virtual ~TownPlacer() {}

	/** Returns the unique key of this town placer.  As it will be serialized to the config, it may not be changed. */
	TownPlacerKey GetKey() { return this->key; }

	/** Returns a map with all parameters of this town placer.  Parameters are identified by the indices used as keys in this map.  */
	std::map<int, TownPlacerParameter> GetParameters() { return this->parameters; }

	/** Returns the name of the TownPlacer, as it is displayed e.g. in drop downs. */
	StringID GetName() { return this->name; }

	/** Returns a description of the TownPlacer, explaining what´s its motivation, and how it might be used. */
	StringID GetDescription() { return this->description; }

	/** Given a TownGridIndex and corresponding score, should we try to found a town somewhere in this grid section?
	 */
	virtual bool PlaceInGridSection(TownGridIndex c, TownScore *score, std::map<int, int> &parameters) = 0;

	/** Given a TileIndex within a town grid section, should we found a town there?
	 */
	virtual bool PlaceAtTile(TownGridIndex c, TownScore *score, TileIndex tile, std::map<int, int> &parameters) = 0;
};

/** Configuration for using a TownPlacer.
 */
struct TownPlacerConfig {
	/** Which town placer to use */
	TownPlacerKey town_placer;

	/** With which weight (this weight, divided by the sum of all weights of all town placers, gives the probability to use it) */
	int weight;

	/** Optionally, with which town placer specific parameters.  The keys of the map are constants, unique within a town placer class.  Something like MIN_FLOW. */
	std::map<int, int> parameter_map;
};

/** Towns and cities are placed in (currently) two phases: First the cities, and then the towns.
 *  The reason to do so is, that the cities should get the chance to choose their locations freely,
 *  before the towns can occupy the good positions on map.
 */
enum TownPlacerPhase {
	TPP_INVALID_PHASE = -1,
	TPP_PHASE_ONE_CITY = 0,
	TPP_PHASE_TWO_TOWN = 1
};

std::vector<TownPlacer*> GetAllTownPlacers();

#endif /* GENWORLD_H */
