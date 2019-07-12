/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rivers_path.h River calculation based on simulated rainfall. */

#ifndef RIVERS_RAINFALL_H
#define RIVERS_RAINFALL_H

#include "direction_type.h"
#include "landscape_util.h"
#include "slope_func.h"
#include "slope_type.h"
#include "tile_map.h"
#include "tile_type.h"

#include "core/random_func.hpp"

#include <algorithm>
#include <map>
#include <set>
#include <vector>

#include "genworld.h"

/* The following constants define allowed ranges and default values for all
 * configuration options of the Rainfall River Generator.  The documentation
 * here focuses on why some particular bound was chosen, not on how those settings
 * work.
 *
 * The Few / Moderate / Lot of rivers settings only influences flow necessary for river and lakes,
 * and the wider rivers setting.  All other settings affect the look of rivers and lakes,
 * but not in the first place their quantity.
 */

/** Minimal flow necessary to form a river.  Values near to zero are dangerous and thus forbidden
 *  (as they would flood the whole landscape with water.
 */
static const int MIN_FLOW_FOR_RIVER = 10;
static const int DEF_FEW_RIVERS_FLOW_FOR_RIVER = 500;      ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Few Rivers" option.
static const int DEF_MODERATE_RIVERS_FLOW_FOR_RIVER = 150; ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Moderate Rivers" option.
static const int DEF_LOT_RIVERS_FLOW_FOR_RIVER = 50;       ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Lot of Rivers" option.
static const int MAX_FLOW_FOR_RIVER = INT32_MAX;           ///< The more flow is necessary for a river, the less rivers you will get.  High values leading to hardly any rivers are a valid choice.

static const int MIN_FLOW_PER_LAKE_VOLUME = 0;                  ///< Minimum allowed flow needed per lake volume.  Zero means "Generate an outflow for any lake" and thus is a valid choice.
static const int DEF_FEW_RIVERS_FLOW_PER_LAKE_VOLUME = 200;     ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Few Rivers" options.
static const int DEF_MODERATE_RIVERS_FLOW_PER_LAKE_VOLUME = 50; ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Moderate Rivers" options.
static const int DEF_LOT_RIVERS_FLOW_PER_LAKE_VOLUME = 25;      ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Lot of Rivers" options.
static const int MAX_FLOW_PER_LAKE_VOLUME = INT32_MAX;          ///< The more flow is necessary per lake volume, the smaller lakes will be.  High values leading to minimum sized lakes are a valid choice.

static const int MIN_NUMBER_OF_FLOW_MODIFICATIONS = 0;          ///< Flow modifications are an optional step of river generation to make rivers look nicer.  Omitting them is a valid choice.
static const int DEF_NUMBER_OF_FLOW_MODIFICATIONS = 20;         ///< Flow modifications are an optional step of river generation to make rivers look nicer.
static const int MAX_NUMBER_OF_FLOW_MODIFICATIONS = INT16_MAX;  ///< More flow calculations lead to more calculation time, but there is no actual senseful bound.

static const int DEF_FEW_RIVERS_WIDER_RIVERS_ENABLED = 0;       ///< If few rivers should be calculated, they shouldn´t be wider also.
static const int DEF_MODERATE_RIVERS_WIDER_RIVERS_ENABLED = 1;  ///< Generate rivers with really much flow wider, if "Moderate Rivers" are chosen.
static const int DEF_LOT_RIVERS_WIDER_RIVERS_ENABLED = 1;       ///< Generate rivers with really much flow wider, if "Lot of Rivers" are chosen.

static const int MIN_WIDER_RIVERS_MULTIPLIER = 40;              ///< 1 would mean infinitely wide rivers, thus we need at least value two.
static const int DEF_WIDER_RIVERS_MULTIPLIER = 20;              ///< Value 10 means, that for minimum necessary river flow 200, rivers with more than 2000 (20000, 200000) flow are 2 (3, 4) tiles wide.
static const int MAX_WIDER_RIVERS_MULTIPLIER = INT16_MAX;       ///< If the bound is high, no wider rivers will be generated, but this is a valid choice.

static const int DEF_WIDER_VALLEYS_ENABLED = 0;                 ///< Regard this a as an expert setting a user can enable if appropriate for the generated map at hand
static const int MIN_WIDER_VALLEYS_MULTIPLIER = 0;              ///< Negative values make no sense, zero would essentially disable wider valleys
static const int DEF_WIDER_VALLEYS_MULTIPLIER = 5;              ///< Default multiplier on the river width.
static const int MAX_WIDER_VALLEYS_MULTIPLIER = 2048;           ///< Values more than e.g. 10 probably make no sense, but there is no actual bound

static const int MIN_WIDER_VALLEYS_RANDOMNESS = 0;              ///< Minimum value for wider river randomness
static const int DEF_WIDER_VALLEYS_RANDOMNESS = 1000;           ///< Default value for wider rivers randomness
static const int MAX_WIDER_VALLEYS_RANDOMNESS = 65535;          ///< Maximum value for wider river randomness

static const uint MIN_SMALL_OCEANS_REMOVAL_FACTOR = 0;          ///< 0 means turn the feature off, do not remove small oceans
static const uint DEF_SMALL_OCEANS_REMOVAL_FACTOR = 1000;
static const uint MAX_SMALL_OCEANS_REMOVAL_FACTOR = INT32_MAX;

static const uint MIN_SMALL_BASINS_REMOVAL_LIMIT = 0;
static const uint DEF_SMALL_BASINS_REMOVAL_LIMIT = 13;
static const uint MAX_SMALL_BASINS_REMOVAL_LIMIT = 1000;

static const uint MIN_RAINFALL_PROBABILITY = 0;                 ///< Probabilities are scaled in the range 0 ... 1000.
static const uint MAX_RAINFALL_PROBABILITY = 1000;              ///< Probabilities are scaled in the range 0 ... 1000.

static const uint DEF_LAKE_OUTFLOW_CANYON_PROBABILITY = 0;      ///< By default, don´t lower lakes by adding an outflow canyon.
static const uint DEF_LAKE_REDUCE_TO_GUARANTEED_PROBABILITY = 0;///< By default, don´t reduce lakes to their minimum necessary tiles to stay connected
static const uint DEF_LAKE_ISLAND_PROBABILITY = 10;             ///< Default probability that an island is started at some lake tile.
static const uint DEF_LAKE_SHORE_PROBABILITY = 50;              ///< Default probability that shore is expanded starting at a lake shore tile.

static const uint MIN_RAINFALL_MAX_SIZE = 0;                    ///< Fan delta / island / shore sizes generated in a step are arbitrary.
static const uint MAX_RAINFALL_MAX_SIZE = INT16_MAX;            ///< Fan delta / island / shore sizes generated in a step are arbitrary.

static const uint DEF_LAKE_ISLAND_MAX_SIZE = 20;                ///< Default maximum size of an island (as generated in one step, multiple islands can merge with each other and with the shore)
static const uint DEF_LAKE_SHORE_MAX_SIZE = 5;                  ///< Default maximum number of shore tiles generated in one step.

/* The following symbols are for easier maintenance of the debug loggers.  The algorithms of this
 * generator are often heuristic, and thus the question whether the generator works correctly
 * isn´t just a binary one, but rather one of, (1) generate a test map, (2) have a look wether
 * some aspects of the generated rivers and lakes don´t look good, (3) find out why, and how
 * the generator can be improved.  For (3), typically some set of loggers that are sufficient
 * for finding out the major decisions of the generator regarding some aspect, while not
 * overflooding the log is needed.
 *
 * These symbols are for simply turning on and off that loggers.
 */

#define RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL 9

/** Just for Debugging purposes: number_of_lower_tiles array used during river generation, preserved
 *  for displaying it in the map info dialog, in order to provide easily accessible information about
 *  what the algorithm did for which tile.
 */
extern int *_number_of_lower_tiles;

/** ConnectedComponentCalculator for finding all tiles of a basin, i.e. a connected component
 *  of (straight) neighbor tiles that are all below some given heightlevel.
 */
struct BasinConnectedComponentCalculator : public ConnectedComponentCalculator<std::set<TileIndex> > {

private:
	/* Consider tiles smaller than this value */
	int max_height;

protected:
	virtual bool RecognizeTile(std::set<TileIndex> &tiles, TileIndex tile, TileIndex prev_tile);
	inline virtual void StoreInContainer(std::set<TileIndex> &tiles, TileIndex tile) { tiles.insert(tile); }

public:
	BasinConnectedComponentCalculator() : ConnectedComponentCalculator(false) { }
	inline void SetMaxHeight(int max_height) { this->max_height = max_height; }
};

/** This ConnectedComponentCalculator calculates a connected component for use in a NumberOfLowerHeightIterator.
 *  That calculator calculates for each tile on map the number of lower tiles reachable from that tile by going
 *  downwards.  Due to the properties of landscape in OpenTTD, that number in general is somewhat ambiguous,
 *  and in particular not every tile processed by the HeightLevelIterator at that time has already processed
 *  neighbors.
 *
 *  Thus, whenever number-of-lower-tiles calculation reaches a tile whose neighbors aren´t processed
 *  yet, it calculates the connected component of all adjacent tiles that
 *  are equal in terms of height.  The calculation then starts with those tiles of
 *  the connected component which already have calculated neighbors (if none are found, we have found a lake
 *  basin), and proceeds step-wise until it has processed all tiles.
 */
struct NumberOfLowerConnectedComponentCalculator : public ConnectedComponentCalculator<std::set<TileIndex> > {

private:
	/** Recognize only tiles whose height is equal to the given reference height. */
	int ref_height;

	/** Ignore non-coast ocean tiles */
	bool ignore_ocean;

protected:
	/** Returns wether the given Slope is recognized as flat-like by our algorithm, i.e. is either FLAT or with just one corner up. */
	inline bool IsFlatLikeSlope(Slope slope) { return slope == SLOPE_FLAT || slope == SLOPE_N || slope == SLOPE_W || slope == SLOPE_E || slope == SLOPE_S; }

	/** See class comment for the rationale behind this implementation.
	 *  @see ConnectedComponentCalculator.RecognizeTile
	 */
	virtual bool RecognizeTile(std::set<TileIndex> &tiles, TileIndex tile, TileIndex prev_tile);


	/** Put tile into map, and at the same time calculate its number of lower tiles.
     *  @see ConnectedComponentCalculator.StoreInContainer
	 */
	inline virtual void StoreInContainer(std::set<TileIndex> &tiles, TileIndex tile)	{ tiles.insert(tile); }

public:
	NumberOfLowerConnectedComponentCalculator(bool ignore_ocean) { this->ignore_ocean = ignore_ocean; }
	inline void Initialize(int ref_height) { this->ref_height = ref_height; }

	int GetNumberOfLowerNeighborTiles(TileIndex tile);
};

static const int LEVEL_MIN = 0;
static const int LEVEL_FLAT = 0;
static const int LEVEL_ONE_CORNER_RAISED = 1;
static const int LEVEL_TWO_CORNER_RAISED = 2;
static const int LEVEL_THREE_CORNER_RAISED = 3;
static const int LEVEL_STEEP = 4;
static const int LEVEL_MAX = 4;

bool IsSlopeForHeightIterationLevel(Slope slope, int level);

/** HeightLevelIterator that calculates for each tile the number of lower tiles reachable from that tile.
 */
struct NumberOfLowerHeightIterator : public HeightLevelIterator {

private:
	/** This array, calculated by this iterator, contains for each Tile the number of reachable, lower tiles.
	 *  Reachable are tiles where you don´t have to travel upwards to reach them from the tile at hand.
	 *  However, this calculation is somewhat heuristic: If multiple adjacent tiles are just plain,
	 *  we have to decide in a heuristic way, which is below which of its neighbors.
	 *
	 *  The aim of this count is having a basis for deciding where water flows to.  In particular, using
	 *  this count we want to avoid situations where a huge valley has a flat basis, and due to limited
	 *  search radius, we let the water flow upwards the valley instead downwards towards the sea.
	 *
	 *  This count avoids that, by first looking at the tiles which have lower neighbors, and then checking
	 *  their neighbors, and so on...
	 */
	int* number_of_lower_tiles;

	/** If and only if this flag is set, in case no other dirty tiles can be found, the map edge tiles get
	 *  a value of zero lower tiles.  The flag exists, since when removing small basins, we only want
	 *  *one* tile with zero lower tiles per connected component (for each such tile, a connected component
	 *  calculation is started), whereas for the flow calculation, we need the behaviour that the map
	 *  edge gets zero lower tiles.
	 */
	bool set_map_edge_tiles_to_zero;

	/** Not every tile the iterator processes has already processed neighbors (e.g. if we process a big plain).
     *  This connected component calculator comes into play for all tiles where this can happen, and
     *  calculates the whole connected component.  The iterator then starts with those tiles of the connected
     *  component that have already calculated neighbors, and proceeds step-wise towards the other tiles.
     */
	NumberOfLowerConnectedComponentCalculator *connected_component_calculator;

	void StoreNeighborTilesOfProcessedTiles(std::set<TileIndex> &connected_component, std::set<TileIndex> &dirty_tiles, TileIndex neighbor_tiles[DIR_COUNT]);

 protected:
 	virtual void ProcessTile(TileIndex tile, Slope slope);

 public:
 	NumberOfLowerHeightIterator(HeightIndex *height_index, bool set_map_edge_tiles_to_zero);
	~NumberOfLowerHeightIterator();

	/** Returns the array containing the number of reachable lower tiles, calculated by this iterator.
	 *  Note that this array is shared and controlled by this iterator, i.e. do not delete this iterator
	 *  while still using the fetched array.
	 */
	inline int* GetNumberOfLowerTiles()	{ return this->number_of_lower_tiles; }
};

/** A river generator, that generates rivers based on simulating rainfall on each tile
 *  (currently, each tile receives the same rainfall, but this is no must in terms of the algorithm),
 *  and based on this, simulates flow downwards the landscape.  Where enough flow is available,
 *  rivers and lakes will be generated.
 *
 *  Furthermore, the river generator can be configured with various options, resulting in quite different
 *  looking rivers and lakes.
 */
struct RainfallRiverGenerator : public RiverGenerator {

private:
	int *CalculateNumberOfLowerTiles(NumberOfLowerHeightIterator *lower_iterator);
	void RemoveSmallBasins(int *number_of_lower_tiles);

public:
	RainfallRiverGenerator() {}
	virtual void GenerateRivers();
};

#endif /* RIVERS_RAINFALL_H */
