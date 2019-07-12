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
#include "terraform_func.h"
#include "tile_map.h"
#include "tile_type.h"

#include "core/random_func.hpp"
#include "pathfinder/npf/aystar.h"

#include <algorithm>
#include <map>
#include <set>
#include <vector>

#include "genworld.h"
#include "debug.h"

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
#define RAINFALL_CALCULATE_FLOW_LOG_LEVEL 9
#define RAINFALL_DEFINE_LAKES_LOG_LEVEL 9
#define RAINFALL_CONSUME_LAKES_LOG_LEVEL 9
#define RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL 9
#define RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL 9
#define RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL 9
#define RAINFALL_FINETUNE_TILES_FULL_LOG_LEVEL 9
#define RAINFALL_FLOW_MODIFICATION_LOG_LEVEL 9
#define RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL 9
#define RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL 9
#define RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL 9

/** Just for Debugging purposes: number_of_lower_tiles array used during river generation, preserved
 *  for displaying it in the map info dialog, in order to provide easily accessible information about
 *  what the algorithm did for which tile.
 */
extern int *_number_of_lower_tiles;
extern int *_water_flow;
extern byte *_water_info;

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

struct DefineLakesIterator;

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

    void ReInit(bool set_map_edge_tiles_to_zero);

	/** Returns the array containing the number of reachable lower tiles, calculated by this iterator.
	 *  Note that this array is shared and controlled by this iterator, i.e. do not delete this iterator
	 *  while still using the fetched array.
	 */
	inline int* GetNumberOfLowerTiles()	{ return this->number_of_lower_tiles; }
};

/** The kind of water as stored in the water_info array, bits 3 to 5.  The default is no water,
 *  the other values indicate what the algorithm wants to do with some tile at hand.
 */
enum WaterType {
	/** No water. */
	WI_NONE = 0,

	/** Water disappearing at the map edge.
	 */
	WI_MAP_EDGE_DISAPPEAR = 1,

	/**  Tile having a visible river.
	 */
	WI_RIVER       = 2,

	/**  Any non-center tile of a lake.  Generally, it must not be higher than the surface height of the Lake.  If the lake spreads
     *   over rivers flowing into the lake, they are declard WI_LAKE.
	 */
	WI_LAKE        = 3,

	/**  Center tile of a lake.  The tile, where the flow algorithm did not manage to find any tile suitable for an out-flow from this tile.
	 *   Based on this tile, the lake algorithm will lateron start producing a lake (if there is enough water).
	 *   NOTE: Thus, the direction of a lake center is undefined.
	 *   For each Lake object, there is exactly one active lake center.  The flow of the active lake center is the flow of the Lake.
	 *   If two lakes are merged (because one of them receives enough water to grow till the other one), the lake center of the
	 *   consumed lake is declared a consumed lake center, only the center of the consuming lake stays active.
	 */
	WI_ACTIVE_LAKE_CENTER = 4,

	/**  Center of a Lake that was consumed by another Lake.  Still indicates that at this point, flow cannot flow any further downwards.
	 *   However, any Lake related calculations that find this tile must delegate to the corresponding active lake center.
     *   (which is possible as the DefineLakesIterator stores a map from lake centers to Lake instances).
     */
	WI_CONSUMED_LAKE_CENTER = 5
};

/** Returns the kind of water the algorithm intends to generate for some tile.
 *  @param water_info array
 *  @param tile some tile
 *  @return the kind of water the algorithm intends to generate for some tile.
 *  @see enum WaterType
 */
inline WaterType GetWaterType(byte *water_info, TileIndex tile) { return (WaterType)GB(water_info[tile], 3, 3); }

/** Sets the kind of water the algorithm wants to generate at some tile.
 *  @param water_info array
 *  @param tile some tile
 *  @param water_type intended kind of water
 *  @see enum WaterType
 */
inline void SetWaterType(byte *water_info, TileIndex tile, WaterType water_type) { SB(water_info[tile], 3, 3, water_type); }

TileIndex AddFlowDirectionToTile(TileIndex tile, byte water_info);
TileIndex AddDirectionToTile(TileIndex tile, Direction direction);
TileIndex GetLakeCenterForTile(TileIndex tile, byte *water_info);
byte GetWaterInfoDirection(TileIndex source, TileIndex dest);

/** Returns the flow direction at the given tile.
 *  @param water_info water info array
 *  @param tile some tile
 *  @return flow direction
 */
inline Direction GetFlowDirection(byte *water_info, TileIndex tile) { return (Direction)GB(water_info[tile], 0, 3); }

/** Returns what the algorithm wants to do with some tile.  This includes kind of water and flow direction; not the flow itself.
 *  @param water_info array
 *  @param tile some tile
 *  @return what the algorithm wants to do with some tile; bits 0..2 direction, 3..5 WaterType
 */
inline byte GetWaterInfo(byte *water_info, TileIndex tile) { return water_info[tile]; }

/** Sets the state what the algorithm wants to do with some tile.
 *  @param water_info array
 *  @param tile some tile
 *  @param value bits 0..2 direction, 3..5 WaterType
 */
inline void SetWaterInfo(byte *water_info, TileIndex tile, byte value) { water_info[tile] = value; }

/** Returns wether the given tiles is not declared to become any water.
 *  @return wether the given tiles is not declared to become any water.
 */
inline bool IsNoWater(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_NONE; }

/** Declares the given tile no water.
 *  @param water_info water info array
 *  @param tile some tile
 */
inline void DeclareNoWater(byte *water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_NONE); }

/** Declares the given tile an active lake center.
 *  @param water_info array
 *  @param tile some tile
 *  @see WI_ACTIVE_LAKE_CENTER
 */
inline void DeclareActiveLakeCenter(byte* water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_ACTIVE_LAKE_CENTER); }

/** Returns wether the given tile is declared an active lake center.
 *  @param water_info array
 *  @param tile some tile
 *  @return wether the given tile is declared an active lake center.
 *  @see WI_ACTIVE_LAKE_CENTER
 */
inline bool IsActiveLakeCenter(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_ACTIVE_LAKE_CENTER; }

/** Declares the given tile a consumed lake center.
 *  @param water_info array
 *  @param tile some tile
 *  @see WI_CONSUMED_LAKE_CENTER
 */
inline void DeclareConsumedLakeCenter(byte* water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_CONSUMED_LAKE_CENTER); }

/** Returns wether the given tile is declared a consumed lake center.
 *  @param water_info array
 *  @param tile some tile
 *  @return wether the given tile is declared a consumed lake center.
 *  @see WI_CONSUMED_LAKE_CENTER
 */
inline bool IsConsumedLakeCenter(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_CONSUMED_LAKE_CENTER; }

/** Returns wether the given tile is a lake center (active or consumed).
 *  @param water_info array
 *  @param tile some tile
 *  @return wether the given tile is a lake center (active or consumed).
 *  @see WI_ACTIVE_LAKE_CENTER, WI_CONSUMED_LAKE_CENTER
 */
inline bool IsLakeCenter(byte *water_info, TileIndex tile) { return IsActiveLakeCenter(water_info, tile) || IsConsumedLakeCenter(water_info, tile); }

/** Declares the given tile a non-center tile of a lake.
 *  @param water_info array
 *  @param tile some tile
 *  @see WI_LAKE
 */
inline void DeclareOrdinaryLakeTile(byte *water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_LAKE); }

/** Returns wether the given tile is a non-center tile of a lake.
 *  @param water_info array
 *  @param tile some tile
 *  @return wether the given tile is a non-center tile of a lake.
 *  @see WI_LAKE
 */
inline bool IsOrdinaryLakeTile(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_LAKE; }

/** Declares the given tile a tile where flow disappears because the map edge is reached.
 *  @param water_info array
 *  @param tile some tile
 *  @see WI_MAP_EDGE_DISAPPEAR
 */
inline void DeclareDisappearTile(byte *water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_MAP_EDGE_DISAPPEAR); }

/** Returns wether the given tile is a tile where flow disappears because the map edge is reached.
 *  @param water_info array
 *  @param tile some tile
 *  @see WI_MAP_EDGE_DISAPPEAR
 *  @return wether the given tile is a tile where flow disappears because the map edge is reached.
 */
inline bool IsDisappearTile(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_MAP_EDGE_DISAPPEAR; }

inline void DeclareRiver(byte *water_info, TileIndex tile) { SetWaterType(water_info, tile, WI_RIVER); }
inline bool IsRiver(byte *water_info, TileIndex tile) { return GetWaterType(water_info, tile) == WI_RIVER; }

inline bool IsWaterTile(byte *water_info, TileIndex tile) { return IsRiver(water_info, tile) || IsLakeCenter(water_info, tile) || IsOrdinaryLakeTile(water_info, tile); }

inline void MarkGuaranteed(byte *water_info, TileIndex tile) { SetBit(water_info[tile], 6); }
inline bool IsGuaranteed(byte *water_info, TileIndex tile) { return GB(water_info[tile], 6, 1); }

inline void MarkProcessed(byte *water_info, TileIndex tile) { SetBit(water_info[tile], 7); }
inline void MarkNotProcessed(byte *water_info, TileIndex tile) { ClrBit(water_info[tile], 7); }
inline bool WasProcessed(byte *water_info, TileIndex tile) { return GB(water_info[tile], 7, 1); }

/** This HeightLevelIterator is responsible for calculating the flow.  Flow is meant to flow from higher towards
 *  lower tiles, and not in cycles.  Usually, in flow direction flow increases, but later code
 *  (flow modificators) may break that condition.  Flow calculation relies on the previously calculated
 *  number of lower tiles, which serves as basis of the following calculation.  By flowing towards tiles
 *  with a smaller number of lower tiles, flow can find the sea and doesn´t e.g. flow a big flat valley
 *  upwards.
 *
 *  Lateron, flow is used for determining where rivers are (e.g. if more than x flow on a tile), and for determinig
 *  how much water should be filled into a lake.
 */
struct CalculateFlowIterator : public HeightLevelIterator {

private:

	/** Tile-indexed array, for each Tile containing the number of lower tiles as calculated by the NumberOfLowerHeightIterator */
	int *number_of_lower_tiles;

	/** Tile-indexed array, for each Tile containing the water flow.  Will be calculated by this iterator. */
	int *water_flow;

	/** Tile-indexed array, for each Tile containing some detail information about the water flow.  One byte is built up as follows:
	 *    Bits 0..2: Direction of water flow, see enum Direction
	 *    Bits 3..5: See enum WaterInfo.
	 */
	byte *water_info;

	NumberOfLowerConnectedComponentCalculator *connected_component_calculator;

	void StoreNeighborTilesOfProcessedTiles(std::set<TileIndex> &base_set, std::set<TileIndex> &dirty_tiles, TileIndex neighbor_tiles[DIR_COUNT]);

protected:

	virtual void ProcessTile(TileIndex tile, Slope slope);

public:

	CalculateFlowIterator(HeightIndex *height_index, int *number_of_lower_tiles);
	~CalculateFlowIterator();

	/** Returns the array containing the water flow, indexed by Tiles, as calculated by this Iterator.
	 *  Note that this array is shared and controlled by this iterator, i.e. do not delete this iterator
	 *  while still using the fetched array.
	 */
	inline int* GetWaterFlow() { return this->water_flow; }

	/** Returns the array containing the additional water flow information, indexed by Tiles, as calculated by this Iterator.
	 *  Note that this array is shared and controlled by this iterator, i.e. do not delete this iterator
	 *  while still using the fetched array.
	 */
	inline byte* GetWaterInfo() { return this->water_info; }
};

/** FlowModificators modify an already calculated flow on the small scale.  They are necessary, because
 *  the NumberOfLowerTilesHeightIterator and CalculateFlowIterator above tend to calculate flow that
 *  proceeds to the sea / to some lake as fast as possible.  Thus, their raw result doesn´t look like
 *  real world rivers.
 *
 *  A FlowModificator is meant to be called a lot of times in a row.  In each step, it chooses some tile
 *  along a river, and redirects flow starting at that tile on the small scale.  E.g., it might change
 *  flow originally flowing straight to the sea to flow three tiles to the left, two to the right, and
 *  then it proceeds straight.
 *
 *  Note that FlowModificators always have to preserve correctness of the calculated flow, i.e. they
 *  may not produce any cycle, nor may the flow flow upwards afterwards.  In fact, as they are called
 *  many times in a row, aborting a particular modification because it would lead to an incorrect result
 *  is a perfectly usual step in such an algorithm.
 */
struct FlowModificator {

protected:
	int *water_flow;
	byte *water_info;

public:
	FlowModificator(int *water_flow, byte *water_info) { this->water_flow = water_flow;	this->water_info = water_info; }

	virtual ~FlowModificator() {}

	/** Modifies flow at some random place of map.  What exactly this means
     *  is subclass-specific.  As this method is meant to be called
     *  many times in a loop, the changes should be rather local, limited ones.
	 *  @return wether none of the invariants of flow modification have been violated
     */
	virtual bool ModifyFlow(int min_equal_directions) = 0;
};

/** A CurveFlowModificator modifies flow by letting it flow along simulated paths that sometimes change direction.
 *  To achieve this, in any step of a CurveFlowModificator run, it maintains an current angle, and a phase.
 *  The phase can be left, right or straight.
 *
 *  For example: A run may look as follows:
 *  Start with angle 90 degrees (i.e., south-east).  Switch to phase left. Go one step south-east.  Decrease
 *  angle by 34 degrees to 56 degrees, as we go left.  Go one step east, as 56 is nearer to 45 than to 90.
 *  Decrease angle by another 29 degrees to 27 degrees.  Go one step south-east, as the actually desired
 *  direction east would end in going upwards, thus south-east is the next-better direction.  Decrease
 *  angle by another 44 degrees to -17 degrees.  Go one step north-east, and change phase to straight.
 *
 *  Now, in phase straight, angle is sometimes decreased and sometimes increased, to get a roughly straight
 *  path, but with small random curves to the left or to the right.  Phase right, would finally mean
 *  increasing the angle.
 *
 *  The path calculated using the algorithm above ignores flow and number of lower tiles completely.  Thus,
 *  there is always some chance that we introduce a cycle.  This happens e.g. if the path finds an inflow tile
 *  of the start tile.
 *
 *  To cope with that, the above algorithm is performed for a random number of steps, or until absolutely no tile
 *  for proceeding can be found, or until there is a cycle in the new path itself.  Then, a postprocessing
 *  step proceeds along the path, redirects flow for each of the tiles (i.e. subtract flow of the current tile
 *  from all tiles on the old path, and add it for all tiles of the new path), and at the same time
 *  checks wether there is any cycle.
 *
 *  Finally flow modification until the last tile that doesn´t introduce a cycle is performed.
 *
 *  Once this algorithm is run some thousand times for a map of e.g. 512x512, the originally straight rivers have
 *  turned into quite real-world-like curvaceous ones.
 *
 *  Note that although this algorithm calculates river paths of limited length, it would not be suitable
 *  for replacing the number of lower tiles based initial calculation.  Simply because its behaviour
 *  in a huge plain or a huge flat valley is a purely local one, i.e. it steps somewhere, somewhat predetermined
 *  by the flow direction at its start tile, but hasn´t any knowledge wether its path goes down the valley or up.
 */
struct CurveFlowModificator : public FlowModificator {

private:
	enum Phase {
		PHASE_STRAIGHT = 0,
		PHASE_LEFT = 1,
		PHASE_RIGHT = 2
	};

	const char* PhaseToString(Phase phase);
	bool IsSuitableFlowTile(TileIndex tile);

	TileIndex FindStartTile(int min_equal_directions);
	bool FinishFlowModification(TileIndex tile);
	TileIndex GetNextTile(TileIndex tile, int angle);

public:
	CurveFlowModificator(int *water_flow, byte *water_info);
	virtual bool ModifyFlow(int min_equal_directions);
};

struct LakeDefinitionRun;

/** Class storing all necessary information about an already calculated lake.  Instances of this class are constructed
 *  in the DefineLakesIterator.
 */
struct Lake {
	/** The center tile of the lake, i.e. the only active lake center within the lake.
     */
	TileIndex lake_center;

	/** Lakes in terms of this algorithm have a vertical dimension.  The more water flows into a lake
	 *  filling some basin, the higher water level ascends.  Lateron, we will level terrain to that
	 *  level, as lakes currently have no depth in OpenTTD.  The value stored here is the currently
     *  generated heightlevel of a lake surface, in terms of heightlevels.
	 */
	int surface_height;

	/** The amount of flow, which for this lake was already processed as outflow, i.e. left the lake.
	 *  The point is, that if when the lake is processed first, not enough water enters for making an
	 *  outflow at all, this variable remains to be zero, and maybe lateron, all outflow has to be
	 *  processed at once.  On the other hand, we *must* avoid situations where we process
	 *  the same outflow twice.
	 */
	int already_processed_outflow;

	/** The amount of flow that was already spent in this lake for adding tiles to the lake. */
	int already_spent_flow;

	/** If this flag is set, the Lake was finished without finding an outflow.  The only case where this
	 *  can happen is if the Lake hits the map border.
	 */
	bool finished_without_outflow;

	/** The outflow tile of the lake, if it has one, or INVALID_TILE else.  The out-flow tile is a tile,
	 *  where flow leaves the lake, and ends up at the sea, at the map edge, or in another lake.
     */
	TileIndex outflow_tile;

	/** During construction of the lake, this stores the tiles where we may expand the lake.  Stored in
     *  Lake, as we may come back to a lake multiple times, and want to proceed with the edge we
     *  calculated last time if doing so.  After we have found an outflow, this set is not of any interest
     *  any longer.
	 *
	 *  Note: This set only contains edge tiles at the current surface height.  When they are exhausted, and
	 *  not outflow is found yet, the tiles for the next level have to be recalculated.
     */
	std::set<TileIndex> unprocessed_edge_tiles;

	/** The unprocessed edge tiles are dirty, and need to be recalculated before the Lake is enlarged next time.
	 *  This happens, if parts of the lake, but not (yet) the lake center are consumed by another lake.
	 *  Then we cannot be sure that the unprocessed edge tiles make sense any longer, but as recalculation comes
	 *  at some cost, we want to delay it until actually needed.
	 */
	bool unprocessed_edge_tiles_dirty;

	/** Stores all tiles of this lake. This may on the one hand cost some memory, on the other hand, lake
	 *  construction cannot work such that when determining lake extent via the lake connected component,
	 *  never accidentally two lakes that are in fact different, maybe with different surface heights,
	 *  are merged.  Thus, storing lake extent explicitely instead of recalculating it whenever needed
	 *  is the safer approach.
	 */
	std::set<TileIndex> lake_tiles;

	Lake(TileIndex lake_center);

	/** Returns the center tile of the lake, i.e. the only active lake center of the lake.
	 *  @return the center tile of the lake.
	 */
	inline TileIndex GetCenterTile() { return this->lake_center; }

	/** Sets the surface height of the lake, i.e. the height to which it will be terraformed when actually generating the water.
	 *  @param surface_height the new surface height
	 */
	inline void SetSurfaceHeight(int surface_height) { this->surface_height = surface_height; }

	/** Returns the surface height of the lake, i.e. the height to which it will be terraformed when actually generating the water.
	 */
	inline int GetSurfaceHeight() { return this->surface_height; }

	/** Sets the already processed outflow of the lake
	 *  @param already_processed_outflow the already processed outflow of the lake
	 *  @see Lake::already_processed_outflow for detail information about why this information is needed
	 */
	inline void SetAlreadyProcessedOutflow(int already_processed_outflow) { this->already_processed_outflow = already_processed_outflow; }

	/** Returns the already processed outflow of the lake
	 *  @param the already processed outflow of the lake
	 *  @see Lake::already_processed_outflow for detail information about why this information is needed
	 */
	inline int GetAlreadyProcessedOutflow() { return this->already_processed_outflow; }

	inline void SetAlreadySpentFlow(int already_spent_flow) { this->already_spent_flow = already_spent_flow; }
	inline int GetAlreadySpentFlow() { return this->already_spent_flow; }

	/** Returns wether the lake was finished without finding an outflow (i.e. if it hit the map border).
	 */
	inline bool WasFinishedWithoutOutflow() { return this->finished_without_outflow; }

	/** Sets the outflow tile of the lake.  Flow starting at this tile *must* end up somewhere outside the lake.
	 *  It is typically not part of the lake.
	 *  @param outflow_tile the outflow tile
	 */
	inline void SetOutflowTile(TileIndex outflow_tile) { this->outflow_tile = outflow_tile; }

	/** Returns the outflow tile of the lake. Flow starting at this tile ends up somewhere outside the lake.
	 *  It is typically not part of the lake.
	 *  @return the outflow tile of the lake
	 */
	inline TileIndex GetOutflowTile() { return this->outflow_tile; }

	/** Returns the number of remaining unprocessed edge tiles.
	 */
	inline uint GetNumberOfUnprocessedEdgeTiles() { return this->unprocessed_edge_tiles.size(); }

	/** Registers the given tile as unprocessed edge tile.
	 *  @param tile some tile
	 *  @see Lake::unprocessed_edge_tiles for more information
	 */
	inline void RegisterUnprocessedEdgeTile(TileIndex tile) { this->unprocessed_edge_tiles.insert(tile); }

	/** Unregisters the given tile from the unprocessed edge tiles, typically because it has become a lake tile.
	 *  @param tile some tile
	 *  @see Lake::unprocessed_edge_tiles for more information
	 */
	inline void UnregisterUnprocessedEdgeTile(TileIndex tile) { this->unprocessed_edge_tiles.erase(tile); }

	/** Removes all unprocessed edge tiles.
	 *  @see Lake::unprocessed_edge_tiles for more information
	 */
	inline void ClearUnprocessedEdgeTiles() { this->unprocessed_edge_tiles.clear(); }

	inline void SetUnprocessedEdgeTilesDirty(bool unprocessed_edge_tiles_dirty) { this->unprocessed_edge_tiles_dirty = unprocessed_edge_tiles_dirty; }
	inline bool AreUnprocessedEdgeTilesDirty() { return this->unprocessed_edge_tiles_dirty; }

	/** Returns the begin() const iterator of the unprocessed edge tiles.
	 *  @return the begin() const iterator of the unprocessed edge tiles.
	 */
	inline std::set<TileIndex>::const_iterator GetUnprocessedEdgeBegin() { return this->unprocessed_edge_tiles.begin(); }

	/** Returns the end() const iterator of the unprocessed edge tiles.
	 *  @return the end() const iterator of the unprocessed edge tiles.
	 */
	inline std::set<TileIndex>::const_iterator GetUnprocessedEdgeEnd() { return this->unprocessed_edge_tiles.end(); }

	/** Returns wether the given tile is a tile of this lake.
     *  @param tile some tile
	 *  @return wether the given tile is a tile of this lake.
	 */
	inline bool IsLakeTile(TileIndex tile) { return this->lake_tiles.find(tile) != this->lake_tiles.end(); }

	/** Adds the given tile to the lake.
	 *  @param tile some tile
	 */
	void AddLakeTile(TileIndex tile);

	/** Removes the given tile from the lake.
	 *  @param tile some tile
	 */
	inline void RemoveLakeTile(TileIndex tile) { this->lake_tiles.erase(tile); }

	/** Returns the number of tiles of the lake.
	 *  @return the number of tiles of the lake.
	 */
	inline int GetNumberOfLakeTiles() { return this->lake_tiles.size(); }

	/** Removes all tiles from the lake.
	 */
	inline void ClearLakeTiles() { this->lake_tiles.empty(); }

	/** Returns the begin() const iterator for the lake tiles.
	 *  @return the begin() const iterator for the lake tiles.
	 */
	inline std::set<TileIndex>::const_iterator GetLakeTilesBegin() { return this->lake_tiles.begin(); }

	/** Returns the end() const iterator for the lake tiles.
	 *  @return the end() const iterator for the lake tiles.
	 */
	inline std::set<TileIndex>::const_iterator GetLakeTilesEnd() { return this->lake_tiles.end(); }

	/** Returns the set of lake tiles.  In fact, we want to hide it using the upper functions,
	 *  but for calculating a path within a lake, the direct reference is needed.
	 *  @return the set of lake tiles
	 */
	inline std::set<TileIndex>* GetLakeTiles() { return &this->lake_tiles; }

	void RemoveHigherLakeTiles(int max_height, int *water_flow, byte *water_info, std::map<TileIndex, Lake*> &tile_to_lake);

	void PrintToDebug(int level, int detailLevel);
};

/** ConnectedComponentCalculator for finding all tiles of a lake (the DefineLakesIterator declared lake tiles WI_LAKE,
 *  but did not produce an explicit list of the tiles of some particular lake).
 */
struct LakeConnectedComponentCalculator : public ConnectedComponentCalculator<std::set<TileIndex> > {

private:
	byte *water_info;
	int max_height;
	Lake *excluded_lake;

protected:
	inline virtual bool RecognizeTile(std::set<TileIndex> &tiles, TileIndex tile, TileIndex prev_tile)
	{
		return !excluded_lake->IsLakeTile(tile) && tiles.find(tile) == tiles.end() && (IsLakeCenter(water_info, tile) || IsOrdinaryLakeTile(water_info, tile)) && GetTileZ(tile) <= max_height;
	}

	inline virtual void StoreInContainer(std::set<TileIndex> &tiles, TileIndex tile) { tiles.insert(tile); }

public:
	LakeConnectedComponentCalculator(byte *water_info) : ConnectedComponentCalculator(false) { this->water_info = water_info;	}

	inline void SetMaxHeight(int max_height) { this->max_height = max_height; }
	inline void SetExcludedLake(Lake *excluded_lake) { this->excluded_lake = excluded_lake; }
};

struct LakeDefinitionState;

/** Information about processing one particular Lake during lake definition.
 *  @see LakeDefinitionState
 */
struct LakeDefinitionRun {

	Lake *lake;        ///< The Lake
	int extra_flow;    ///< The flow added newly to the Lake in this step

	LakeDefinitionRun(Lake *lake, int extra_flow) : lake(lake), extra_flow(extra_flow) {}

	inline Lake *GetLake() { return this->lake; }
	inline int GetExtraFlow() { return this->extra_flow; }
};

/** During one run of lake definition, flow is propagated from some lake center up in the mountains,
 *  through possibly several other lake centers, until the sea is reached.  The LakeDefinitionState
 *  stores the information that needs to be propagated along that path (e.g., was a lake already
 *  processed?, to avoid cycles).
 */
struct LakeDefinitionState {
	/** Each LakeDefinitionRun stores information about processing one particular Lake along the path described above.
	 */
	std::vector<LakeDefinitionRun> runs;

	/** Set of already processed lake centers in this run of lake definition.
	 */
	std::set<TileIndex> processed_lake_centers;

	/** The minimum surface height we have seen so far in this particular run of lake definition.
	 */
	int min_surface_height;

	LakeDefinitionState() : runs(std::vector<LakeDefinitionRun>()), processed_lake_centers(std::set<TileIndex>()), min_surface_height(-1) {}
	void StartRun(LakeDefinitionRun &run);
	int WasPreviousLakeLower();

	inline void SetMinSurfaceHeight(int min_surface_height) { this->min_surface_height = min_surface_height; }
	inline void DropMinSurfaceHeight() { this->min_surface_height = -1; }
	inline int GetMinSurfaceHeight() { return this->min_surface_height; }

	inline bool WasAlreadyProcessed(TileIndex tile) { return this->processed_lake_centers.find(tile) != this->processed_lake_centers.end(); }
	inline void MarkProcessed(TileIndex tile) { this->processed_lake_centers.insert(tile); }
	inline void MarkUnprocessed(TileIndex tile) { this->processed_lake_centers.erase(tile); }
};

/** The CalculateFlowIterator just defines tiles where water ends without immediate possibility for continuing flow LAKE_CENTERs.
 *  This iterator calculates lakes around these tiles.  If an outflow tile can be found within the bounds given by the inflow,
 *  flow along the outflow path is increased.  Lakes that are found at the end of an outflow path are calculated immediately
 *  in a recursive call, thus this iterator doesn´t purely calculate top-down.  But this is no harm, as a lake can only
 *  grow when calculated for the second time (e.g. because it gains the water of another outflow of another lake), never
 *  shrink.  And only shrinking would be a problem, as tiles are declared WI_LAKE tiles immediately.
 */
struct DefineLakesIterator : public HeightLevelIterator {

private:
	int create_lake_runs = 0;

	LakeConnectedComponentCalculator *lake_connected_component_calculator;

	int *number_of_lower_tiles;

	int *water_flow;

	byte *water_info;

	/** Map from lake tiles to Lake.  In a first version of this code, only lake centers were
     *  stored here, but experience with running the algorithms indicated that storing all lake
	 *  tiles, and thus being able to consume lakes without having to calculate a connected
	 *  component is the better decision.
	 */
	std::map<TileIndex, Lake*> tile_to_lake;

	/** Helper vector just for the purpose of properly deleting all Lake instances we ever created.
	 *  (entries of the map tile_to_lake will be modified by mapping tiles to other Lake instances,
	 *   thus this method of avoiding memory leaks is the safest and simplest one.
	 */
	std::vector<Lake*> all_lakes;

	void DiscardLakeNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int ref_height);
	bool DoesFlowEndOutsideLake(TileIndex tile, Lake *lake, LakeDefinitionState &state);
	Lake* GetDestinationLake(TileIndex tile);
	bool DoesOutflowHitLake(Lake *other_lake, Lake *lake);
	int ConsumeLake(Lake *lake, TileIndex other_lake_tile, int max_height, LakeDefinitionState &state, LakeDefinitionRun &run);
	void RecalculateUnprocessedEdgeTiles(Lake *lake);
	void TerraformPathsFromCentersToOutflow(Lake *lake, TileIndex outflow_tile, int desired_height);
	void RegisterAppropriateNeighborTilesAsUnprocessed(Lake *lake, TileIndex tile, int ref_height);

protected:

	virtual void ProcessTile(TileIndex tile, Slope slope);

public:

	DefineLakesIterator(HeightIndex *height_index, int *number_of_lower_tiles, int *water_flow, byte *water_info);
	~DefineLakesIterator();
	void CreateLake(TileIndex tile, LakeDefinitionState &state, int extra_flow = 0);

	inline bool HasSurfaceHeightForLake(TileIndex lake_center) { return this->tile_to_lake.find(lake_center) != this->tile_to_lake.end(); }
	inline int GetSurfaceHeightForLake(TileIndex lake_center) {	return this->tile_to_lake[lake_center]->GetSurfaceHeight(); }
	inline Lake* GetLake(TileIndex tile) { return this->HasLakeTile(tile) ? this->tile_to_lake[tile] : NULL; }

	/** Registers a tile to lake mapping in the internal map.
	 *  Only needed in special cases lake fine tuning tiles to meet slopes.  The main lake related
	 *  work is done in private functions of this class.
	 */
	inline void RegisterLakeTile(TileIndex tile, Lake *lake) { this->tile_to_lake[tile] = lake; }

	inline bool HasLakeTile(TileIndex tile) { return this->tile_to_lake.find(tile) != this->tile_to_lake.end(); }

	inline void SetLakeCenterMapping(TileIndex tile, Lake *lake) { this->tile_to_lake[tile] = lake; }
	inline void EraseLakeCenterMapping(TileIndex tile) { this->tile_to_lake.erase(tile); }
	inline void AddToFlow(TileIndex tile, int offset) { this->water_flow[tile] += offset; }
	inline void SetWaterInfo(TileIndex tile, byte value) { this->water_info[tile] = value; }
};

/* Helper class to place both (desired) height and flow of a tile in a map.
 */
struct HeightAndFlow {
	int height;
	int flow;

	/** Default constructor, to make it usable as value in a std::map.
	 */
	HeightAndFlow() {}

	HeightAndFlow(int height, int flow) { this->height = height; this->flow = flow; }
};

/** Helper class for sorting tiles appropriately for potential terraforming.
 */
struct TileWithHeightAndFlow {
	TileIndex tile;
	uint height;
	int flow;

	TileWithHeightAndFlow(TileIndex tile, uint height, int flow) { this->tile = tile; this->height = height; this->flow = flow; }

	bool operator < (const TileWithHeightAndFlow& other) const
    {
		if (this->height == other.height) {
			return this->flow > other.flow;
		} else {
			return this->height < other.height;
		}
    }
};

struct TileWithTerraformerState {
	TileIndex tile;
	TerraformerState terraformer_state;
	std::set<TileIndex> processed_tiles;

	TileWithTerraformerState(TileIndex tile, TerraformerState terraformer_state, std::set<TileIndex> processed_tiles) : tile(tile),
									terraformer_state(terraformer_state), processed_tiles(processed_tiles) {}
};

/** LakeModificators are called from RainfallRiverGenerator::PrepareLake, and have the task of making lakes nicer,
 *  before their tiles are actually declared water.  For details, see the extensive comment of function ModifyLake.
 */
struct LakeModificator {

protected:
	void DiscardRegion(TileIndex start_tile, int total_number, std::set<TileIndex>* lake_tiles, std::set<TileIndex> &guaranteed_water_tiles, std::set<TileIndex> &discarded_lake_tiles,
					bool stop_if_guaranteed);

public:
	virtual ~LakeModificator() {}

	/** Implementations of this function adapt a lake in some way.  They get a Lake, and some additional data about it,
	 *  and may modify it in some way.  In detail, the function receives a set of guaranteed lake tiles.
	 *
	 *  All lake tiles not being member of that set may be discarded (by adding them to the respective set).  This allows
	 *  for shrinking the lake (e.g. to gain more usable space in flat valleys), but at the same time ensures that the
	 *  rivers around the lake stay connected.  Lake tiles being registered in the lake, but marked as discarded will
	 *  nevertheless be terraformed to the surface height of the lake, in order to avoid ugly dry basins next to a lake.
	 *
	 *  The surface height of a lake may not be increased (as doing so might damage the landscape next to a lake in a
	 *  not-river-compatible way).  The surface height may however be decreased as long as the LakeModificator ensures
	 *  by terraforming that the water doesn´t need to flow upwards to escape the lake, *and* the surface height is still
     *  greater or equal the surface height of a lake the water of the lake flows to.
	 *
	 *  After all lake modificators have run, all remaining lake tiles (discarded or not) which are lower than the surface
	 *  height will be terraformed to the surface height.  Lake tiles that are higher than the surface height (can happen
	 *  if the surface height was decreased) will not be touched with respect to terraforming, and will be discarded
	 *  afterwards, unless they are declared guaranteed lake tiles.
	 *
	 *  The latter on the one hand ensures that the rivers around the lake stay connected, while on the other hand, ugly
	 *  water tiles ascending next to the lake are avoided.
	 *
	 *  @param lake some lake; its surface height may be modified within the bounds described above, all other properties
	 *              must not be modified.
     *  @param inflow_tile_to_center map from inflow tiles of the lake, to the corresponding center tiles inside the lake where the flow
	 *                               ends.  May not be modified.
     *  @param guaranteed_water_tiles The set of tiles of the lake, that must stay lake tiles (in order to keep rivers connected).
	 *                      May not be modified.
     *  @param discarded_lake_tiles Set of lake tiles that are discarded, i.e. they are subject to terraforming within the
	 *                              bounds described above, but will not be declared water.
	 */
	virtual void ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
							std::set<TileIndex> &discarded_lake_tiles) = 0;
};

struct OnlyGuaranteedLakeModificator : public LakeModificator {
	virtual void ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
							std::set<TileIndex> &discarded_lake_tiles);
};

struct IslandLakeModificator : public LakeModificator {
	virtual void ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
							std::set<TileIndex> &discarded_lake_tiles);
};

struct ExpandShoreLakeModificator : public LakeModificator {
	virtual void ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
							std::set<TileIndex> &discarded_lake_tiles);
};

/** Index in the grid of wide valley multipliers. */
typedef int ValleyGridIndex;

/** Grid size in that grid */
static const int VALLEY_GRID_SIZE = 16;

inline ValleyGridIndex ValleyGridXY(int x, int y) { return y * (MapSizeX() / VALLEY_GRID_SIZE) + x; }
inline int ValleyGridX(ValleyGridIndex c) { return c % (MapSizeX() / VALLEY_GRID_SIZE); }
inline int ValleyGridY(ValleyGridIndex c) { return c / (MapSizeX() / VALLEY_GRID_SIZE); }
inline int GetNumberOfValleyGrids() { return (MapSizeX() * MapSizeY()) / (VALLEY_GRID_SIZE * VALLEY_GRID_SIZE); }

/** A pattern for a local terraform operation.  It specifies, to which slope and which height (relative to the current height) some given
 *  tile should be performed.  Optionally, a (dx,dy) delta for jumping to another tile to be terraformed (instead of the given tile) can
 *  be given.  In that case, the base height will be taken from the given tile, not from the calculated and terraformed tile.
 */
struct TerraformingScheme {
	Slope slope;
	int delta_height;
	int dx;
	int dy;

	TerraformingScheme() {}
	TerraformingScheme(Slope slope, int delta_height) { this->slope = slope; this->delta_height = delta_height; dx = 0; dy = 0; }
	TerraformingScheme(Slope slope, int delta_height, int dx, int dy) { this->slope = slope; this->delta_height = delta_height; this->dx = dx; this->dy = dy; }
};

/** Information about a local terraform operation, that was already evaluated, but not yet performed.
 */
struct TerraformingAction {
	TerraformerState terraformer_state;
	std::set<TileIndex> affected_tiles;

	/** The score, i.e. the number of fixed tiles minus the number of tiles that were previously ok, but would be not ok for water if the operation would be executed. */
	int score;

	/** Similar score, but for the number of planned river tiles having steep slope, or SLOPE_NS, or SLOPE_EW. */
	int extra_bad_score;

	bool success;

	Slope slope;
	int height;

	TerraformingAction(Slope slope, int height) { this->slope = slope; this->height = height; this->terraformer_state = TerraformerState(); affected_tiles = std::set<TileIndex>(); this->score = 0; this->extra_bad_score = 0; this->success = false; }

	bool operator < (const TerraformingAction& other) const
    {
		if (!this->success) {
			return false;
		} else if (!other.success) {
			return true;
		} else {
			return this->score > other.score;
		}
    }
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

	/* Background about the following three variables: The aystar implementation of OpenTTD unfortunately needs static callback functions,
	 * instead of passing some object oriented interface implementation.  This river generator is implemented purely object oriented.
     * Thus, somehow accessing the generator state from the callback functions of aystar does only work if that state is stored
	 * in a static way.
	 */
	static std::vector<TileIndex>* found_path;
	static std::set<TileIndex>* lake_tiles;
	static int max_height;

	static int32 LakePathSearch_EndNodeCheck(AyStar *aystar, OpenListNode *current);
	static int32 LakePathSearch_CalculateG(AyStar *aystar, AyStarNode *current, OpenListNode *parent);
	static int32 LakePathSearch_CalculateH(AyStar *aystar, AyStarNode *current, OpenListNode *parent);
	static void LakePathSearch_GetNeighbours(AyStar *aystar, OpenListNode *current);
	static void LakePathSearch_FoundEndNode(AyStar *aystar, OpenListNode *current);

	static const uint LAKE_HASH_SIZE = 8; ///< The number of bits the hash for lake path finding should have.
	static uint LakePathSearch_Hash(uint tile, uint dir);

	/* Conceptionally a map from slopes to lists of possible terraforming operations.  Implemented in a way that
     * is efficient, and can be written down in a concise manner.
     */
	std::vector<TerraformingScheme> slope_to_schemes[31];

	LakeConnectedComponentCalculator *lake_connected_component_calculator;

	bool IsPlannedAsWater(TileIndex tile, int *water_flow, byte *water_info);
	bool IsPureDiagonalFlow(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info,
						   Direction diagonal_direction, Direction straight_direction_one, Direction straight_direction_two);
	int GetNumberOfAscendedSlopes(Slope slope);
	void DeclareNeighborTileWater(TileIndex water_neighbor_tiles[DIR_COUNT], TileIndex neighbor_tiles[DIR_COUNT], bool add_tile, Direction direction);
	void SetExtraNeighborTilesProcessed(TileIndex water_neighbor_tiles[DIR_COUNT], byte *water_info, std::vector<TileWithValue> &extra_river_tiles, bool add_tile, Direction direction, int flow);
	void UpdateFlow(int *water_flow, std::vector<TileWithHeightAndFlow> &water_tiles);
	void GetProblemTiles(std::vector<TileWithHeightAndFlow> &water_tiles, std::set<TileIndex> &problem_tiles, byte *water_info);

	void ModifyFlow(int *water_flow, byte *water_info);

	std::vector<int> wide_river_bounds;
	int GetWideRiverBoundForFlow(int flow);
	inline int GetFlowNeededForWideRiverBound(int bound) { return this->wide_river_bounds[bound]; }

	void PrepareLake(TileIndex tile, int *water_flow, byte *water_info, DefineLakesIterator *lake_iterator, std::vector<TileWithValue> &extra_water_tiles);
	void ChooseTileForExtraRiver(TileIndex tile,
								TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT],
								Direction diagonal_direction, Direction straight_direction_one, Direction straight_direction_two,
								bool inflow_pure, bool *choose_tile_one, bool *choose_tile_two);
	void PrepareRiverTile(TileIndex tile, int flow, int *water_flow, byte *water_info, std::vector<TileWithValue> &extra_river_tiles);
	void DeterminePlannedWaterTiles(std::vector<TileWithHeightAndFlow> &water_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator);
	void PrepareRiversAndLakes(std::vector<TileWithHeightAndFlow> &water_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, std::vector<TileWithValue> &extra_river_tiles);
	void AddExtraRiverTilesToWaterTiles(std::vector<TileWithHeightAndFlow> &water_tiles, std::vector<TileWithValue> &extra_river_tiles);
	void GenerateRiverTiles(std::vector<TileWithHeightAndFlow> &water_tiles, byte *water_info);

	void MarkCornerTileGuaranteed(int *water_flow, byte *water_info, std::set<TileIndex>* lake_tiles, std::set<TileIndex> &guaranteed_water_tiles, TileIndex tile, Direction direction,
						  Direction alternative_direction_one, Direction alternative_direction_two);

	bool ImproveByTerraforming(TileIndex tile, int flow, std::set<TileIndex> &tiles_fixed, std::set<TileIndex> &new_problem_tiles, int *water_flow, byte *water_info,
							   DefineLakesIterator *define_lakes_iterator, bool only_self, bool only_improve, bool make_water_afterwards);
	void FixByLocalTerraforming(std::set<TileIndex> &problem_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator);

	void StoreNeighborTilesPlannedForWater(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info);
	bool IsInclinedSlopePossible(TileIndex tile, TileIndex water_neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info, Direction direction, Slope slope, Slope desired_slope, int min_diagonal_height,
								 Direction neighbor_direction_one, Direction neighbor_direction_two, Direction diagonal_direction_one, Direction diagonal_direction_two);
	void RegisterTilesAffectedByTerraforming(TerraformerState &terraformer_state, std::set<TileIndex> &affected_tiles, byte *water_info, int min_height, bool only_processed_tiles = true);
	void LowerTileForDiagonalWater(std::set<TileIndex> &problem_tiles, TileIndex tile, byte *water_info, int x_offset, int y_offset, std::set<TileIndex> &affected_tiles);
	void LowerHigherWaterTilesUntilValid(std::set<TileIndex> &problem_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator);
	void FineTuneTilesForWater(int *water_flow, byte *water_info, std::vector<TileWithHeightAndFlow> &water_tiles, DefineLakesIterator *define_lakes_iterator);

	void MakeRiverTileWiderStraight(bool river, bool valley,
									TileIndex tile, int base_flow, int dx, int dy, int desired_width, int desired_height, Slope desired_slope, int number_of_alternatives, int *water_flow, byte *water_info,
									DefineLakesIterator *define_lakes_iterator, int *valley_grid, std::map<TileIndex, HeightAndFlow> &additional_water_tiles);
	void MakeRiversWiderByDirection(bool river, bool valley, TileIndex tile, Direction direction, int reached_bound, int height, Slope slope, int *water_flow, byte *water_info,
								    DefineLakesIterator *define_lakes_iterator, int *valley_grid, std::map<TileIndex, HeightAndFlow> &additional_water_tiles);
	void DoGenerateWiderRivers(bool river, bool valley, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, int *valley_grid, std::vector<TileWithHeightAndFlow> &water_tiles);
	void ModifyValleyGrid(int *valley_grid, int number_of_steps, int radius, int max_offset, bool init);
	void InitializeValleyGrid(int *valley_grid);
	void GenerateWiderRivers(int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, std::vector<TileWithHeightAndFlow> &water_tiles);

public:
	static bool CalculateLakePath(std::set<TileIndex> &lake_tiles, TileIndex from_tile, TileIndex to_tile, std::vector<TileIndex> &path_tiles, int max_height);

	RainfallRiverGenerator();
	virtual void GenerateRivers();
};

#endif /* RIVERS_RAINFALL_H */
