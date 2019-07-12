/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rivers_rainfall.cpp River calculation based on simulated rainfall. */

#include "stdafx.h"

#include "clear_map.h"
#include "debug.h"
#include "genworld.h"
#include "landscape_util.h"
#include "map_func.h"
#include "slope_func.h"
#include "settings_type.h"
#include "terraform_func.h"
#include "tile_map.h"
#include "water_map.h"

#include "core/alloc_func.hpp"
#include "core/mem_func.hpp"
#include "core/random_func.hpp"

#include "rivers_rainfall.h"

#include <algorithm>
#include <vector>

#include "safeguards.h"

/* Debug copies of the arrays calculated in the river generator, to allow debugging inspection via the map info window. */
int *_number_of_lower_tiles = NULL;
int *_water_flow = NULL;
byte *_water_info = NULL;

/* ================================================================================================================== */
/* ============================ BasinConnectedComponentCalculator =================================================== */
/* ================================================================================================================== */

bool BasinConnectedComponentCalculator::RecognizeTile(std::set<TileIndex> &tiles, TileIndex tile, TileIndex prev_tile)
{
	if (tiles.find(tile) == tiles.end()) {
		int height;
		Slope slope = GetTileSlope(tile, &height);

		if (height >= this->max_height) {
			/* Don´t recognize tiles that are higher than the potential basin we inspect */
			return false;
		} else {
			/* Only consider straight neighbors, and stop if both tiles are at the same height, but share a raised edge.
			 */
			bool west_corner_raised = ((slope & SLOPE_W) == SLOPE_W);
			bool east_corner_raised = ((slope & SLOPE_E) == SLOPE_E);
			bool north_corner_raised = ((slope & SLOPE_N) == SLOPE_N);
			bool south_corner_raised = ((slope & SLOPE_S) == SLOPE_S);

			Slope prev_slope = GetTileSlope(prev_tile);
			bool prev_west_corner_raised = ((prev_slope & SLOPE_W) == SLOPE_W);
			bool prev_east_corner_raised = ((prev_slope & SLOPE_E) == SLOPE_E);
			bool prev_north_corner_raised = ((prev_slope & SLOPE_N) == SLOPE_N);
			bool prev_south_corner_raised = ((prev_slope & SLOPE_S) == SLOPE_S);

			int x = TileX(tile);
			int y = TileY(tile);
			int prev_x = TileX(prev_tile);
			int prev_y = TileY(prev_tile);

			return (   (x == prev_x - 1 && y == prev_y && !(west_corner_raised && south_corner_raised && prev_east_corner_raised && prev_north_corner_raised))
					|| (x == prev_x && y == prev_y + 1 && !(north_corner_raised && west_corner_raised && prev_south_corner_raised && prev_east_corner_raised))
				    || (x == prev_x + 1 && y == prev_y && !(east_corner_raised && north_corner_raised && prev_west_corner_raised && prev_south_corner_raised))
					|| (x == prev_x && y == prev_y - 1 && !(south_corner_raised && east_corner_raised && prev_north_corner_raised && prev_west_corner_raised)));
		}
	} else {
		return false;
	}
}

/* ================================================================================================================== */
/* ====================== NumberOfLowerConnectedComponentCalculator ================================================= */
/* ================================================================================================================== */

bool NumberOfLowerConnectedComponentCalculator::RecognizeTile(std::set<TileIndex> &tiles, TileIndex tile, TileIndex prev_tile)
{
	if (tiles.find(tile) != tiles.end() || (this->ignore_ocean && IsTileType(tile, MP_WATER) && !IsCoastTile(tile))) {
		return false;
	} else {
	    int height;
		Slope slope = GetTileSlope(tile, &height);

		if (height != this->ref_height) {
			return false;
		} else if (prev_tile == INVALID_TILE) {
			return true;
		} else {
			/*  If tiles share a corner or an edge, but that corner / edge is raised with respect to our heightlevel,
			 *  then at least here, the two tiles are not connected in the sense of this iterator.
		     */
			int x = TileX(tile);
			int y = TileY(tile);
			int prev_x = TileX(prev_tile);
			int prev_y = TileY(prev_tile);

			if (prev_x == x - 1) {
				if (prev_y == y - 1) {
					return (slope & SLOPE_N) == 0;
				} else if (prev_y == y) {
					return (slope & SLOPE_N) == 0 || (slope & SLOPE_E) == 0;
				} else {
					return (slope & SLOPE_E) == 0;
				}
			} else if (prev_x == x) {
				if (prev_y == y - 1) {
					return (slope & SLOPE_N) == 0 || (slope & SLOPE_W) == 0;
				} else {
					return (slope & SLOPE_S) == 0 || (slope & SLOPE_E) == 0;
				}
			} else {
				if (prev_y == y - 1) {
					return (slope & SLOPE_W) == 0;
				} else if (prev_y == y) {
					return (slope & SLOPE_S) == 0 || (slope & SLOPE_W) == 0;
				} else {
					return (slope & SLOPE_S) == 0;
				}
			}
		}
	}
}

/* ================================================================================================================== */
/* ============================= NumberOfLowerHeightIterator ======================================================== */
/* ================================================================================================================== */

/** Returns wether the given slope is recognized in the given level of iterating over tiles of some heightlevel */
inline bool IsSlopeForHeightIterationLevel(Slope slope, int level)
{
	return (level == LEVEL_FLAT && slope == SLOPE_FLAT)
		|| (level == LEVEL_ONE_CORNER_RAISED && IsSlopeWithOneCornerRaised(slope))
		|| (level == LEVEL_TWO_CORNER_RAISED && (IsInclinedSlope(slope) || slope == SLOPE_NS || slope == SLOPE_EW))
		|| (level == LEVEL_THREE_CORNER_RAISED && (slope == SLOPE_NWS || slope == SLOPE_WSE || slope == SLOPE_SEN || slope == SLOPE_ENW))
		|| (level == LEVEL_STEEP && IsSteepSlope(slope));
}

/** Inserts all tiles into the given dirty_tiles set, that are members of the given base_set, are not yet processed, but have processed neighbor tiles.
 */
void NumberOfLowerHeightIterator::StoreNeighborTilesOfProcessedTiles(std::set<TileIndex> &base_set, std::set<TileIndex> &dirty_tiles, TileIndex neighbor_tiles[DIR_COUNT])
{
	for (std::set<TileIndex>::const_iterator it = base_set.begin(); it != base_set.end(); it++) {
		TileIndex tile = *it;

		if (this->number_of_lower_tiles[tile] < 0) {
			StoreAllNeighborTiles(tile, neighbor_tiles);
			for (int z = DIR_BEGIN; z < DIR_END; z++) {
				if (neighbor_tiles[z] != INVALID_TILE && this->number_of_lower_tiles[neighbor_tiles[z]] >= 0) {
					dirty_tiles.insert(tile);
					break;
				}
			}
		}
	}
}

/** This function actually calculates the number of lower tiles measure.  Called for some tile, it processes
 *  all tiles in the connected component of its height, using a breadth first search.  Among the dirty tiles
 *  at some point of the calculation, the flatter a tile is, the earlier it will be processed
 *  (in other words, if tiles A (flat), B (one corner up), C (two corners up) are available, A will be processed
 *  first, followed by B.  If then, a flat tile D is found and enters the dirty tiles, it will be processed
 *  *before* C.  This approach is necessary to do things right, if e.g. a valley has some narrow part
 *  where no flat tiles are between the inclined sides of the valley, and upwards, the valley contains flat tiles
 *  again.
 */
void NumberOfLowerHeightIterator::ProcessTile(TileIndex tile, Slope slope)
{
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;

	int ref_height = GetTileZ(tile);
	DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, "ProcessTile called for (%i,%i), height %i", TileX(tile), TileY(tile), ref_height);

	/* Check wether we already calculated this tile (see below). */
	if (this->number_of_lower_tiles[tile] != -1) {
		return;
	}

	/* Calculate connected component */
	std::set<TileIndex> connected_component = std::set<TileIndex>();
	this->connected_component_calculator->Initialize(ref_height);
	this->connected_component_calculator->StoreConnectedComponent(connected_component, tile);

	/* Start with all tiles of the connected component, that are neighbor tiles of already calculated tiles */
	std::set<TileIndex> dirty_tiles = std::set<TileIndex>();
	this->StoreNeighborTilesOfProcessedTiles(connected_component, dirty_tiles, neighbor_tiles);

	DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... Found connected component of " PRINTF_SIZE " tiles, and " PRINTF_SIZE " dirty tiles.", connected_component.size(), dirty_tiles.size());
	for (std::set<TileIndex>::const_iterator it = dirty_tiles.begin(); it != dirty_tiles.end(); it++) {
		DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, "........ Tile (%i,%i)", TileX(*it), TileY(*it));
	}

	/* If no tile of the connected component has an already calculated neighbor, recognize tiles at the map edge.
     * After all, we might be in a valley ending at the map edge, where the straightforward thing is assigning
     * the map edge tiles zero lower tiles.
     */
	if (dirty_tiles.size() == 0 && this->set_map_edge_tiles_to_zero) {
		std::set<TileIndex> edge_tiles = std::set<TileIndex>();
		for (std::set<TileIndex>::const_iterator it = connected_component.begin(); it != connected_component.end(); it++) {
			TileIndex tile = *it;
			int x = TileX(tile);
			int y = TileY(tile);

			if (x == 1 || y == 1 || x == (int)MapMaxX() - 1 || y == (int)MapMaxY() - 1) {
				this->number_of_lower_tiles[tile] = 0;
				edge_tiles.insert(tile);
			}
		}

		/* Remove the edge tiles from the connected component (as in contrast to other tiles, we assigned them a value just now),
		 * and try to find dirty tiles based on them.
		 */
		if (edge_tiles.size() > 0) {
			for (std::set<TileIndex>::const_iterator it = edge_tiles.begin(); it != edge_tiles.end(); it++) {
				connected_component.erase(*it);
			}

			this->StoreNeighborTilesOfProcessedTiles(connected_component, dirty_tiles, neighbor_tiles);
			if (dirty_tiles.size() > 0) {
				DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... Declared " PRINTF_SIZE " map edge tiles dirty, as no other dirty tiles existed.", dirty_tiles.size());
			}
		}
	}

	/* It´s possible that we ran out of tiles */
	if (connected_component.size() == 0) {
		return;
	}

	/* If we did not find any already calculated neighbor tile by now, we are in a depression.
	 * Pick up a random tile, it will be declared a lake center with zero number of lower tiles.
	 */
	if (dirty_tiles.size() == 0) {
		std::set<TileIndex> lake_center_set = std::set<TileIndex>();

		// TODO: Try to find tile in lowest group, e.g. a flat tile if such a tile exists in the connected component
		std::set<TileIndex>::const_iterator connected_component_it = connected_component.begin();
		std::advance(connected_component_it, RandomRange(connected_component.size()));
		lake_center_set.insert(*connected_component_it);

		for (std::set<TileIndex>::const_iterator it = lake_center_set.begin(); it != lake_center_set.end(); it++) {
			this->number_of_lower_tiles[*it] = 0;
		}

		this->StoreNeighborTilesOfProcessedTiles(connected_component, dirty_tiles, neighbor_tiles);
		for (std::set<TileIndex>::const_iterator it = lake_center_set.begin(); it != lake_center_set.end(); it++) {
			connected_component.erase(*it);
		}
		DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... Declared " PRINTF_SIZE " random tiles dirty (for a future lake), as no other dirty tiles existed.", dirty_tiles.size());
	}

	/* Again, we might have ran out of tiles */
	if (connected_component.size() == 0) {
		return;
	}

	/* Now, we actually process the connected component.  We start in LEVEL_FLAT, i.e. in the first iteration we only consider flat tiles.
	 * If we have no flat tiles, in the next iteration, we only consider tiles with one corner raised, and so on.
	 * Once, in some level, we found at least one tile of the corresponding slopes, we go back to LEVEL_FLAT for the next iteration,
	 * as we will probably find new dirty tiles in that situations, and they might be flat as well.
	 */
	int iteration = 0;
	int level = LEVEL_FLAT;
	while (connected_component.size() > 0) {
		iteration++;
		DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... Starting with " PRINTF_SIZE " remaining tiles in iteration %i, at level %i", connected_component.size(), iteration, level);

		std::vector<TileIndex> curr_fixed_tiles = std::vector<TileIndex>();
		std::vector<TileIndex> next_dirty_tiles = std::vector<TileIndex>();

		bool progress = false;
		for (std::set<TileIndex>::const_iterator it = dirty_tiles.begin(); it != dirty_tiles.end(); it++) {
			TileIndex dirty_tile = *it;
			Slope slope = GetTileSlope(dirty_tile);

			if (   (level == LEVEL_FLAT && slope == SLOPE_FLAT)
				|| (level == LEVEL_ONE_CORNER_RAISED && IsSlopeWithOneCornerRaised(slope))
				|| (level == LEVEL_TWO_CORNER_RAISED && (IsInclinedSlope(slope) || slope == SLOPE_NS || slope == SLOPE_EW))
				|| (level == LEVEL_THREE_CORNER_RAISED && (slope == SLOPE_NWS || slope == SLOPE_WSE || slope == SLOPE_SEN || slope == SLOPE_ENW))
				|| (level == LEVEL_STEEP && IsSteepSlope(slope))) {

				/* Calculate number of lower tiles based on the value of all already calculated neighbors.
				 * Consider the neighbor tiles in the order of their heightlevels, to ensure that we base our number on actually
			     * lower neighbor tiles. */
				StoreAllNeighborTiles(dirty_tile, neighbor_tiles);
				int min_lower_tiles = MapSize() + 1;
				for (int h = max(0, ref_height - 2); h <= ref_height; h++) {
					int curr_lower_tiles = h < ref_height ? -1 : MapSize() + 1;
					bool found_tile = false;
					for (int z = 0; z < DIR_COUNT; z++) {
						TileIndex neighbor_tile = neighbor_tiles[z];
						if (neighbor_tile != INVALID_TILE && this->number_of_lower_tiles[neighbor_tile] >= 0 && GetTileZ(neighbor_tile) == h) {
							if (h < ref_height) {
								/* For lower heightlevels, take the maximum over the already calculated neighbor tiles of the heightlevel at hand.
								 * Taking the maximum, instead of the minimum, might seem contra-intuitive at first glance.  The reasoning behind it is as follows.
								 * Consider a series of inlined SLOPE_NE.  In the left, the southwest tiles are located, in the right the northeast tiles.  The number
								 * is the number of lower tiles.  With min, the results might be as follows:
								 * 86 87 88 89
								 * 87 87 88 89
								 * 88 88 88 89
								 * i.e. we might run into situations, where tiles that have a clear lower-higher-relationship have equal number of lower tiles.
								 * With maximum, the result would look like:
								 * 86 88 90
								 * 87 89 90
								 * 88 89 90
								 * i.e. numbers grow with increasing heightlevel.
								 */
								curr_lower_tiles = max(curr_lower_tiles, this->number_of_lower_tiles[neighbor_tile]);
							} else {
								/* In contrast, in flat areas, we need to take the minimum.  If e.g. tiles at map edge have
								 * 0 0 0 0 0 0 0 lower tiles
								 * with max, the tiles in the next row would get
								 * 0 0 0 0 0 0 0
							     * 1 2 3 4 5 6 7 lower tiles
								 * which is clearly not what we want.
								 */
								curr_lower_tiles = min(curr_lower_tiles, this->number_of_lower_tiles[neighbor_tile]);
							}
							found_tile = true;

							DEBUG(map, 9, "............ Setting min_lower_tiles = %i based on neighbor tile (%i,%i)", min_lower_tiles, TileX(neighbor_tile), TileY(neighbor_tile));
						}
					}
					/* Consider the values at the lowest heightlevel of neighbor tiles where something was already processed. */
					if (found_tile) {
						min_lower_tiles = curr_lower_tiles;
						break;
					}
				}

				/* ... and actually store the calculated number of lower tiles */
				if (min_lower_tiles <= (int)MapSize()) {
					this->number_of_lower_tiles[dirty_tile] = min_lower_tiles + 1;
				} else {
					DEBUG(map, 0, "WARNING: Found dirty tile (%i,%i) with no already processed neighbor tiles.  This should not be possible here.", TileX(dirty_tile), TileY(dirty_tile));
				}

				DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, "........ Processing tile (%i,%i), calculating number of lower tiles %i",
							  TileX(dirty_tile), TileY(dirty_tile), this->number_of_lower_tiles[dirty_tile]);

				/* Update dirty tiles for next iteration */
				for (int z = 0; z < DIR_COUNT; z++) {
					TileIndex neighbor_tile = neighbor_tiles[z];
					if (neighbor_tile != INVALID_TILE && this->number_of_lower_tiles[neighbor_tile] < 0 && connected_component.find(neighbor_tile) != connected_component.end()) {
						next_dirty_tiles.push_back(neighbor_tile);
						DEBUG(map, 9, "............ Adding neighbor tile (%i,%i) to dirty tiles", TileX(neighbor_tile), TileY(neighbor_tile));
					}
				}

				curr_fixed_tiles.push_back(dirty_tile);
				connected_component.erase(dirty_tile);
				progress = true;
			}
		}

		if (progress) {
			/* If we made any progress, we go back to LEVEL_FLAT. After all, the tiles in next_dirty_tiles that entered our iteration
			 * newly might in particular be flat; and if that is the case, we want to process them first.
			 */
			dirty_tiles.insert(next_dirty_tiles.begin(), next_dirty_tiles.end());
			for (uint z = 0; z < curr_fixed_tiles.size(); z++) {
				dirty_tiles.erase(curr_fixed_tiles[z]);
			}
			level = LEVEL_FLAT;

			DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... Made progress, having " PRINTF_SIZE " dirty tiles now, level = LEVEL_FLAT = 0", dirty_tiles.size());
		} else if (level < LEVEL_MAX) {
			/* If no flat tiles were found in dirty_tiles, proceed with those with one corner raised, and so on */
			level++;

			DEBUG(map, RAINFALL_NUMBER_OF_LOWER_LOG_LEVEL, ".... No progress, switching to level %i", level);
		} else if (connected_component.size() > 0) {
			DEBUG(map, 0, "Tiles are left for connected component, but no more progress possible. ABORTING.");
			return;
		}
	}
}

NumberOfLowerHeightIterator::NumberOfLowerHeightIterator(HeightIndex *height_index, bool set_map_edge_tiles_to_zero) : HeightLevelIterator(height_index)
{
	this->number_of_lower_tiles = CallocT<int>(MapSizeX() * MapSizeY());
	this->ReInit(set_map_edge_tiles_to_zero);

	this->connected_component_calculator = new NumberOfLowerConnectedComponentCalculator(false);
}

NumberOfLowerHeightIterator::~NumberOfLowerHeightIterator()
{
	free(this->number_of_lower_tiles);
	delete this->connected_component_calculator;
}

void NumberOfLowerHeightIterator::ReInit(bool set_map_edge_tiles_to_zero)
{
	this->set_map_edge_tiles_to_zero = set_map_edge_tiles_to_zero;
	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		this->number_of_lower_tiles[n] = -1;
	}
}

/* ================================================================================================================== */
/* ================================ Flow related functions ========================================================== */
/* ================================================================================================================== */

/** Given the water information about a tile (which contains the flow direction), this function returns the tile where
 *  the flow of the tile directs to.
 *  @param tile some tile
 *  @param water_info corresponding water information
 *  @return the tile where the flow ends up, INVALID_TILE if we end up at the map edge
 */
TileIndex AddFlowDirectionToTile(TileIndex tile, byte water_info)
{
    int water_type = GB(water_info, 3, 3);
	assert(water_type == WI_NONE || water_type == WI_RIVER || water_type == WI_LAKE);
	uint direction = GB(water_info, 0, 3);
	return AddDirectionToTile(tile, (Direction)direction);
}

TileIndex AddDirectionToTile(TileIndex tile, Direction direction)
{
	int x = TileX(tile);
	int y = TileY(tile);

	switch (direction) {
		case DIR_N    : return x > 1                  && y > 1                  ? tile + TileDiffXY(-1, -1) : INVALID_TILE;
		case DIR_NE   : return x > 1                                            ? tile + TileDiffXY(-1,  0) : INVALID_TILE;
		case DIR_E    : return x > 1                  && y < (int)MapMaxY() - 1 ? tile + TileDiffXY(-1,  1) : INVALID_TILE;
		case DIR_SE   : return                           y < (int)MapMaxY() - 1 ? tile + TileDiffXY( 0,  1) : INVALID_TILE;
		case DIR_S    : return x < (int)MapMaxX() - 1 && y < (int)MapMaxY() - 1 ? tile + TileDiffXY( 1,  1) : INVALID_TILE;
		case DIR_SW   : return x < (int)MapMaxX() - 1                           ? tile + TileDiffXY( 1,  0) : INVALID_TILE;
		case DIR_W    : return x < (int)MapMaxX() - 1 && y > 1                  ? tile + TileDiffXY( 1, -1) : INVALID_TILE;
		case DIR_NW   : return                           y > 1                  ? tile + TileDiffXY( 0, -1) : INVALID_TILE;
		default: return INVALID_TILE; // Impossible default case, as we fetch just three bits above
	}
}

/** Returns the direction from the source to the dest tile.
 *  @param source source tile
 *  @param dest dest tile
 *  @return the direction from the source to the dest tile.
 */
byte GetWaterInfoDirection(TileIndex source, TileIndex dest)
{
	int dx = TileX(dest) - TileX(source);
	int dy = TileY(dest) - TileY(source);

	if (dx == -1) {
		if (dy == -1) {
			return DIR_N;
		} else if (dy == 0) {
			return DIR_NE;
		} else if (dy == 1) {
			return DIR_E;
		}
	} else if (dx == 0) {
		if (dy == -1) {
			return DIR_NW;
		} else if (dy == 1) {
			return DIR_SE;
		}
	} else if (dx == 1) {
		if (dy == -1) {
			return DIR_W;
		} else if (dy == 0) {
			return DIR_SW;
		} else if (dy == 1) {
			return DIR_S;
		}
	}

	DEBUG(map, 0, "Illegal direction from (%i, %i) to (%i, %i): (%i, %i)", TileX(source), TileY(source), TileX(dest), TileY(dest), dx, dy);
	assert(false);
	return 0;
}

/* ================================================================================================================== */
/* ====================================== CalculateFlowIterator ===================================================== */
/* ================================================================================================================== */

/** Stores all tiles of base_set, that have neighbor tiles with already calculated flow in the given set.
 *  @param base_set base set
 *  @param dirty_tiles dirty tiles (out parameter)
 */
void CalculateFlowIterator::StoreNeighborTilesOfProcessedTiles(std::set<TileIndex> &base_set, std::set<TileIndex> &dirty_tiles, TileIndex neighbor_tiles[DIR_COUNT])
{
	for (std::set<TileIndex>::const_iterator it = base_set.begin(); it != base_set.end(); it++) {
		TileIndex tile = *it;

		if (this->water_flow[tile] < 0) {  // Maybe condition is unnecessary
			StoreAllNeighborTiles(tile, neighbor_tiles);
			for (int z = DIR_BEGIN; z < DIR_END; z++) {
				TileIndex neighbor_tile = neighbor_tiles[z];
				if (neighbor_tile != INVALID_TILE && this->water_flow[neighbor_tile] >= 0) {
					dirty_tiles.insert(tile);
					break;
				}
			}
		}
	}
}

void CalculateFlowIterator::ProcessTile(TileIndex tile, Slope slope)
{
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;

	/* Don´t calculate flow for non-coast ocean tiles.  Also, which is important as we calculate flow using connected components,
	 * don´t calculate flow if it is already calculated.
	 */
	if ((IsTileType(tile, MP_WATER) && !IsCoastTile(tile)) || this->water_flow[tile] != -1) {
		return;
	}

	/* Calculate connected component of tiles of the current height */
	int ref_height = GetTileZ(tile);
	std::set<TileIndex> connected_component = std::set<TileIndex>();
	this->connected_component_calculator->Initialize(ref_height);
	this->connected_component_calculator->StoreConnectedComponent(connected_component, tile);

	/* Sort them by their number of lower tiles, start with the biggest number of lower tiles (i.e. at the top in terms of landscape) */
	std::vector<TileWithValue> tiles_with_lower = std::vector<TileWithValue>();
	for (std::set<TileIndex>::const_iterator it = connected_component.begin(); it != connected_component.end(); it++) {
		TileIndex curr_tile = *it;
		tiles_with_lower.push_back(TileWithValue(curr_tile, this->number_of_lower_tiles[curr_tile]));
	}

	std::sort(tiles_with_lower.begin(), tiles_with_lower.end(), std::greater<TileWithValue>());

	DEBUG(map, RAINFALL_CALCULATE_FLOW_LOG_LEVEL, "Processing connected component near (%i,%i), with height %i, and " PRINTF_SIZE " tiles.",
				  TileX(tile), TileY(tile), ref_height, tiles_with_lower.size());

	/* ... and iterate over them */
	for (std::vector<TileWithValue>::const_iterator it = tiles_with_lower.begin(); it != tiles_with_lower.end(); it++) {
		TileIndex curr_tile = it->tile;

		/* Calculate inflow towards this tile, based on already calculated neighbor tiles */
		int inflow = 0;
		StoreAllNeighborTiles(curr_tile, neighbor_tiles);
		for (int z = 0; z < DIR_COUNT; z++) {
			TileIndex candidate_inflow_tile = neighbor_tiles[z];
			if (candidate_inflow_tile != INVALID_TILE && this->water_flow[candidate_inflow_tile] >= 0
				&& IsNoWater(this->water_info, candidate_inflow_tile)
				&& AddFlowDirectionToTile(candidate_inflow_tile, this->water_info[candidate_inflow_tile]) == curr_tile) {

				inflow += this->water_flow[candidate_inflow_tile];
			}
		}

		/* Precipitation on this tile.  Modify / replace this line of code, to implement more sophisticated
		 * percipitation schemes (like more rain in mountains, desert vs. rainforest, etc.) */
		int outflow = inflow + 1;

		DEBUG(map, RAINFALL_CALCULATE_FLOW_LOG_LEVEL, "........ Processing tile (%i,%i), number_of_lower %i, calculated inflow %i and outflow %i",
					TileX(curr_tile), TileY(curr_tile), it->value, inflow, outflow);

		/* Store the calculated flow */
		this->water_flow[curr_tile] = outflow;

		/* And finally determine where it flows to. */
		int lower_tiles_sum = 0;
		for (int z = DIR_BEGIN; z < DIR_END; z++) {
			TileIndex candidate_outflow_tile = neighbor_tiles[z];

			/* Important: Only choose tiles with smaller number of lower tiles.  This is the direction where terrain finally descends. */
			if (candidate_outflow_tile != INVALID_TILE && water_flow[candidate_outflow_tile] == -1 && this->number_of_lower_tiles[candidate_outflow_tile] < this->number_of_lower_tiles[curr_tile]) {
				int our_z = GetTileZ(curr_tile);
				int candidate_z = GetTileZ(candidate_outflow_tile);

				if (candidate_z <= our_z) {
					/* Add one to account for neighbor tiles with zero lower tiles. */
					lower_tiles_sum += (this->number_of_lower_tiles[candidate_outflow_tile] + 1);
					DEBUG(map, RAINFALL_CALCULATE_FLOW_LOG_LEVEL, "............ Tile (%i,%i) is a candidate outflow tile", TileX(candidate_outflow_tile), TileY(candidate_outflow_tile));
				} else {
					neighbor_tiles[z] = INVALID_TILE;
				}
			} else {
				neighbor_tiles[z] = INVALID_TILE;
			}
		}

		if (lower_tiles_sum == 0) {
			/* No out flow exists. */
			int x = TileX(curr_tile);
			int y = TileY(curr_tile);
			if (x > 1 && y > 1 && x < (int)MapMaxX() - 1 && y < (int)MapMaxY() - 1 && !IsTileType(curr_tile, MP_WATER)) {
				/* Declare the tile a lake, but only if it is not at the map edge. */
				DeclareActiveLakeCenter(this->water_info, curr_tile);
				DEBUG(map, RAINFALL_CALCULATE_FLOW_LOG_LEVEL, "........ No outflow found, declaring tile (%i,%i) a lake", TileX(curr_tile), TileY(curr_tile));
			} else {
				/* This means, that a potential river hits the map edge and disappears. */
				DeclareDisappearTile(this->water_info, curr_tile);
			}
		} else {
			/* Decide where the outflow goes to.  Prefer neighbor tiles with high number of lower tiles, as we want to prefer the
			 * way down a huge valley over the way into a local depression. */
			int curr_sum = 0;
			int r = RandomRange(lower_tiles_sum);
			for (int z = DIR_BEGIN; z < DIR_END; z++) {
				TileIndex candidate_outflow_tile = neighbor_tiles[z];
				if (candidate_outflow_tile != INVALID_TILE) {
					curr_sum += (this->number_of_lower_tiles[candidate_outflow_tile] + 1);
					if (r < curr_sum) {
						/* Calculate direction, and store it in the lower three bits of the water_info */
						byte direction = GetWaterInfoDirection(curr_tile, candidate_outflow_tile);
						this->water_info[curr_tile] = direction & 0x07;
						DEBUG(map, RAINFALL_CALCULATE_FLOW_LOG_LEVEL, "........ Setting water_flow %i in direction %i for tile (%i,%i)",
											this->water_flow[curr_tile], this->water_info[curr_tile], TileX(curr_tile), TileY(curr_tile));
						break;
					}
				}
			}
		}
	}
}

CalculateFlowIterator::CalculateFlowIterator(HeightIndex *height_index, int *number_of_lower_tiles) : HeightLevelIterator(height_index)
{
	this->number_of_lower_tiles = number_of_lower_tiles;
	this->water_flow = CallocT<int>(MapSizeX() * MapSizeY());
	this->water_info = CallocT<byte>(MapSizeX() * MapSizeY());

	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		this->water_flow[n] = -1;
		this->water_info[n] = 0;
	}
	this->connected_component_calculator = new NumberOfLowerConnectedComponentCalculator(true);
}

CalculateFlowIterator::~CalculateFlowIterator()
{
	free(this->water_flow);
	free(this->water_info);
	delete this->connected_component_calculator;
}

/* ================================================================================================================== */
/* ======================================== Lake ==================================================================== */
/* ================================================================================================================== */

/** Creates a new Lake instance.  Its surface height is set to GetTileZ of the given center tile.
 */
Lake::Lake(TileIndex lake_center)
{
	this->lake_center = lake_center;
	this->surface_height = GetTileZ(this->lake_center);
	this->already_processed_outflow = 0;
	this->already_spent_flow = 0;
	this->finished_without_outflow = false;
	this->outflow_tile = INVALID_TILE;
	this->unprocessed_edge_tiles = std::set<TileIndex>();
	this->unprocessed_edge_tiles_dirty = false;
	this->lake_tiles = std::set<TileIndex>();
}

void Lake::AddLakeTile(TileIndex tile)
{
	this->lake_tiles.insert(tile);

	uint x = TileX(tile);
	uint y = TileY(tile);
	if (x <= 1 || y <= 1 || x >= MapMaxX() - 1 || y >= MapMaxY() - 1) {
		this->finished_without_outflow = true;
	}
}

/** This function removes all tiles from the lake that are higher than the given height.
 *  As it is meant to be called in a situation where already an outflow tile is known, it
 *  does not touch the unprocessed edge tiles.
 *  @param max_height maximum allowed height
 */
void Lake::RemoveHigherLakeTiles(int max_height, int *water_flow, byte *water_info,  std::map<TileIndex, Lake*> &tile_to_lake)
{
	/* Remove safely what is to be removed.  As this whole code is only executed in a corner case anyway
     * performance shouldn´t be an issue either.
	 */
	std::set<TileIndex> tiles_to_be_removed = std::set<TileIndex>();
	for (std::set<TileIndex>::const_iterator it = this->lake_tiles.begin(); it != this->lake_tiles.end(); it++) {
		TileIndex tile = *it;
		if (GetTileZ(tile) > max_height) {
			tiles_to_be_removed.insert(tile);
			if (!IsLakeCenter(water_info, tile)) {
				if (water_flow[tile] >= _settings_newgame.game_creation.rainfall.flow_for_river) {
					DeclareRiver(water_info, tile);
				} else {
					SetWaterType(water_info, tile, WI_NONE);
				}
			}
		}
	}

	for (std::set<TileIndex>::const_iterator it = tiles_to_be_removed.begin(); it != tiles_to_be_removed.end(); it++) {
		this->lake_tiles.erase(*it);
		tile_to_lake.erase(*it);
	}
}

/** Debug function: Print extensive information about the lake to DEBUG(misc).
 *  @param level output level for anything except the lake tiles and unprocessed edge tiles
 *  @param detail_level output level for the lake tiles and unprocessed edge tiles
 */
void Lake::PrintToDebug(int level, int detail_level)
{
	DEBUG(map, level, "Lake at (%i,%i) with surface height %i, " PRINTF_SIZE " unprocessed edge tiles, " PRINTF_SIZE " lake tiles",
						TileX(this->lake_center), TileY(this->lake_center), this->surface_height, this->unprocessed_edge_tiles.size(), this->lake_tiles.size());
	if (this->outflow_tile != INVALID_TILE) {
		DEBUG(map, level, ".... Outflow tile is (%i,%i)", TileX(this->outflow_tile), TileY(this->outflow_tile));
	} else {
		DEBUG(map, level, ".... No outflow tile found yet.");
	}

	DEBUG(map, detail_level, ".... Unprocessed edge tiles:");
	for (std::set<TileIndex>::const_iterator it = this->unprocessed_edge_tiles.begin(); it != this->unprocessed_edge_tiles.end(); it++) {
		DEBUG(map, detail_level, "........ (%i,%i)", TileX(*it), TileY(*it));
	}
	DEBUG(map, detail_level, ".... Lake tiles:");
	for (std::set<TileIndex>::const_iterator it = this->lake_tiles.begin(); it != this->lake_tiles.end(); it++) {
		DEBUG(map, detail_level, "........ (%i,%i)", TileX(*it), TileY(*it));
	}
}

/* ================================================================================================================== */
/* ================================= LakeDefinitionState ============================================================ */
/* ================================================================================================================== */

/** Records, that lake definition has a look at the next Lake, while processing a path from the mountains to the sea.
 *  @param run detail information
 */
void LakeDefinitionState::StartRun(LakeDefinitionRun &run)
{
	this->runs.push_back(run);
	this->processed_lake_centers.insert(run.GetLake()->GetCenterTile());
}

/** Checks wether the previous lake has a lower surface height than the current one.
 *  For convenience, if yes, the surface height of the previous lake is returned.
 *  Otherwise (especially if no previous lake exists), -1 is returned.
 *  @return the surface height of the previous Lake, if it was lower than the one of the current Lake, -1 otherwise.
 */
int LakeDefinitionState::WasPreviousLakeLower()
{
	if ((int)this->runs.size() >= 2) {
		Lake *last_lake = this->runs[this->runs.size() - 1].GetLake();
		Lake *previous_lake = this->runs[this->runs.size() - 2].GetLake();
		return (previous_lake->GetSurfaceHeight() < last_lake->GetSurfaceHeight() ? previous_lake->GetSurfaceHeight() : -1);
	} else {
		return -1;
	}
}

/* ================================================================================================================== */
/* ================================== DefineLakesIterator  ========================================================== */
/* ================================================================================================================== */

/** Discards all tiles of the given neighbor tiles, that are not suitable for expanding the
 *  lake in this step.  Does not yet take the flow into account.
 */
void DefineLakesIterator::DiscardLakeNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int ref_height)
{
	for (uint n = DIR_BEGIN; n < DIR_END; n++) {
		if (neighbor_tiles[n] != INVALID_TILE) {
			if (GetTileZ(neighbor_tiles[n]) > ref_height) {
				neighbor_tiles[n] = INVALID_TILE;
			}
		}
	}
}

/** If flow starting at the given tile ends up in a Lake, that Lake instance is returned.
 *  @return destination Lake, or NULL if flow doesn´t end in a lake
 */
Lake* DefineLakesIterator::GetDestinationLake(TileIndex tile)
{
	/* Code robustness: Replace potential endless loops in case of (forbidden) circular flow by
	 * a senseful error message.
	 */
	std::set<TileIndex> processed_tiles = std::set<TileIndex>();

	while (tile != INVALID_TILE) {
		if (IsLakeCenter(this->water_info, tile)) {
			if (this->tile_to_lake.find(tile) != this->tile_to_lake.end()) {
				return this->tile_to_lake[tile];
			} else {
				return NULL;
			}
		} else if (IsTileType(tile, MP_WATER) || IsDisappearTile(this->water_info, tile)) {
			return NULL;
		} else {
			tile = AddFlowDirectionToTile(tile, this->water_info[tile]);
		}

		if (processed_tiles.find(tile) != processed_tiles.end()) {
			DEBUG(map, 0, "Error: Circular flow detected at (%i,%i)", TileX(tile), TileY(tile));
			return NULL;
		} else {
			processed_tiles.insert(tile);
		}
	}

	return NULL;
}

/** Returns wether flow starting at the given tile, ends outside the given Lake.
 *  @param tile some tile
 *  @param lake some Lake
 *  @param state current lake definition state
 */
bool DefineLakesIterator::DoesFlowEndOutsideLake(TileIndex tile, Lake *lake, LakeDefinitionState &state)
{
	int number_of_steps = 0;

	DEBUG(map, 9, "Checking wether tile (%i,%i) ends out of lake", TileX(tile), TileY(tile));
	while (true) {
		if (IsLakeCenter(this->water_info, tile)) {
			/* A Lake center was found, now look wether it is a suitable one. */

			/* Find the corresponding active lake center (we might have found a consumed one) */
			if (this->tile_to_lake.find(tile) != this->tile_to_lake.end()) {
				lake = this->tile_to_lake[tile];
				tile = lake->GetCenterTile();
			}

			if (number_of_steps == 0 || state.WasAlreadyProcessed(tile)) {
				/* In the first step, we never find a foreign Lake center.  If we have already seen the Lake center in this
				 * run of Lake definition we ignore it, since this would lead to endless recursion of CreateLake() below.
				 * The second case also catches the case that we have found the center of our own Lake.
				 */

				DEBUG(map, 9, ".... Finishing at (%i,%i) since already processed lake center", TileX(tile), TileY(tile));
				return false;
			} else {
				/* All problematic cases excluded above, success. */

				DEBUG(map, 9, "........ DoesFlowEndOutsideLake found not yet processed lake center at (%i,%i)", TileX(tile), TileY(tile));
				return true;
			}
		} else if (number_of_steps > 0 && lake->IsLakeTile(tile)) {
			/* We are back in the lake we currently process, this is not an outwards flow. */

			DEBUG(map, 9, ".... We end up in our lake: Tile (%i,%i)", TileX(tile), TileY(tile));
			return false;
		} else if ((IsTileType(tile, MP_WATER) && !IsCoastTile(tile)) || IsDisappearTile(this->water_info, tile)) {
			/* We have reached the sea or the map edge, and have for sure left the lake. */

			DEBUG(map, 9, ".... We end up in the WATER ===> Success: Tile (%i,%i)", TileX(tile), TileY(tile));
			return true;
		} else {
			/* Step forwards along the flow.  This eventually ends up in one of the cases above, as we don´t calculate circular flows.
			 */

			tile = AddFlowDirectionToTile(tile, this->water_info[tile]);
			if (tile == INVALID_TILE) {
				/* INVALID_TILE means leaving the map, which is a legal flow end. */
				DEBUG(map, 9, ".... Finishing as leaving the map");
				return true;
			}

			number_of_steps++;
			DEBUG(map, 9, ".... We step towards tile (%i,%i)", TileX(tile), TileY(tile));
		}
	}
}

/** Returns wether the outflow of the given other_lake hits (via a chain of arbitrary many lakes) the given lake.
 *  The case that no outflow tile exists is allowed.
 *  @param other_lake some Lake
 *  @param lake another Lake
 */
bool DefineLakesIterator::DoesOutflowHitLake(Lake *other_lake, Lake *lake)
{
	bool flow_back_to_lake = false;
	std::set<TileIndex> already_seen_lake_centers = std::set<TileIndex>();
	Lake *curr_lake = other_lake;
	while (curr_lake != NULL && curr_lake != lake) {
		TileIndex center_tile = curr_lake->GetCenterTile();
		if (already_seen_lake_centers.find(center_tile) != already_seen_lake_centers.end()) {
			DEBUG(map, 0, "Warning: While consuming lakes: Found a lake cycle at (%i,%i), near (%i,%i) and (%i,%i)", TileX(center_tile), TileY(center_tile),
						  TileX(other_lake->GetCenterTile()), TileY(other_lake->GetCenterTile()), TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()));
			break;
		} else {
			DEBUG(map, 9, "........ Add: (%i,%i)", TileX(center_tile), TileY(center_tile));
			already_seen_lake_centers.insert(center_tile);
		}

		TileIndex outflow_tile = curr_lake->GetOutflowTile();
		if (outflow_tile != INVALID_TILE) {
			curr_lake = this->GetDestinationLake(outflow_tile);
			if (curr_lake == lake) {
				flow_back_to_lake = true;
				break;
			}
		} else {
			curr_lake = NULL;
		}
	}
	return flow_back_to_lake;
}

/** Given a lake, this function consumes another lake.  Consuming means, add its tiles and flow to this lake,
 *  make the other lake center a consumed lake center.
 *  @param lake a Lake
 *  @param tile active lake center of the other lake
 *  @param state state of lake definition
 *  @param run current lake definition run
 *  @return the additional flow that was added to this Lake center tile flow (i.e., if the lake was already consumed, 0 is returned)
 */
int DefineLakesIterator::ConsumeLake(Lake *lake, TileIndex tile, int max_height, LakeDefinitionState &state, LakeDefinitionRun &run)
{
	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Consuming neighbor lake starting at (%i,%i)", TileX(tile), TileY(tile));

	/* Extra flow gained by consuming the Lake */
	int other_lake_flow = 0;

	/** There are cases where we consume a lake with higher surface height.
	 *  Example: Lake basin of height 1, outflow at height 2, two lake centers A and B in the basin.  lake A might find an outflow to lake B of height 1,
	 *           and afterwards, lake B is forced to find an outflow of height 2, since it may not choose lake A again.
	 *  In such cases, we choose all tiles of the higher lake that are low enough, but need to ensure that they are connected with the lower lake
	 *  (there might be a barrier of higher tiles in between).
	 *  We do this by calculating paths, and terraforming landscape downwards if necessary.
	 */
	std::vector<TileIndex> path_destinations = std::vector<TileIndex>();

	Lake *other_lake = this->tile_to_lake[tile];
	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "............... Lake is at (%i,%i) with %i tiles, other surface height %i",
						  TileX(other_lake->GetCenterTile()), TileY(other_lake->GetCenterTile()), other_lake->GetNumberOfLakeTiles(), other_lake->GetSurfaceHeight());

	bool flow_back_to_lake = this->DoesOutflowHitLake(other_lake, lake) || this->DoesOutflowHitLake(lake, other_lake);

	/* Record tiles added newly to this lake separately, as adding them too early disturbs our
	 * "Does it belong to our lake"-detection below using lake->IsLakeTile(tile).
	 */
	std::vector<TileIndex> new_tiles = std::vector<TileIndex>();

	/* Add all tiles of the other lake */
	std::set<TileIndex>* other_lake_tiles = other_lake->GetLakeTiles();
	for (std::set<TileIndex>::const_iterator other_it = other_lake_tiles->begin(); other_it != other_lake_tiles->end(); other_it++) {
		TileIndex curr_other_lake_tile = *other_it;

		DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".................... Inspecting tile (%i,%i)", TileX(curr_other_lake_tile), TileY(curr_other_lake_tile));

		if (IsActiveLakeCenter(this->water_info, curr_other_lake_tile) && lake->GetCenterTile() != curr_other_lake_tile) {
			/* Declare other active lake center consumed; our lake may only contain one active lake center */

			/* Add not yet processed flow of the other lake */
			if (!flow_back_to_lake) {
				other_lake_flow += this->water_flow[curr_other_lake_tile];
			}

			DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "......................... Is an active lake center; adding flow %i - %i / %i = %i",
						  this->water_flow[curr_other_lake_tile], other_lake->GetAlreadyProcessedOutflow(), other_lake->GetAlreadySpentFlow(), other_lake_flow);

			DeclareConsumedLakeCenter(this->water_info, curr_other_lake_tile);

			/* If the consumed lake (which in case of lake switching becomes the lake we continue with shortly) has an outflow to the current lake,
			 * its flow isn´t actually lost.
			 */
			if (other_lake->GetOutflowTile() != INVALID_TILE) {
				Lake *dest_lake = this->GetDestinationLake(other_lake->GetOutflowTile());
				if (dest_lake == lake) {
					other_lake->SetAlreadyProcessedOutflow(0);
					DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, "........................ Resetting already processed outflow of lake (%i,%i) to 0 since it ends up in our lake",
						  TileX(other_lake->GetCenterTile()), TileY(other_lake->GetCenterTile()));
				}
			}
		}

		if (IsLakeCenter(this->water_info, curr_other_lake_tile)) {
			/* Bookkeeping in the lake center to lake map, mark lake tile processed to avoid a potential endless recursion in case the other lake
			 * gains an outflow back to the tile processed here
			 */
			DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "......................... Is a lake center.");

			state.MarkProcessed(curr_other_lake_tile);

			path_destinations.push_back(curr_other_lake_tile);
		}
		new_tiles.push_back(curr_other_lake_tile);
	}

	/* Finally add the tiles of the other lake to our lake. */
	for (int n = 0; n < (int)new_tiles.size(); n++) {
		this->tile_to_lake.erase(new_tiles[n]);
		other_lake->RemoveLakeTile(new_tiles[n]);

		if (GetTileZ(new_tiles[n]) <= max_height) {
			this->tile_to_lake[new_tiles[n]] = lake;
			lake->AddLakeTile(new_tiles[n]);
		} else {
			DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".................... Ignoring tile (%i,%i) since it is higher than the conuming lake: %i > %i",
					  TileX(new_tiles[n]), TileY(new_tiles[n]), GetTileZ(new_tiles[n]), max_height);
		}
	}

	/* Unprocessed edge tiles need to be recalculated now */
	this->RecalculateUnprocessedEdgeTiles(lake);

	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Finished consuming lake, increased flow at (%i,%i) by %i to %i",
			  TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()), other_lake_flow, this->water_flow[lake->GetCenterTile()]);

	return other_lake_flow;
}

/** Clears and recalculates the unprocessed edge tiles of the given lake.
 *  @param lake some Lake
 */
void DefineLakesIterator::RecalculateUnprocessedEdgeTiles(Lake *lake)
{
	lake->ClearUnprocessedEdgeTiles();

	for (std::set<TileIndex>::const_iterator it = lake->GetLakeTilesBegin(); it != lake->GetLakeTilesEnd(); it++) {
		TileIndex tile = *it;
		this->RegisterAppropriateNeighborTilesAsUnprocessed(lake, tile, lake->GetSurfaceHeight());
	}
}


/** This function checks, wether for all lake centers of the given lake, a path of at most the given height exists
 *  to the outflow tile.  If no such path exists, one is terraformed.
 *  @param lake some lake
 *  @param outflow_tile the outflow tile
 *  @param desired_height desired heightlevel of path
 */
void DefineLakesIterator::TerraformPathsFromCentersToOutflow(Lake *lake, TileIndex outflow_tile, int desired_height)
{
	std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		TileIndex tile = *it;
		if (IsLakeCenter(this->water_info, tile)) {
			std::vector<TileIndex> path_tiles = std::vector<TileIndex>();
			bool success = RainfallRiverGenerator::CalculateLakePath(*lake_tiles, tile, outflow_tile, path_tiles, desired_height);
			if (success) {
				DEBUG(map, RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL, "Found a path with max height %i from center tile (%i,%i) to outflow tile (%i,%i), thus there is no need to terraform one.",
							desired_height, TileX(tile), TileY(tile), TileX(outflow_tile), TileY(outflow_tile));
			} else {
				success = RainfallRiverGenerator::CalculateLakePath(*lake_tiles, tile, outflow_tile, path_tiles, -1);
				if (success) {
					for (int z = 0; z < (int)path_tiles.size(); z++) {
						TerraformTileToSlope(path_tiles[z], desired_height, SLOPE_FLAT);
							DEBUG(map, RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL, "Digging a outflow canyon: Terraforming (%i,%i) to height %i",
										  TileX(path_tiles[z]), TileY(path_tiles[z]), lake->GetSurfaceHeight());
					}
				} else {
					DEBUG(map, 0, "WARNING: Could not connect tile (%i,%i) with outflow tile (%i,%i) in lake, this should not happen as lakes are supposed to be connected.",
								  TileX(tile), TileY(tile), TileX(outflow_tile), TileY(outflow_tile));
				}
			}
		}
	}
}

/** This function registers all neighbor tiles of the given tile, with at most the given height, which are not yet
 *  part of the lake, as unregistered edge tiles, i.e. candidate tiles for expanding the lake.
 *  @param lake some lake
 *  @parma tile some tile
 *  @param ref_height height limit as described
 */
void DefineLakesIterator::RegisterAppropriateNeighborTilesAsUnprocessed(Lake *lake, TileIndex tile, int ref_height)
{
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;


	/* Determine not higher straight neighbor tiles.  Those are suitable for exanding the lake in this step. */
	StoreStraightNeighborTiles(tile, neighbor_tiles);
	this->DiscardLakeNeighborTiles(tile, neighbor_tiles, ref_height);

	/* Record all found suitable neighbor tiles for later processing.  But only if they are not yet lake tiles. */
	for (uint n = DIR_BEGIN; n < DIR_END; n++) {
		if (neighbor_tiles[n] != INVALID_TILE && !lake->IsLakeTile(neighbor_tiles[n])) {
			lake->RegisterUnprocessedEdgeTile(neighbor_tiles[n]);
		}
	}
}

/** Creates a lake at the given tile, which is supposed to be a lake center.  Optionally adds extra flow
 *  gained from lakes somewhere up in the mountains.
 *  This function calls itself recursively for each Lake along a path from the given tile to
 *  (1) the sea, or
 *  (2) a lake for which no outflow can be found, or
 *  (3) the map edge
 *  Bookkeeping in the passed LakeDefinitionState assures that no endless recursion can occur.
 *  @param tile some lake center tile
 *  @param state LakeDefinitionState, see there
 *  @param extra_flow extra flow gained from a lake upwards in the mountains, or 0 if we start at this lake
 */
void DefineLakesIterator::CreateLake(TileIndex tile, LakeDefinitionState &state, int extra_flow)
{
	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "CREATE_LAKE for (%i,%i), extra_flow %i", TileX(tile), TileY(tile), extra_flow);

	bool already_processed = tile_to_lake.find(tile) != tile_to_lake.end();
	state.MarkProcessed(tile);
	Lake *lake;
	if (!already_processed) {
		/* Lake wasn´t processed yet, create a new Lake instance */
		lake = new Lake(tile);
		this->all_lakes.push_back(lake);
		lake->RegisterUnprocessedEdgeTile(tile);
		this->tile_to_lake[tile] = lake;
		DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Created new Lake instance for (%i,%i) since none was constructed yet.", TileX(tile), TileY(tile));
	} else {
		/* Reprocess existing Lake instance, iterate until we are sure that we found the lake center, and not just a consumed lake center of that Lake. */
		TileIndex prev_tile;
		do {
			lake = this->tile_to_lake[tile];
			prev_tile = tile;
			tile = lake->GetCenterTile();
			state.MarkProcessed(tile);
			DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Reusing existing lake instance, delegating to (%i,%i)", TileX(tile), TileY(tile));
		} while (tile != prev_tile);

		/* If the Lake was already finished without outflow, do nothing, the flow simply disappears.  This is the case, if the Lake has already hit the map border. */
		if (lake->WasFinishedWithoutOutflow()) {
			return;
		}
	}

	lake->PrintToDebug(RAINFALL_DEFINE_LAKES_LOG_LEVEL, 9);

	LakeDefinitionRun run = LakeDefinitionRun(lake, extra_flow);
	state.StartRun(run);

	/* Add new flow */
	this->water_flow[tile] += extra_flow;

	/* Array used for storing neighbor tiles of some tile.
	 */
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;


	/* The height we currently process.  We only choose tiles with at most that height, for expanding the lake.
	 * If we run out of tiles, have remaining_flow left, but not yet found an out-flow, we increase that height,
	 * i.e., we increase the depth of the lake.
	 */
	int ref_height = lake->GetSurfaceHeight();

	/* Do not consider extra_flow here, as in recursive calls (the only case where it is set, i.e. != 0) it is
	 * already added when processing the outflow tiles.
	 */
	int total_flow = this->water_flow[tile];

	/* Flow that remains to be distributed on the lake tiles */
	int remaining_flow = total_flow - lake->GetAlreadySpentFlow();

	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... total_flow = %i, remaining_flow = %i - %i = %i", total_flow, total_flow, lake->GetAlreadySpentFlow(), remaining_flow);

	/* The outflow tile might lead to Lake, whose outflow goes back to our Lake.  This can happen, if both lakes have
	 * the same surface height, and is a within our concept legal situation.  In that case, go through the whole
	 * Lake expansion code again, and maybe find another outflow tile.
	 */
	Lake *destination_lake = this->GetDestinationLake(lake->GetOutflowTile());
	if (destination_lake != NULL && state.WasAlreadyProcessed(destination_lake->GetCenterTile())) {
		lake->SetOutflowTile(INVALID_TILE);
	}

	/* Get the outflow tile of the lake.  If the lake was already processed before, and an outflow tile was found,
	 * use that one, and skip the whole lake expansion code.  The additional flow then simply goes through the lake,
	 * without changing its extent any further.
	 * If no outflow tile was found so far (case INVALID_TILE), we extend the lake until we either run out of flow,
	 * or find an outflow tile.
	 */
	TileIndex outflow_tile = lake->GetOutflowTile();

	if (outflow_tile != INVALID_TILE) {
		DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Found already calcuted outflow tile (%i,%i) and will use it, total_flow = %i, already_processed = %i",
													 TileX(outflow_tile), TileY(outflow_tile), total_flow, lake->GetAlreadyProcessedOutflow());
	} else {
		if (lake->AreUnprocessedEdgeTilesDirty()) {
			this->RecalculateUnprocessedEdgeTiles(lake);
			lake->SetUnprocessedEdgeTilesDirty(false);
		}

		DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ No outflow tile was already found, will expand lake, total_flow = %i, already_processed = %i",
													 total_flow, lake->GetAlreadyProcessedOutflow());

		/* Iterate until all flow is distributed, or an outflow tile is found.  The max heightlevel condition is for
		 * real corner cases, e.g. if the whole map is one single volcano, and flow_per_lake_volume is set to zero, the whole
		 * map might fill with water and the surface height might grow forever.  Quite improbable, but make sure that his doesn´t kill the algorithm...
		 */
		while (remaining_flow >= _settings_newgame.game_creation.rainfall.flow_per_lake_volume && outflow_tile == INVALID_TILE && ref_height < _settings_game.construction.max_heightlevel) {
			/* Tiles we want to process in this iteration of the algorithm.  Is a subset of the unprocessed_edge_tiles set above,
			 * to introduce some assymmetry (we don´t just expand in circles, but expand sometimes here and sometimes there).
			 * But do always declare tiles to lake tiles, that have sufficient flow for a river.  We don´t want river tiles in the
			 * middle of a lake.  They would just confuse following algorithms e.g. for lake modification.
			 */
			std::vector<TileIndex> tiles_this_time = std::vector<TileIndex>();
			for (std::set<TileIndex>::const_iterator it = lake->GetUnprocessedEdgeBegin(); it != lake->GetUnprocessedEdgeEnd(); it++) {
				if (this->water_flow[*it] >= _settings_newgame.game_creation.rainfall.flow_for_river || RandomRange(3) == 1) {
					tiles_this_time.push_back(*it);
				}
			}

			/* If our probabilistic approach above didn´t choose any tile, most probably just a few candidate tiles are left, thus choose them all.
			 */
			if (tiles_this_time.size() == 0) {
				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Choosing all these tiles.");
				for (std::set<TileIndex>::const_iterator it = lake->GetUnprocessedEdgeBegin(); it != lake->GetUnprocessedEdgeEnd(); it++) {
					tiles_this_time.push_back(*it);
				}
			} else {
				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Choosing " PRINTF_SIZE " tiles by random", tiles_this_time.size());
			}

			/* Process all chosen tiles, expanding the lake or eventually finding a suitable out-flow tile.
			 */
			for (std::vector<TileIndex>::const_iterator it = tiles_this_time.begin(); it != tiles_this_time.end(); it++) {
				TileIndex curr_tile = *it;

				/* Mark the tile as being processed. */
				lake->UnregisterUnprocessedEdgeTile(curr_tile);

				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Declared tile (%i,%i) a lake tile, total_flow = %i, remaining_flow is %i",
									TileX(curr_tile), TileY(curr_tile), total_flow, remaining_flow);

				/* Check wether the tile is already part of a lake. */
				if (this->tile_to_lake.find(curr_tile) != this->tile_to_lake.end()) {

					/* If yes, and it´s not part of our lake, then we have to take care of it.  If it´s already
					 * part of our lake, nothing is to be done here.
					 */
					Lake *other_lake = this->tile_to_lake[curr_tile];
					if (other_lake != lake) {
						/* Consume the lake */
						if (lake->GetNumberOfLakeTiles() < other_lake->GetNumberOfLakeTiles()) {
							/* As consuming involves moving tiles around between the different containers, always
							 * consume the smaller lake by the bigger one.  If the current lake is smaller than the other lake,
							 * exchange the two, i.e. we proceed with the other lake, and consume the current one.
							 */

							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, "Current lake (%i,%i) has less tiles (" PRINTF_SIZE " < " PRINTF_SIZE ") than the other lake (%i,%i); will switch lakes for consuming",
											TileX(tile), TileY(tile),
											lake->GetLakeTiles()->size(), other_lake->GetLakeTiles()->size(),
											TileX(other_lake->GetCenterTile()), TileY(other_lake->GetCenterTile()));
							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... total_flow = %i, remaining_flow = %i, our_already_processed = %i, other_already_processed = %i",
									  total_flow, remaining_flow, lake->GetAlreadySpentFlow(), other_lake->GetAlreadySpentFlow());
							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... other_lake_outflow: %i", other_lake->GetOutflowTile() != INVALID_TILE);
							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... flow at our lake tile %i, at other lake tile %i", this->water_flow[tile], this->water_flow[other_lake->GetCenterTile()]);

							/* The tile is now the center tile of the other lake */
							tile = other_lake->GetCenterTile();

							/* Mark the center tile of the other lake also processed, i.e. we will not recognize it for outflow etc. */
							state.MarkProcessed(tile);

							/* ConsumeLake expects a situation, where the tile at hand is in the other lake.  Thus, we transfer it to our lake,
							 * before it will be consumed by ConsumeLake.
							 */
							tile_to_lake.erase(curr_tile);
							tile_to_lake[curr_tile] = lake;
							other_lake->RemoveLakeTile(curr_tile);
							lake->AddLakeTile(curr_tile);

							/* The flow the other lake has left to distribute.  Note that if an outflow tile was already found,
							 * this will lead to a remaining flow that does not actually reflect the size of the lake.
							 * But this is no harm, as in that case we will stop expanding the lake immediately anyway.
							 */
							int remaining_other_lake_flow = this->water_flow[tile] - other_lake->GetAlreadySpentFlow();

							/* The flow of the consumed lake, in this case of the current lake.  Calculates as "total flow minus already processed outflow".
							 */
							int extra_flow = this->ConsumeLake(other_lake, curr_tile, ref_height, state, run);

							/* Add the extra flow for the other lake.  If the other_lake flows into our lake, extra_flow is zero since ConsumeLake detects that.
							 * (the other way round is impossible, since our lake hasn´t yet an outflow, otherwise we wouldn´t be in that code).
							 * In that case, we have to take the water_flow of our lake, since it is the lake downwards which likely collected more flow from
							 * other sources which we don´t want to discard.
							 */
							this->water_flow[tile] = (extra_flow == 0 ? this->water_flow[lake->GetCenterTile()] : this->water_flow[tile] + extra_flow);

							/* We have consumed some flow in this run of the function, for the tiles declared lake tiles so far.  Also, the other lake has
							 * already spent some flow for its tiles.  Don´t count the latter, if that lake actually flows into our lake.
							 */
							int already_consumed_flow = this->water_flow[tile] - remaining_flow - (extra_flow == 0 ? 0 : remaining_other_lake_flow);

							/* The total flow is the sum of the flow of both lakes; this->water_flow[tile] was already corrected inside ConsumeLake */
							total_flow = this->water_flow[tile];

							/* Subtract the already spent flow to get the remaining flow */
							remaining_flow = total_flow - already_consumed_flow;

							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL,
											".... Calculated: already_consumed_flow = %i, total_flow_new = %i, remaining_flow_new = %i, other_center_flow = %i",
											already_consumed_flow, total_flow, remaining_flow, this->water_flow[tile]);

							/* Finally switch the lake instances */
							Lake *tmp_lake = lake;
							lake = other_lake;
							other_lake = tmp_lake;
							DeclareActiveLakeCenter(this->water_info, lake->GetCenterTile());
							DeclareConsumedLakeCenter(this->water_info, other_lake->GetCenterTile());

							/* Maybe the other lake has already found an outflow tile, then we take that one.  This essentially stops expanding the lake. */
							if (extra_flow != 0) {
								outflow_tile = lake->GetOutflowTile();
								DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... Actually adding the other lake flow, since it´s flow doesn´t end in our lake");
							} else {
								DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... Dropping already calculated outflow tile since it ends in our lake, resetting alreadyProcessedOutflow to 0");
								lake->SetAlreadyProcessedOutflow(0);
								lake->SetOutflowTile(INVALID_TILE);
								outflow_tile = INVALID_TILE;
							}
						} else {
							int extra_flow = this->ConsumeLake(lake, curr_tile, ref_height, state, run);
							total_flow += extra_flow;
							remaining_flow += (extra_flow == 0 ? 0 : extra_flow - other_lake->GetAlreadySpentFlow());
							this->water_flow[tile] += extra_flow;

							if (extra_flow != 0) {
								DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, ".... Actually adding the other lake flow, since it´s flow doesn´t end in our lake");
							}

							DEBUG(map, RAINFALL_CONSUME_LAKES_LOG_LEVEL, "Adding other_lake_flow %i, total_flow now %i, remaining %i, already spent %i, tile (%i,%i), at tile %i",
										extra_flow, total_flow, remaining_flow, other_lake->GetAlreadySpentFlow(), TileX(tile), TileY(tile), this->water_flow[lake->GetCenterTile()]);
						}

						/* During lake consumption, the set of unprocessed edge tiles has to be reprocessed
						 * (since some of them are already processed by the consumed lake, whereas others have to be added
						 *  newly as part of the edge of the consumed lake)
						 * In any case: tiles_this_time is not longer valid at this time, thus we abort the loop, and start
						 * again based on the newly calculated set of unprocessed edge tiles.
						 */
						break;
					}
				} else if (IsActiveLakeCenter(this->water_info, curr_tile)) {
					/* A not yet processed active lake center, just process it. */
					DeclareConsumedLakeCenter(this->water_info, curr_tile);
					this->RegisterLakeTile(curr_tile, lake);

					remaining_flow += this->water_flow[curr_tile];
					total_flow += this->water_flow[curr_tile];
					this->water_flow[tile] += this->water_flow[curr_tile];
					DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Declaring not yet processed active lake center consumed, additional flow %i", this->water_flow[curr_tile]);
				} else if (ref_height >= state.GetMinSurfaceHeight() && this->DoesFlowEndOutsideLake(curr_tile, lake, state)) {
					/* Check wether this tile offers an out-flow out of the lake.  If yes, stop processing here, and let all the water flow there.
					 * (note: due to the probabilistic element of the algorithm above, we really have to check the whole path,
					 *        as we might easily see the situation that water flows out the already processed region, and enters
					 *        it via a not-yet-processed tile again.)
					 */
					state.DropMinSurfaceHeight();

					outflow_tile = curr_tile;
					DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "........ Declared tile (%i,%i) the outflow_tile.", TileX(outflow_tile), TileY(outflow_tile));
					break;
				}

				remaining_flow -= _settings_newgame.game_creation.rainfall.flow_per_lake_volume;

				/* We might find two lake centers within one call of this method.  Simply remove the second one,
				 * and add its flow to the first one.
				 */

				this->RegisterAppropriateNeighborTilesAsUnprocessed(lake, curr_tile, ref_height);

				/* Explicitely do not declare the outflow tile a lake tile.  It can live perfectly as river, and if it
				 * would be a lake tile, it would frequently trigger lake consumptions where in fact the upper lake
				 * just is a direct neighbor of the lower lake via the outflow tile.
				 */
				lake->AddLakeTile(curr_tile);
				this->tile_to_lake[curr_tile] = lake;
				if (!IsLakeCenter(this->water_info, curr_tile)) {
					DeclareOrdinaryLakeTile(this->water_info, curr_tile);
				}
			}

			/* Last, if no unprocessed_edge_tiles are left for the next iteration, but there is flow left, increase ref_height, decrease
			 * the remaining_flow accordingly (all already processed tiles now count again with FLOW_PER_LAKE_VOLUME).
			 */
			if (outflow_tile == INVALID_TILE && lake->GetNumberOfUnprocessedEdgeTiles() == 0 && remaining_flow >= _settings_newgame.game_creation.rainfall.flow_per_lake_volume) {
				/* If the above loop has hit the map edge, and we would have to increase the surface height, instead abort lake generation.
				 * We wait until here to avoid generating lakes that hit the map edge with just one tile, it probably looks better if the lake
				 * approaches the map edge with all tiles possible at that surface height.
				 * Then, however, we have to abort, as otherwise on maps without sea, this could lead to lakes consuming the whole landscape,
				 * as they increase their surface height all the time, without finding any outflow to the sea.
				 */
				if (lake->WasFinishedWithoutOutflow()) {
					break;
				}

				/* No tiles are left, but not all flow has yet been processed.
				 * Increase the height (i.e. increase the lake depth), and find all tiles that are now appropriate for the lake.
				 */
				ref_height++;
				lake->SetSurfaceHeight(ref_height);

				remaining_flow -= lake->GetNumberOfLakeTiles() * _settings_newgame.game_creation.rainfall.flow_per_lake_volume;

				/* Fill the unprocessed edge tiles set with the tiles for the next level. */
				std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
				for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
					TileIndex curr_tile = *it;
					DEBUG(map, 9, "........ Checking for neighbor tiles of tile (%i,%i)", TileX(curr_tile), TileY(curr_tile));
					StoreStraightNeighborTiles(curr_tile, neighbor_tiles);
					this->DiscardLakeNeighborTiles(curr_tile, neighbor_tiles, ref_height);
					for (uint n = DIR_BEGIN; n < DIR_END; n++) {
						if (neighbor_tiles[n] != INVALID_TILE) {
							DEBUG(map, 9, "............ Checking neighbor tile (%i,%i), found = %i", TileX(neighbor_tiles[n]), TileY(neighbor_tiles[n]),
										lake_tiles->find(neighbor_tiles[n]) == lake_tiles->end());
							if (!lake->IsLakeTile(neighbor_tiles[n])) {
								lake->RegisterUnprocessedEdgeTile(neighbor_tiles[n]);
							}
						}
					}
				}

				if (lake->GetNumberOfUnprocessedEdgeTiles() == 0) {
					DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "====> ABORT: Did not find another suitable neighbor tile, although remaining_flow is left.");
					break;
				}
				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... No tiles were left, increasing ref_height to %i, remaining_flow is %i, and starting again with %i tiles.",
															 ref_height, remaining_flow, lake->GetNumberOfUnprocessedEdgeTiles());
			} else {
				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... %i tiles are left with remaining_flow %i", lake->GetNumberOfUnprocessedEdgeTiles(), remaining_flow);
			}
		}

		lake->SetSurfaceHeight(ref_height);
	}

	/* If there is an outflow tile, we have to treat the water flowing out of the lake correctly.
	 * Follow the flow, and have a look what happens to that water.
	 */
	if (outflow_tile != INVALID_TILE) {
		/*  There is a corner case, where lake A has an outflow to lake B (the lake calculated in this run of the function),
		 *  and both are situated in a bigger basin.  Then, lake B needs a higher outflow than the surface height of lake A
		 *  to escape from the basin.  This, in essence, would mean a river flowing upwards, which is ugly.
		 *  Our strategy is lowering the lake outflow such that things flow downwards, by terraforming a canyon through the
		 *  barrier at the outflow.
		 *  Furthermore, we do the same by chance with some given probability.  This essentially removes the lakes, and if
		 *  one sets the probability to 100 percent, then one will get a map with just rivers, but no real lakes.
		 *  That trick might be useful if heightmaps etc. have more basins than desired.
		 */
		int prev_lake_height = state.WasPreviousLakeLower();
		int new_lake_height;
		if (prev_lake_height != -1) {
			DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Lake is higher than the previous one, will terraform paths to outflow tile (%i,%i) to height %i",
														TileX(outflow_tile), TileY(outflow_tile), prev_lake_height);
			new_lake_height = prev_lake_height;
		} else {
			if (RandomRange(MAX_RAINFALL_PROBABILITY) < _settings_newgame.game_creation.rainfall.lake_outflow_canyon_probability) {
				TileIndex center_tile = lake->GetCenterTile();

				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Choosing lake (%i,%i) to lower it to its center height by chance, will dig an outflow canyon.",
															  TileX(center_tile), TileY(center_tile));
				new_lake_height = GetTileZ(center_tile);
			} else {
				new_lake_height = -1;
			}
		}

		if (new_lake_height != -1) {
			/* We will lower the lake surface height below, ensure that this doesn´t split the lake into pieces unreachable
			 * from the outflow.  Do this by terraforming appropriate paths to the desired height right here.
			 */
			this->TerraformPathsFromCentersToOutflow(lake, outflow_tile, new_lake_height);

			/* Remove all tiles that are planned to be higher than the desired height from the lake.  Our terraforming
			 * activities above guarantee that the lake still spans till the outflow.
			 */
			lake->SetSurfaceHeight(new_lake_height);
			lake->AddLakeTile(outflow_tile);
			this->tile_to_lake[outflow_tile] = lake;
			if (!IsLakeCenter(this->water_info, outflow_tile)) {
				DeclareOrdinaryLakeTile(this->water_info, outflow_tile);
			}

			/* Reset the unprocessed edge tiles. */
			lake->ClearUnprocessedEdgeTiles();
			std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
			for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
				TileIndex tile = *it;
				TerraformTileToSlope(tile, lake->GetSurfaceHeight(), SLOPE_FLAT);
				DEBUG(map, RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL, "Terraforming tile (%i,%i) to surface height %i during outflow path treatment.", TileX(tile), TileY(tile), lake->GetSurfaceHeight());
				this->RegisterAppropriateNeighborTilesAsUnprocessed(lake, tile, lake->GetSurfaceHeight());
			}

			/* The last step is terraforming all tiles along the outflow path to the desired height, if they are higher.
			 * As we iterate over that path anyway below, this isn´t done right here.
			 * Note that we do this for all tile until including a lake center the path ends at.  This might lower the
			 * next lake center, i.e. we might have the same situation again in the next run of CreateLake, which is no
			 * harm.
			 */
		}

		lake->SetOutflowTile(outflow_tile);

		int already_processed_outflow = lake->GetAlreadyProcessedOutflow();

		TileIndex curr_out_tile = outflow_tile;
		int added_flow = total_flow - already_processed_outflow;

		DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... alreadyProcessedOutflow = %i, alreadySpentOutflow = %i, totalFlow = %i, addedFlow = %i, remaining_flow = %i, newSpentFlow = %i",
											already_processed_outflow, lake->GetAlreadySpentFlow(), total_flow, added_flow, remaining_flow, total_flow - remaining_flow);

		lake->SetAlreadySpentFlow(total_flow - remaining_flow);
		lake->SetAlreadyProcessedOutflow(total_flow);

		this->water_flow[curr_out_tile] += added_flow;
		DEBUG(map, 9, ".... Processing out-flow: Increasing flow at (%i,%i), for lake (%i,%i) by %i to %i; total_flow = %i", TileX(curr_out_tile), TileY(curr_out_tile),
													TileX(tile), TileY(tile), added_flow, this->water_flow[curr_out_tile], total_flow);

		/* Now, finally, step through the whole outflow path until it ends in the sea, at the map edge, or the next lake.
		 * In the later case, process that lake by a recursive call to this function.
		 */
		std::set<TileIndex> processed_tiles = std::set<TileIndex>();
		while ((!IsTileType(curr_out_tile, MP_WATER) || IsCoastTile(curr_out_tile)) && !IsDisappearTile(this->water_info, curr_out_tile)) {
			DEBUG(map, 9, "Processing outflow tile (%i,%i)", TileX(curr_out_tile), TileY(curr_out_tile));

			curr_out_tile = AddFlowDirectionToTile(curr_out_tile, this->water_info[curr_out_tile]);
			if (curr_out_tile == INVALID_TILE) {
				/* Outflow path ended e.g. at the map edge */
				break;
			}

			/* Mainly safety for debugging.  Per definition, flow is non-circular, but if there is a bug somewhere this condition might be violated.
		     * Detecting and fixing such cases is easier if map generation does not abort in an endless loop, and one may actually see and inspect
			 * the problematic landscape at hand.
			 */
			if (processed_tiles.find(curr_out_tile) != processed_tiles.end()) {
				DEBUG(map, 0, "WARNING: CIRCULAR FLOW. circular.  Near tile (%i,%i)", TileX(curr_out_tile), TileY(curr_out_tile));
				break;
			} else {
				processed_tiles.insert(curr_out_tile);
			}

			if ((IsTileType(curr_out_tile, MP_WATER) && !IsCoastTile(curr_out_tile))) {
				/* We have reached the coast, there is nothing to be done left. */
				break;
			}

			if (!IsLakeCenter(this->water_info, curr_out_tile)) {
				/* Increase flow along outflow path. */
				this->water_flow[curr_out_tile] += added_flow;
				DEBUG(map, 9, ".... Processing out-flow: Increasing flow at (%i,%i) by %i to %i", TileX(curr_out_tile), TileY(curr_out_tile),
															 added_flow, this->water_flow[curr_out_tile]);
			}

			/* Terraform outflow tiles to form a canyon through a possibly existing barrier at the lake outflow, if necessary.  Generally, we create an outflow
			 * path only along a flow, leading downwards, but during lake generation, e.g. lake A can flow into lake B of higher surface height, resulting in
			 * a decreased surface height of lake B, resulting in a too high outflow tile.  For such cases, we check the height of the outflow path tiles,
			 * and lower them if necessary.
			 * Note that the height of the previous lakes is checked using state.WasPreviousLakeLower above, i.e. here all we have to do is checking the river tiles
		     * along the outflow.
			 */
			if (GetTileZ(curr_out_tile) > lake->GetSurfaceHeight()) {
				DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Terraforming outflow path tile (%i,%i) to height %i in order to get a canyon for the outflow.",
															  TileX(curr_out_tile), TileY(curr_out_tile), prev_lake_height);
				TerraformTileToSlope(curr_out_tile, lake->GetSurfaceHeight(), SLOPE_FLAT);
				if (!IsLakeCenter(this->water_info, curr_out_tile)) {
					DeclareRiver(this->water_info, curr_out_tile);
				}
			}

			if (IsDisappearTile(this->water_info, curr_out_tile)) {
				/* We have hit map edge */
				break;
			} else if (IsLakeCenter(this->water_info, curr_out_tile)) {
				/* The water flows into another lake.  Thus we process that one.
				 */
				if (!state.WasAlreadyProcessed(curr_out_tile)) {
					DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, ".... Outflow found a in this run not yet processed lake center at (%i,%i), recursively (re)processing it.",
								  TileX(curr_out_tile), TileY(curr_out_tile));
					CreateLake(curr_out_tile, state, added_flow);
				}
				break;
			}
		}
	} else {
		/* If no outflow was found, at least record the flow we have already spent for adding water tiles. */
		lake->SetAlreadySpentFlow(total_flow - remaining_flow);
	}
}

void DefineLakesIterator::ProcessTile(TileIndex tile, Slope slope)
{
	if (water_flow[tile] >= _settings_newgame.game_creation.rainfall.flow_for_river) {
		if (IsActiveLakeCenter(this->water_info, tile) && this->tile_to_lake.find(tile) == this->tile_to_lake.end()) {
			/* The flow iterator has declared this a lake.  This means, this is a tile where it wasn´t able to find any
			 * neighbor tile suitable for an outflow.  Our task here is
			 * (1) modify landscape to make this a real lake (i.e. increase landscape to form a flat area, until the
			 *     lake volume is sufficient for the in-flow, or an out-flow can be found
			 * (2) Define all those tiles lake-tiles
			 * (3) If an out-flow was found, follow its flow, and recalculate the flow values, maybe declaring additional
			 *     tiles river tiles.
			 */
			LakeDefinitionState lake_definition_state = LakeDefinitionState();
			this->CreateLake(tile, lake_definition_state);
			this->create_lake_runs++;
		}
	}
}

DefineLakesIterator::DefineLakesIterator(HeightIndex *height_index, int *number_of_lower_tiles, int *water_flow, byte *water_info) : HeightLevelIterator(height_index)
{
	DEBUG(map, RAINFALL_DEFINE_LAKES_LOG_LEVEL, "Totally created %i lakes", this->create_lake_runs);
	this->number_of_lower_tiles = number_of_lower_tiles;
	this->water_flow = water_flow;
	this->water_info = water_info;
	this->tile_to_lake = std::map<TileIndex, Lake*>();
	this->lake_connected_component_calculator = new LakeConnectedComponentCalculator(this->water_info);
}

DefineLakesIterator::~DefineLakesIterator()
{
	for (uint n = 0; n < this->all_lakes.size(); n++) {
		delete this->all_lakes[n];
	}
	delete this->lake_connected_component_calculator;
}

/* ================================================================================================================== */
/* ================================ RainfallRiverGenerator ========================================================== */
/* ================================================================================================================== */

int *RainfallRiverGenerator::CalculateNumberOfLowerTiles(NumberOfLowerHeightIterator *lower_iterator)
{
	for (uint h = 0; h <= _settings_game.construction.max_heightlevel; h++) {
		/* For a particular heightlevel, first look at the flat tiles, then at those with height difference 1,
		 * and finally at the steep tiles.  Having a look at map geometry, this order does what we want, beside
		 * the fact that the heightlevel is always the one of the north corner. */
		lower_iterator->Calculate(h);
	}
	return lower_iterator->GetNumberOfLowerTiles();
}

/* ========================================= */
/* ========= Removing tiny basins ========== */
/* ========================================= */

/** This function removes small basins, based on the respective config settings
 *  (game_creation.rainfall.small_oceans_removal_factor and game_creation.rainfall.small_basins_removal_limit).
 *
 *  In detail, basins at height zero will be removed if they are smaller than MapSize() / small_oceans_removal_factor,
 *  higher basins if they are smaller than small_basins_removal_limit.  Removing in both cases works by raising land.
 *
 *  The reason for doing so is as follows:
 *  - Especially in tgp generated worlds, tiny oceans can act as endpoints for huge rivers, which looks quite unrealistic.
 *  - Tiny non ocean basins act as lake centers.  They can trigger lakes, where in fact a river flowing along them
 *    would look more realistic.  Additionally, the lake definition algorithms don´t perform that well when being
 *    executed for thousands of lakes.
 */
void RainfallRiverGenerator::RemoveSmallBasins(int *number_of_lower_tiles)
{
	BasinConnectedComponentCalculator *connected_component_calculator = new BasinConnectedComponentCalculator();

	std::set<TileIndex> raised_tiles = std::set<TileIndex>();

	for (TileIndex tile = 0; tile < MapSize(); tile++) {
		if (number_of_lower_tiles[tile] == 0 && raised_tiles.find(tile) == raised_tiles.end()) {
			/* The given tile is at the bottom of some basin. */

			int max_height = GetTileZ(tile) + 1;
			uint max_size;
			if (max_height == 1) {
				max_size = _settings_newgame.game_creation.rainfall.small_oceans_removal_factor > 0 ? MapSize() / _settings_newgame.game_creation.rainfall.small_oceans_removal_factor : 0;
			} else {
				max_size = _settings_newgame.game_creation.rainfall.small_basins_removal_limit;
			}

			/* Calculate a connected component, of at most the given limit. */
			std::set<TileIndex> basin_tiles = std::set<TileIndex>();
			connected_component_calculator->SetMaxHeight(max_height);
			connected_component_calculator->StoreConnectedComponent(basin_tiles, tile, max_size);

			/* If the calculated component is smaller than the limit, we know that we stopped because we did not find further tiles,
			 * not because we reached the limit. */
			if (basin_tiles.size() < max_size) {

				/* Thus, raise the tiles */
				for (std::set<TileIndex>::const_iterator it = basin_tiles.begin(); it != basin_tiles.end(); it++) {
					TileIndex basin_tile = *it;
					raised_tiles.insert(basin_tile);

					TerraformTileToSlope(basin_tile, max_height, SLOPE_FLAT);

					DEBUG(map, 9, "Basin near tile (%i,%i): Raising tile (%i,%i)", TileX(tile), TileY(tile), TileX(basin_tile), TileY(basin_tile));
				}
			} else {
				DEBUG(map, 9, "Basin near tile (%i,%i): Will not raise basin, too big.", TileX(tile), TileY(tile));
			}
		}
	}
}

/* =================================================== */
/* ============ Path finding inside lakes ============ */
/* =================================================== */

std::vector<TileIndex>* RainfallRiverGenerator::found_path;
std::set<TileIndex>* RainfallRiverGenerator::lake_tiles;
int RainfallRiverGenerator::max_height;

int32 RainfallRiverGenerator::LakePathSearch_EndNodeCheck(AyStar *aystar, OpenListNode *current)
{
	return current->path.node.tile == *(TileIndex*)aystar->user_target ? AYSTAR_FOUND_END_NODE : AYSTAR_DONE;
}

int32 RainfallRiverGenerator::LakePathSearch_CalculateG(AyStar *aystar, AyStarNode *current, OpenListNode *parent)
{
	return 1 + RandomRange(_settings_game.game_creation.river_route_random);
}

int32 RainfallRiverGenerator::LakePathSearch_CalculateH(AyStar *aystar, AyStarNode *current, OpenListNode *parent)
{
	return DistanceManhattan(*(TileIndex*)aystar->user_target, current->tile);
}

void RainfallRiverGenerator::LakePathSearch_GetNeighbours(AyStar *aystar, OpenListNode *current)
{
	TileIndex tile = current->path.node.tile;
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;
	StoreStraightNeighborTiles(tile, neighbor_tiles);

	aystar->num_neighbours = 0;
	for (int n = DIR_BEGIN; n < DIR_END; n++) {
		/* Only accept lake tiles, or the outflow tile.  Detect the outflow tile by checking user_target (it is in generally not part of the lake). */
		if (neighbor_tiles[n] != INVALID_TILE
			&& (RainfallRiverGenerator::max_height == -1 || GetTileZ(neighbor_tiles[n]) <= RainfallRiverGenerator::max_height)
			&& (RainfallRiverGenerator::lake_tiles->find(neighbor_tiles[n]) != RainfallRiverGenerator::lake_tiles->end()
									|| neighbor_tiles[n] == *(TileIndex*)aystar->user_target)) {
			aystar->neighbours[aystar->num_neighbours].tile = neighbor_tiles[n];
			aystar->neighbours[aystar->num_neighbours].direction = INVALID_TRACKDIR;
			aystar->num_neighbours++;
		}
	}
}

void RainfallRiverGenerator::LakePathSearch_FoundEndNode(AyStar *aystar, OpenListNode *current)
{
	RainfallRiverGenerator::found_path->clear();
	for (PathNode *path = &current->path; path != NULL; path = path->parent) {
		TileIndex tile = path->node.tile;
		RainfallRiverGenerator::found_path->push_back(tile);
	}
}

uint RainfallRiverGenerator::LakePathSearch_Hash(uint tile, uint dir)
{
	return GB(TileHash(TileX(tile), TileY(tile)), 0, RainfallRiverGenerator::LAKE_HASH_SIZE);
}

/** Given a set of tiles forming a lake, and a start and a end tile, this function calculates a path within the lake,
 *  between the two tiles, and stores the result in the vector given by reference.
 *  @return wether a path could be found
 */
bool RainfallRiverGenerator::CalculateLakePath(std::set<TileIndex> &lake_tiles, TileIndex from_tile, TileIndex to_tile, std::vector<TileIndex> &path_tiles, int max_height)
{
	RainfallRiverGenerator::found_path = &path_tiles;
	RainfallRiverGenerator::lake_tiles = &lake_tiles;
	RainfallRiverGenerator::max_height = max_height;

	AyStar finder;
	MemSetT(&finder, 0);
	finder.CalculateG = RainfallRiverGenerator::LakePathSearch_CalculateG;
	finder.CalculateH = RainfallRiverGenerator::LakePathSearch_CalculateH;
	finder.GetNeighbours = RainfallRiverGenerator::LakePathSearch_GetNeighbours;
	finder.EndNodeCheck = RainfallRiverGenerator::LakePathSearch_EndNodeCheck;
	finder.FoundEndNode = RainfallRiverGenerator::LakePathSearch_FoundEndNode;

	/* Swap from_tile and to_tile, since the output path goes from dest to start, just the other way round
	 * than one expects.
	 */
	finder.user_target = &from_tile;

	finder.Init(RainfallRiverGenerator::LakePathSearch_Hash, 1 << RainfallRiverGenerator::LAKE_HASH_SIZE);

	AyStarNode start;
	start.tile = to_tile;
	start.direction = INVALID_TRACKDIR;
	finder.AddStartNode(&start, 0);
	int result = finder.Main();
	finder.Free();

	return result == AYSTAR_FOUND_END_NODE;
}

/** The generator function.  The following steps are performed in this order when generating rivers:
 *  (1) Calculate a height index, for fast iteration over all tiles of a particular heightlevel.
 *  (2) Remove tiny basins, to (a) avoid rivers ending in tiny oceans, and (b) avoid generating too many senseless lakes.
 *  (3) Recalculate HeightIndex and NumberOfLowerTiles, as step (2) performed terraforming, and thus invalidated them.
 *  (4) Calculate flow.  Each tiles gains one unit of flow, while flowing downwards, it sums up.
 *  (6) Define lakes, i.e. decide which lake covers which tiles, and decide about the lake surface height.
 */
void RainfallRiverGenerator::GenerateRivers()
{
	/* (1) Calculate height index. */
	HeightIndex *height_index = new HeightIndex();
	NumberOfLowerHeightIterator *lower_iterator = new NumberOfLowerHeightIterator(height_index, false);

	/* (2) Remove tiny basins, based on the number-of-lower-tiles measure */
	int *calculated_number_of_lower_tiles = this->CalculateNumberOfLowerTiles(lower_iterator);
	this->RemoveSmallBasins(calculated_number_of_lower_tiles);

	/* (3) Recalculate height index, and number-of-lower-tiles.  The number-of-lower-tiles of a tile is a measure
     *     for the number of lower tiles one needs to pass to the ocean.  Flow, and later rivers, find the ocean
	 *     by choosing paths towards low number-of-lower-tiles numbers.
	 */
	height_index->Recalculate();
	lower_iterator->ReInit(true);
	calculated_number_of_lower_tiles = this->CalculateNumberOfLowerTiles(lower_iterator);

	/* (4) Now, calculate the flow for each tile.  The flow basically is based on a simulated precipitation on each
	 *     tile (currently, each tile gets one unit of precipitation, but more sophisticated schemes are possible).
	 *     On the way downwards to the ocean, flow sums up.
	 */
	CalculateFlowIterator *flow_iterator = new CalculateFlowIterator(height_index, calculated_number_of_lower_tiles);
	for (int h = _settings_game.construction.max_heightlevel; h >= 0; h--) {
		flow_iterator->Calculate(h);
	}
	int *water_flow = flow_iterator->GetWaterFlow();
	byte *water_info = flow_iterator->GetWaterInfo();

	/* (6) Define lakes, i.e. decide which tile is assigned to which lake, and which surface height lakes gain.
     *     Lakes start growing at points, where the number-of-lower-tiles iterator reported zero lower tiles.
	 *     Most code here is about correct bookkeeping regarding how much flow flows through the lakes.
	 *     In particular, the case of joining neighbor lakes is non-trivial with respect to this.
	 */
	DefineLakesIterator *define_lakes_iterator = new DefineLakesIterator(height_index, calculated_number_of_lower_tiles, water_flow, water_info);
	for (int h = _settings_game.construction.max_heightlevel; h >= 0; h--) {
		define_lakes_iterator->Calculate(h);
	}

	/* Debug code: Store the results of the iterators in a public array, to be able to query them in the LandInfoWindow.
	 *             Without that possibility, debugging those values would be really hard. */
	if (_number_of_lower_tiles != NULL) {
		free(_number_of_lower_tiles);
		_number_of_lower_tiles = NULL;
	}
	if (_water_flow != NULL) {
		free(_water_flow);
		_water_flow = NULL;
	}
	if (_water_info != NULL) {
		free(_water_info);
		_water_info = NULL;
	}

	_number_of_lower_tiles = CallocT<int>(MapSizeX() * MapSizeY());
	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		_number_of_lower_tiles[n] = calculated_number_of_lower_tiles[n];
	}
	_water_flow = CallocT<int>(MapSizeX() * MapSizeY());
	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		_water_flow[n] = water_flow[n];
	}
	_water_info = CallocT<byte>(MapSizeX() * MapSizeY());
	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		_water_info[n] = water_info[n];
	}

	/*** (Debugging code end ***/

	/* Delete iterators last, as the data arrays constructed by them are in use outside them */
	delete define_lakes_iterator;
	delete lower_iterator;
	delete flow_iterator;
	delete height_index;
}
