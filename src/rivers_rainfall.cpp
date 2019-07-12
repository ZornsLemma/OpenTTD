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
#include <math.h>
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

/** If the flow starting at the given tile ends up in a lake, this function returns the (active or consumed) lake center tile
 *  where it ends.  If flow ends elsewhere (e.g. in the sea, or at the map border), INVALID_TILE is returned.
 */
TileIndex GetLakeCenterForTile(TileIndex tile, byte *water_info)
{
	/* Flow is initially generated non-circular.  We only stick to that rule until
     * after modifying flow.  In lake/river preparation, we use flow directions
	 * for finding out the lake center tile for some tile (this function), but at the
	 * same time do river completion (adding straight neighbor tiles in terms of
	 * diagonal flow) that in corner cases can already disturb flow.
	 * (in detail, this applies to lake centers that did not gain enough flow
	 *  for actually becoming water - if those are declared river, they keep their
	 *  (invalid) flow direction north, and thus may generate a cycle).
	 * This shouldn´t be harmful for what we do using this function (setting up
     * guaranteed lake tiles to keep a lake connected during island generation),
	 * but nevertheless, we need to take care not to run into a infinite loop here.
	 *
	 * In summary, keeping track about already seen tiles here is the probably best
	 * solution for getting rid of that corner case problem.
	 */
	std::set<TileIndex> tiles_seen = std::set<TileIndex>();

	while (tile != INVALID_TILE) {
		if (IsLakeCenter(water_info, tile)) {
			return tile;
		} else if (IsTileType(tile, MP_WATER) || IsDisappearTile(water_info, tile)) {
			return INVALID_TILE;
		} else if (tiles_seen.find(tile) != tiles_seen.end()) {
			return INVALID_TILE;
		} else {
			tiles_seen.insert(tile);
			tile = AddFlowDirectionToTile(tile, water_info[tile]);
		}
	}

	return tile;
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
/* ================================== CurveFlowModificator ========================================================== */
/* ================================================================================================================== */

/** ToString function just meant for human readable log messages.
 */
const char* CurveFlowModificator::PhaseToString(Phase phase) {
	switch(phase) {
		case PHASE_STRAIGHT: return "PHASE_STRAIGHT";
		case PHASE_LEFT: return "PHASE_LEFT";
		case PHASE_RIGHT: return "PHASE_RIGHT";
		default: return "UNKNOWN_PHASE";
	}
}

/** Returns wether the given tile is suitable for (re-)directing flow to it, in the sense of flow modification.
 */
bool CurveFlowModificator::IsSuitableFlowTile(TileIndex tile)
{
	return tile != INVALID_TILE && (!IsTileType(tile, MP_WATER) || IsCoastTile(tile))
				  && !IsLakeCenter(this->water_info, tile) && !IsOrdinaryLakeTile(this->water_info, tile) && !IsDisappearTile(this->water_info, tile);
}

/** Find a start tile for the flow modifications.  As there is no guarantee of finding one,
 *  we try a lot of times before finally giving up.  Try to find an appropriate tile by
 *  starting at a random tile, and stepping downwards until either the end of flow is reached,
 *  or a suitable tile is found.
 *  Basically, we want a non-lake tile with at least somewhat flow on it.  Choose
 *  FLOW_FOR_RIVER / 2 for "somewhat" to bring some randomness into the question, where a
 *  river in a huge valley / plain starts (i.e. start with flow modifications some time
 *  before the river visually starts).
 *  @return start tile as described
 */
TileIndex CurveFlowModificator::FindStartTile(int min_equal_directions)
{
	for (int n = 0; n < 100; n++) {
		TileIndex tile = RandomTile();
		Direction last_flow_direction;
		int number_of_equal_directions = 0;
		while (this->IsSuitableFlowTile(tile)) {
			if (this->water_flow[tile] >= _settings_newgame.game_creation.rainfall.flow_for_river / 2 && number_of_equal_directions >= min_equal_directions) {
				/* Now we have found the first tile, where flow increases above the minimal amount we want to consider.  Choose
				 * a random tile between this one, and the end of flow.
				 */
				std::vector<TileIndex> candidate_tiles = std::vector<TileIndex>();
				while (this->IsSuitableFlowTile(tile)) {
					candidate_tiles.push_back(tile);
					tile = AddFlowDirectionToTile(tile, this->water_info[tile]);
				}
				TileIndex ret_tile = candidate_tiles[RandomRange(candidate_tiles.size())];
				DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, "Choosing start tile (%i,%i) for curve flow modification.", TileX(ret_tile), TileY(ret_tile));
				return ret_tile;
			} else {
				tile = AddFlowDirectionToTile(tile, this->water_info[tile]);
				if (tile != INVALID_TILE) {
					if (number_of_equal_directions > 0 && GetFlowDirection(water_info, tile) == last_flow_direction) {
						number_of_equal_directions++;
					} else {
						number_of_equal_directions = 1;
						last_flow_direction = GetFlowDirection(water_info, tile);
					}
				}
			}
		}
	}
	return INVALID_TILE;
}

bool CurveFlowModificator::FinishFlowModification(TileIndex tile)
{
	return !this->IsSuitableFlowTile(tile);
}

/** Chooses the next tile while setting up an alternative flow path.
 *  The next tile is based on the given angle and may not be higher (one corner up of the same height is allowed).
 *  @param tile base tile
 *  @param angle the angle, on a scale 0..360 degrees, north is 0 degrees, east 90 degrees.
 *  @return the next tile, or INVALID_TILE if no tile could be chosen
 */
TileIndex CurveFlowModificator::GetNextTile(TileIndex tile, int angle)
{
	int ref_height = GetTileZ(tile);

	TileIndex neighbor_tiles[DIR_COUNT];
	StoreAllNeighborTiles(tile, neighbor_tiles);
	TileIndex sorted_tiles[DIR_COUNT];
	SortTilesByAngle(neighbor_tiles, angle, sorted_tiles);

	for (int n = 0; n < DIR_COUNT; n++) {
		if (sorted_tiles[n] != INVALID_TILE) {
			int candidate_height;
			Slope candidate_slope = GetTileSlope(sorted_tiles[n], &candidate_height);
			if (candidate_height < ref_height || (candidate_height == ref_height && (candidate_slope == SLOPE_FLAT || IsSlopeWithOneCornerRaised(candidate_slope)))) {
				return sorted_tiles[n];
			}
		}
	}
	return INVALID_TILE;
}

CurveFlowModificator::CurveFlowModificator(int *water_flow, byte *water_info) : FlowModificator(water_flow, water_info) {}

bool CurveFlowModificator::ModifyFlow(int min_equal_directions)
{
	/* The flow modification consists of a sequence of tiles, if successful finally ending up in some end
	 * tile where flow proceeds along a possibly different path.
	 * Example: Starting at tile (57,32), flow originally proceeded via (58,32), (59,33) to (59,34).  Flow
     * modification might exchange that path by the path (57,32), (56,32), (56,33), (56,34), (57,35),
	 * (58,35) to (59,34).  Alternatively, flow might end up in an end-tile not present on the original
	 * path of flow.
	 *
	 * In the end, we remove the flow leaving the start tile from the original path, and add it to the
     * new, alternative one.
	 *
	 * Note that we do take care about creating no flow cycles, about removing the flow we redirect
	 * from the old path, andd adding it to the new path, but we do NOT care about the flow on
	 * other tiles near the path.  I.e. it keeps its original direction.  This should in general be
     * no harm, since our curves are too small for the tiles in between reaching the minimum river
	 * amount anyway.
	 */

	/* The current tile at the new, alternative, path. */
	TileIndex start_tile = this->FindStartTile(min_equal_directions);
	if (start_tile == INVALID_TILE) {
		/* FindStartTile did its very best, but couldn´t find a suitable start tile. */
		return true;
	}

	TileIndex curr_tile = start_tile;

	/* The current angle when creating the new path.  We do our very best to choose a tile near that
     * angle for proceeding, but if course, obstacles like mountains in the way may make choosing
     * tiles not near that angle necessary.
	 */
	int curr_angle = GetAngleFromDirection((Direction)GB(water_info[curr_tile], 0, 3));

	/* We change direction in phases, to get something looking like a rather round curve.
	 * Each phase has a number of steps, and in each step, direction is changed by a random
	 * amount of degrees in the range 0..direction_change_amount.  In case PHASE_STRAIGHT,
	 * that delta angle is added or subtracted by random, in case PHASE_LEFT, it is subtracted,
     * in case PHASE_RIGHT, it is added.
	 */
	int remaining_steps_this_phase = 0;
	int direction_change_amount = 0;
	Phase curr_phase = PHASE_STRAIGHT;

	/* An upper bound on the total number of steps.  Once we generated a path of at most that length,
	 * starting from its beginning we check each tile for non-cyclic flow, and if a tile fails that
	 * check, we throw away the tail path.
	 */
	int step_index = 0;
	int max_steps = RandomRange(30);

	std::vector<TileIndex> new_path = std::vector<TileIndex>();
	std::set<TileIndex> tiles_in_path = std::set<TileIndex>();
	new_path.push_back(curr_tile);
	tiles_in_path.insert(curr_tile);
	do {
		if (remaining_steps_this_phase == 0) {
			/* Choose a new phase.  If the old phase was left or right, make the opposite direction more
			 * probable, to have a chance of eventually coming back to a location near the original flow path,
		     * otherwise make straight paths more probable, in order to avoid changing direction all the time.
			 */
			Phase old_phase = curr_phase;
			int r = RandomRange(4);
			switch(curr_phase) {
				case PHASE_STRAIGHT: curr_phase = (r <= 1 ? PHASE_STRAIGHT : (r <= 2 ? PHASE_LEFT : PHASE_RIGHT));
									 break;
				case PHASE_LEFT:     curr_phase = (r <= 1 ? PHASE_RIGHT : (r <= 2 ? PHASE_STRAIGHT : PHASE_LEFT));
									 break;
				case PHASE_RIGHT:    curr_phase = (r <= 1 ? PHASE_LEFT : (r <= 2 ? PHASE_STRAIGHT : PHASE_RIGHT));
									 break;
				default: assert(false); // Impossible case, we take care about all legal values above
			}

			/* Big rivers in generally don´t have that narrow curves, thus use more steps of smaller angle each for their curves.
			 * Using just one bound is a rough heuristic, which might get refined further.
			 * Regarding direction_change_amount, the idea is: A curve should in average have 90 degrees.  Split up into three
			 * steps, each step has 30 degrees.  The mean of the equally distributed random variable is 30 degrees, if the maximum
			 * bound is set to 60 degrees.  In the second case: Total angle 60 degrees, one step 12 degrees, max. bound 24 degrees.
		     */
			remaining_steps_this_phase = (water_flow[curr_tile] < 5 * _settings_newgame.game_creation.rainfall.flow_for_river ? 3 : 5);
			direction_change_amount = (water_flow[curr_tile] < 5 * _settings_newgame.game_creation.rainfall.flow_for_river ? 60 : 24);

			DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, ".... Switching from phase %s to phase %s", this->PhaseToString(old_phase), this->PhaseToString(curr_phase));
		}

		/* Adjust angle */
		int delta_angle = RandomRange(direction_change_amount);
		switch(curr_phase) {
			case PHASE_STRAIGHT:
				curr_angle = curr_angle + (RandomRange(2) == 0 ? delta_angle : -delta_angle);
				break;
			case PHASE_LEFT:
				curr_angle -= delta_angle;
				break;
			case PHASE_RIGHT:
				curr_angle += delta_angle;
				break;
			default: assert(false);  // Impossible case, we process all possible values above
		}

		/* And switch to the next tile.  It is chosen by angle, including a bit of randomness and the condition that it must
		 * not be higher (just tiles of the same height with one corner up are allowed).
		 */
		curr_tile = this->GetNextTile(curr_tile, curr_angle);

		/* If we were not able to find a next tile, or built a cycle, abort.  Note that the cycle detection here is just
		 * for our new path, it does not yet search for potential cycles in the changed flow.
		 */
		if (curr_tile == INVALID_TILE || tiles_in_path.find(curr_tile) != tiles_in_path.end()) {
			break;
		}

		DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, ".... Added tile (%i,%i) to path, curr_angle is %i, flow = %i", TileX(curr_tile), TileY(curr_tile), curr_angle, this->water_flow[curr_tile]);

		/* Bookkeeping: Record new tile, adjust step indices */
		new_path.push_back(curr_tile);
		tiles_in_path.insert(curr_tile);
		remaining_steps_this_phase--;
		step_index++;
	} while (!this->FinishFlowModification(curr_tile) && step_index < max_steps);

	DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, "Finished path, finish %i, step %i, max_steps %i", !this->FinishFlowModification(curr_tile), step_index, max_steps);

	/* Now we have calculated a candidate alternative path, which works in terms of landscape, and has no cycle in itself.
	 * It may however easily introduce a cyclic flow, and cylic flow is absolutely forbidden as it breaks various parts of
	 * the algorithm.  Thus, what remains to be done is redirecting the flow for each tile on the candidate path (whose flow
	 * direction actually changed).  This unfortunately involves following the flow until it ends in a lake or the sea or
	 * the map edge.  If the new flow path introduces a cycle, we must discard this step of the redirection, and finish
	 * with the last non-cyclic step.
	 */
	bool cycle = false;
	int prev_delta_flow = -1;
	for (int n = 0; n < (int)new_path.size() - 1 && !cycle; n++) {
		curr_tile = new_path[n];
		TileIndex first_old_tile = AddFlowDirectionToTile(curr_tile, this->water_info[curr_tile]);
		TileIndex first_new_tile = new_path[n + 1];
		if (first_old_tile == first_new_tile) {
			/* Direction doesn´t change, thus there is no flow to redirect, and no danger to introduce a flow cycle either. */
			DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, ".... Nothing to be done for tile (%i,%i), since flow does not change in this step.", TileX(curr_tile), TileY(curr_tile));
		} else {
			/* First find all tiles on the alternative path.  Also detect a introduced cycle, if there is one. */
			std::set<TileIndex> seen_alternative_tiles = std::set<TileIndex>();
			seen_alternative_tiles.insert(curr_tile);
			TileIndex curr_alternative_tile = first_new_tile;
			while (this->IsSuitableFlowTile(curr_alternative_tile)) {
				if (seen_alternative_tiles.find(curr_alternative_tile) != seen_alternative_tiles.end()) {
					DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, ".... Detected cycle at (%i,%i), aborting.  Cycle tiles are:", TileX(curr_alternative_tile), TileY(curr_alternative_tile));
					for (std::set<TileIndex>::const_iterator it = seen_alternative_tiles.begin(); it != seen_alternative_tiles.end(); it++) {
						DEBUG(map, 9, "........ Tile (%i,%i)", TileX(*it), TileY(*it));
					}
					cycle = true;
					break;
				} else {
					DEBUG(map, 9, ".... Registering tile (%i,%i) as seen during cycle check", TileX(curr_alternative_tile), TileY(curr_alternative_tile));
					seen_alternative_tiles.insert(curr_alternative_tile);
					curr_alternative_tile = AddFlowDirectionToTile(curr_alternative_tile, this->water_info[curr_alternative_tile]);
				}
			}

			if (!cycle) {
				/* Subtract flow of curr_tile on old path, and add it on new path.  This might touch some tiles twice,
				 * but it is no harm, and we can´t avoid that either.
				 */
				int delta_flow = this->water_flow[curr_tile];

				/* Look wether our invariants hold */
				if (prev_delta_flow == -1 || prev_delta_flow <= delta_flow) {
					prev_delta_flow = delta_flow;
				} else {
					/* Don´t exactly throw an assertion, as inspecting problems is easier if you can have a look at the generated
					 * landscape.  But abort flow modification right now.
					 */
					DEBUG(map, 0, "WARNING: prev_delta_flow = %i is greater than delta_flow = %i; will ABORT flow modification!", prev_delta_flow, delta_flow);
					return false;
				}

				if (delta_flow < 0) {
					DEBUG(map, 0, "WARNING: delta_flow = %i < 0; will ABORT flow modification!", delta_flow);
					return false;
				}

				TileIndex curr_old_tile = first_old_tile;
				while (this->IsSuitableFlowTile(curr_old_tile)) {
					this->water_flow[curr_old_tile] -= delta_flow;
					if (this->water_flow[curr_old_tile] < 0) {
						DEBUG(map, 0, "WARNING: Trying to produce negative flow %i at (%i,%i)", this->water_flow[curr_old_tile], TileX(curr_old_tile), TileY(curr_old_tile));
						return false;
					}

					DEBUG(map, 9, "Decrementing water_flow at (%i,%i) by %i to %i", TileX(curr_old_tile), TileY(curr_old_tile), delta_flow, this->water_flow[curr_old_tile]);
					curr_old_tile = AddFlowDirectionToTile(curr_old_tile, this->water_info[curr_old_tile]);
				}
				/* Those two kinds of tiles are not suitable for continuing flow, but we have to decrement their flow at least... */
				if (curr_old_tile != INVALID_TILE && (IsLakeCenter(this->water_info, curr_old_tile) || IsDisappearTile(this->water_info, curr_old_tile))) {
					this->water_flow[curr_old_tile] -= delta_flow;
					if (this->water_flow[curr_old_tile] < 0) {
						DEBUG(map, 0, "WARNING: Trying to produce negative flow %i at (%i,%i)", this->water_flow[curr_old_tile], TileX(curr_old_tile), TileY(curr_old_tile));
						return false;
					}

					DEBUG(map, 9, "Decrementing water_flow at (%i,%i) by %i to %i", TileX(curr_old_tile), TileY(curr_old_tile), delta_flow, this->water_flow[curr_old_tile]);
				}

				int prev_flow = -1;
				TileIndex curr_alternative_tile = first_new_tile;
				while (this->IsSuitableFlowTile(curr_alternative_tile)) {
					this->water_flow[curr_alternative_tile] += delta_flow;
					if (prev_flow == -1 || prev_flow < this->water_flow[curr_alternative_tile]) {
						prev_flow = this->water_flow[curr_alternative_tile];
					} else {
						DEBUG(map, 0, "WARNING: Flow is not increasing along path, flow (%i,%i) = %i, prev_flow was %i", TileX(curr_alternative_tile), TileY(curr_alternative_tile),
							this->water_flow[curr_alternative_tile], prev_flow);
						return false;
					}

					DEBUG(map, 9, "Incrementing water_flow at (%i,%i) by %i to %i", TileX(curr_alternative_tile), TileY(curr_alternative_tile), delta_flow, this->water_flow[curr_alternative_tile]);
					curr_alternative_tile = AddFlowDirectionToTile(curr_alternative_tile, this->water_info[curr_alternative_tile]);
				}
				if (curr_alternative_tile != INVALID_TILE && (IsLakeCenter(this->water_info, curr_alternative_tile) || IsDisappearTile(this->water_info, curr_alternative_tile))) {
					this->water_flow[curr_alternative_tile] += delta_flow;
					DEBUG(map, 9, "Incrementing water_flow at (%i,%i) by %i to %i", TileX(curr_alternative_tile), TileY(curr_alternative_tile), delta_flow, this->water_flow[curr_alternative_tile]);
				}

				Direction new_direction = GetDirection(curr_tile, first_new_tile);
				DEBUG(map, RAINFALL_FLOW_MODIFICATION_LOG_LEVEL, "Redirected flow %i to new direction %s starting with tile (%i,%i)",
																  delta_flow, DirectionToString(new_direction), TileX(curr_tile), TileY(curr_tile));
				this->water_info[curr_tile] = SB(this->water_info[curr_tile], 0, 3, new_direction);
			}
		}
	}

	return true;
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

/* =================================================== */
/* ======= Helper functions about bookkeeping ======== */
/* =================================================== */

bool RainfallRiverGenerator::IsPlannedAsWater(TileIndex tile, int *water_flow, byte *water_info)
{
	/* All tiles with no outflow are declared lake centers.  This state has to persist throughout the
	 * whole calculation, since various algorithms need the information "I cannot step any tile further here.".
	 * Some lake centers never become actual lakes, because they don´t gain enough flow for at least a single
	 * lake tile.  Thus, for lake centers we also have to test wether there is enough flow for getting
	 * a visually visible lake.
	 */
	return tile != INVALID_TILE
			  && (IsRiver(water_info, tile) || (IsLakeCenter(water_info, tile) && water_flow[tile] >= _settings_newgame.game_creation.rainfall.flow_for_river) || IsOrdinaryLakeTile(water_info, tile));
}

/** Returns wether the diagonal tile given by its direction has a flow towards this tile, and none of the adjacent straight neighbor tiles are planned as water.
 *  This is the case, where we have to add a river tile, to complete the river.
 *  (we only want to generate rivers where the tiles are connected through edges, not just corners, but earlier, we sometimes generated
 *   diagonal flows in order to honor the existing landscape as best as possible)
 */
bool RainfallRiverGenerator::IsPureDiagonalFlow(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info,
							   Direction diagonal_direction, Direction straight_direction_one, Direction straight_direction_two)
{
	TileIndex diagonal_tile = neighbor_tiles[diagonal_direction];
	if (diagonal_tile != INVALID_TILE && AddFlowDirectionToTile(diagonal_tile, water_info[diagonal_tile] == tile)) {
		TileIndex straight_tile_one = neighbor_tiles[straight_direction_one];
		TileIndex straight_tile_two = neighbor_tiles[straight_direction_two];
		return !this->IsPlannedAsWater(straight_tile_one, water_flow, water_info) && !this->IsPlannedAsWater(straight_tile_two, water_flow, water_info);
	} else {
		return false;
	}
}

/** Returns how many corners of the given slope are raised, or 4 for steep slopes.
 *  Used to get a measure, which tile is the steeper one.
 */
int RainfallRiverGenerator::GetNumberOfAscendedSlopes(Slope slope)
{
	if (slope == SLOPE_FLAT) {
		return 0;
	} else if (IsSlopeWithOneCornerRaised(slope)) {
		return 1;
	} else if (IsInclinedSlope(slope) || slope == SLOPE_NS || slope == SLOPE_EW) {
		return 2;
	} else if (IsSteepSlope(slope)) {
		return 4;
	} else {
		return 3;
	}
}

void RainfallRiverGenerator::DeclareNeighborTileWater(TileIndex water_neighbor_tiles[DIR_COUNT], TileIndex neighbor_tiles[DIR_COUNT], bool add_tile, Direction direction)
{
	if (add_tile) {
		water_neighbor_tiles[direction] = neighbor_tiles[direction];
	}
}

void RainfallRiverGenerator::SetExtraNeighborTilesProcessed(TileIndex water_neighbor_tiles[DIR_COUNT], byte *water_info, std::vector<TileWithValue> &extra_river_tiles,
				bool add_tile, Direction direction, int flow)
{
	if (add_tile) {
		TileIndex tile = water_neighbor_tiles[direction];

		/* Compare comment in GetLakeCenterForTile for information about a corner case related to that code.
		 * (short: what we do about the case that we may enter this code with a lake center tile that did not gain
		 *  enough flow to become a visible lake)
		 */
		DeclareRiver(water_info, tile);
		MarkProcessed(water_info, tile);
		extra_river_tiles.push_back(TileWithValue(tile, flow));
		DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Declared extra tile (%i,%i) water because of diagonal flow.", TileX(tile), TileY(tile));
	}
}


void RainfallRiverGenerator::UpdateFlow(int *water_flow, std::vector<TileWithHeightAndFlow> &water_tiles)
{
	/*  Above, the concept of flow was used to decide where rivers and lakes should be placed.
	 *  However, so far in the water_flow array, only for active lake centers, and for the central tiles of rivers, the correct flow
	 *  values are recorded.  The river tiles that were added to rivers when making rivers wider e.g. still have their originally, usually
	 *  much smaller, flow.
	 *  Below, FineTuneTilesForWater relies on correct flow values for all water tiles, to prevent situations were water flows upwards.
	 *  (specifically, an inclined river slope may not lead from a lower tile with smaller flow, to a higher tile with bigger flow).
	 *  Thus, here we modify the water_flow array such that artificial flow values recorded during lake generation and wider river generation
	 *  are added to the array.
	 */
	for (std::vector<TileWithHeightAndFlow>::const_iterator it = water_tiles.begin(); it != water_tiles.end(); it++) {
		TileWithHeightAndFlow water_tile = *it;
		TileIndex tile = water_tile.tile;
		int flow = water_tile.flow;

		water_flow[tile] = max(water_flow[tile], flow);
	}
}

void RainfallRiverGenerator::GetProblemTiles(std::vector<TileWithHeightAndFlow> &water_tiles, std::set<TileIndex> &problem_tiles, byte *water_info)
{
	problem_tiles.clear();
	for (int n = 0; n < (int)water_tiles.size(); n++) {
		TileIndex tile = water_tiles[n].tile;
		if (WasProcessed(water_info, tile) && !IsTileSuitableForRiver(tile)) {
			problem_tiles.insert(tile);
		}
	}
}

/* ========================================================= */
/* ================ Flow modification ====================== */
/* ========================================================= */

void RainfallRiverGenerator::ModifyFlow(int *water_flow, byte *water_info) {
	CurveFlowModificator *curve_flow_modificator = new CurveFlowModificator(water_flow, water_info);
	int number_of_flow_modifications = _settings_newgame.game_creation.rainfall.number_of_flow_modifications * (MapSize() / 1000);
	for (int n = 0; n < number_of_flow_modifications; n++) {
		int min_equal_directions = n < number_of_flow_modifications / 2 ? 0 : 3;
		curve_flow_modificator->ModifyFlow(min_equal_directions);
	}
	delete curve_flow_modificator;
}

/* ========================================================= */
/* ================== Wider rivers ========================= */
/* ========================================================= */

/** Returns which bound for wide rivers the given flow reaches.  I.e. the width of a corresponding river,
 *  if wide rivers are enabled.
 *  @param flow some flow value
 *  @return the width of a corresponding river
 */
int RainfallRiverGenerator::GetWideRiverBoundForFlow(int flow)
{
	int reached_bound = 0;
	while (this->wide_river_bounds[reached_bound + 1] <= flow && reached_bound < (int)this->wide_river_bounds.size() - 1) {
		reached_bound++;
	}
	return reached_bound;
}

/** This function makes a river wider at one particular tile, and at the same time generates a wider valley at this point if the configuration suggests doing this.
 *
 *  Both for wider rivers and for wider valleys we use a scheme, where in an alternating manner tiles at both sides of the rivers are chosen and processed.
 *  E.g., if you have tiles 000T000 (with T being the start tile, 0 being neighbor tiles along some axis, 1 being processed tiles), then we might process tiles in
 *  the following order: 001T000, 001T100, 011T100, 011T110, 111T110, 111T111.
 *
 *  Technically, the function receives a tuple (dx, dy), where either dx or dy must be zero, and starts by applying the component that is not zero to the respective
 *  coordinate of TileX / TileY.
 *
 *  Wider rivers and valleys are produced in the same loop of the above scheme.  First, we choose tiles according to the river width, and all later iterations of
 *  the tile choosing loop make the valley wider.  The latter works by maintaining a current height offset, which is increased in a probabilistic manner to
 *  get not too regular looking valleys.  That probabilistic scheme works as follow:  In the first step, increase height with probability 1 / desired_valley_width,
 *  in the second step with probability 2 / desired_valley_width, and so on until the whole desired valley width is processed.  This might lead to a scheme like,
 *  river tile has height 8, extra valley tiles have height 8,8,9,10,10,11, i.e. we let the terrain ascend towards the slope of the valley, and ascending is the
 *  more probable the farer we are away from the river.
 *
 *  With respect to wider rivers, the default behaviour is to simply ignore tiles that cannot be made a river (as they are already one, or are sea tiles), but
 *  if number_of_alternatives > 0 are given, the function will instead try to choose another tile according to the algorithm described above, until the number
 *  of alternatives are exhausted.  This is for avoiding situations, where a wide river flows along a map edge, and suddendly becomes quite small just because
 *  the first chosen tile would be outside the map.
 *
 *  @param tile the water tile
 *  @param dx size of first step in x direction, if not zero, it should be 1 or -1, and if not zero, dy must be zero
 *  @param dy size of first step in y direction, if not zero, it should be 1 or -1, and if not zero, dx must be zero
 *  @param desired_width desired width of the river
 *  @param desired_height desired height of the river
 *  @param desired_slope desired slope of the river
 *  @param number_of_alternatives number of extra tries if a tile is not suitable for water, until we give up.
 *  @param water_info water info
 *  @param additional_water_tiles map from tile index to desired height, record additional water tiles here, instead of terraforming them immediately (doing so might disturb
 *                                the calling algorithm, as the same tile may be subject to this function multiple times with different directions, caused by different neighbor
 *                                tiles
 */
void RainfallRiverGenerator::MakeRiverTileWiderStraight(bool river, bool valley,
														TileIndex tile, int base_flow, int dx, int dy, int desired_width, int desired_height, Slope desired_slope, int number_of_alternatives,
														int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, int *valley_grid,
														std::map<TileIndex, HeightAndFlow> &additional_water_tiles)
{
	assert(dx == 0 || dy == 0);

	DEBUG(map, 9, ".. Calling MakeRiverTileWiderStraight with dx = %i, dy = %i, desired_width = %i, number_of_alternatives = %i",
			  dx, dy, desired_width, number_of_alternatives);

	int x = TileX(tile);
	int y = TileY(tile);
	int tile_height = GetTileZ(tile);

	/* The number of extra valley tiles at each side of the river */
	int number_of_extra_valley_tiles;
	if (_settings_newgame.game_creation.rainfall.wider_valleys_multiplier > 0) {
		int valley_modifier = valley_grid[ValleyGridXY(x / VALLEY_GRID_SIZE, y / VALLEY_GRID_SIZE)];
		int wider_valleys_multiplier = (valley_modifier * _settings_newgame.game_creation.rainfall.wider_valleys_multiplier * desired_width) / 1000;
		number_of_extra_valley_tiles = RandomRange(wider_valleys_multiplier);
	} else {
		number_of_extra_valley_tiles = 0;
	}

	/* Currently, we generate extra valley tiles with height <river height> plus this offset */
	int curr_height_offset = 0;

	/* Both wider rivers, and wider valleys in the same loop */
	for (int n = 1; n < desired_width + 2 * number_of_extra_valley_tiles; n++) {
		int offset = (n - 1) % 2;

		if (x + dx > 0 && y + dy > 0 && x + dx < (int)MapSizeX() - 1 && y + dy < (int)MapSizeY() - 1) {
			TileIndex candidate_tile = TileXY(x + dx, y + dy);
			if (!IsTileType(candidate_tile, MP_WATER) || IsCoastTile(candidate_tile)) {
				/* n < desired_widht: Tiles can be declared water.  n >= desired_width: Tile can be terraformed to valley height as described above */

				/* Case distinction between generating wider rivers, or wider valleys */
				if (river && n < desired_width) {
					if (WasProcessed(water_info, candidate_tile)) {
						water_flow[candidate_tile] = max(water_flow[candidate_tile], base_flow);
					} else {
						/* Propagate type of tile */
						if ((IsOrdinaryLakeTile(water_info, tile) || IsLakeCenter(water_info, tile)) && WasProcessed(water_info, tile)) {
							Lake *parent_lake = define_lakes_iterator->GetLake(tile);
							DeclareOrdinaryLakeTile(water_info, candidate_tile);
							parent_lake->AddLakeTile(candidate_tile);
							if (parent_lake != NULL) {
								define_lakes_iterator->RegisterLakeTile(candidate_tile, parent_lake);
							} else {
								DEBUG(misc, 0, "WARNING: No lake registered for lake tile (%i,%i)", TileX(tile), TileY(tile));
							}
						} else {
							/* If tile is an unprocessed lake center (i.e. with too small flow), we also end in the river case. */
							DeclareRiver(water_info, candidate_tile);
						}
						MarkProcessed(water_info, candidate_tile);

						DEBUG(map, 9, ".... Declared tile (%i,%i) a wider river tile", TileX(candidate_tile), TileY(candidate_tile));

						if (additional_water_tiles.find(candidate_tile) != additional_water_tiles.end()) {
							HeightAndFlow old_state = additional_water_tiles[candidate_tile];
							additional_water_tiles.erase(candidate_tile);
							additional_water_tiles[candidate_tile] = HeightAndFlow(max(old_state.height, tile_height), max(old_state.flow, base_flow));

							if (old_state.height < tile_height) {
								DEBUG(map, 9, ".... Increased desired height of tile (%i,%i) from %i to %i, based on tile (%i,%i), flow %i",
										TileX(candidate_tile), TileY(candidate_tile), old_state.height, max(old_state.height, tile_height), TileX(tile), TileY(tile), base_flow);
							}
						} else {
							additional_water_tiles[candidate_tile] = HeightAndFlow(tile_height, base_flow);
							DEBUG(map, 9, ".... Registered desired height of tile (%i,%i) as %i, based on tile (%i,%i), flow %i",
										TileX(candidate_tile), TileY(candidate_tile), tile_height, TileX(tile), TileY(tile), base_flow);
						}
					}
				}
				if (valley && n >= desired_width && !IsTileType(candidate_tile, MP_WATER)) {
					/* Terraform tile for wider valley, but only if it is higher, we don´t want to accidentally raise terrain */
					if (GetTileZ(candidate_tile) > tile_height + curr_height_offset) {
						TerraformerState terraformer_state;
						int h = tile_height + curr_height_offset;
						bool success = SimulateTerraformTileToSlope(candidate_tile, h, SLOPE_FLAT, terraformer_state);
						if (success) {
							bool veto = false;
							for (TileIndexToHeightMap::const_iterator it = terraformer_state.tile_to_new_height.begin(); it != terraformer_state.tile_to_new_height.end(); it++) {
								TileIndex affected_tile = it->first;

								if ((IsRiver(water_info, affected_tile) && water_flow[affected_tile] > water_flow[tile] / 2)
									|| IsOrdinaryLakeTile(water_info, affected_tile) || IsLakeCenter(water_info, affected_tile)) {
									veto = true;
								}
							}
							if (!veto) {
								DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Terraforming tile (%i,%i) to height %i while widening valleys, base tile (%i,%i) with height %i",
																					TileX(candidate_tile), TileY(candidate_tile), tile_height + curr_height_offset, TileX(tile), TileY(tile), tile_height);
								ExecuteTerraforming(terraformer_state);
							}
						}
					}
				}
			} else {
				DEBUG(map, 9, ".... Ignoring tile (%i,%i) since it is already water or sea", TileX(candidate_tile), TileY(candidate_tile));
				if (number_of_alternatives > 0) {
					n--;
					number_of_alternatives--;
				}
			}
		}

		DEBUG(map, 9, ".... dx = %i, dy = %i, offset = %i", dx, dy, offset);
		if (dx == 0) {
			/* Add tiles alternately: If we start with dy = -1, add: y - 1 in step n == 1, y + 1 in step n == 2, y - 2 in
			 * step n == 3, and so on.
			 */
			dy = (dy < 0 ? -dy + offset : -dy - offset);
			DEBUG(map, 9, ".... case dx == 0, now dy = %i", dy);
		} else {
			dx = (dx < 0 ? -dx + offset : -dx - offset);
			DEBUG(map, 9, ".... case dy == 0, now dx = %i", dx);
		}

		/* Increase height offset with probability <current extra valley tile> / <number of extra valley tiles> */
		int valley_offset = n - desired_width >= 0 ? n - desired_width / 2 : -1;
		if (valley_offset >= 0) {
			curr_height_offset += ((valley_offset * 1000) / number_of_extra_valley_tiles < (int)RandomRange(1000) ? 1 : 0);
		}
	}
}

/** This function tries to make a river or a guaranteed lake path wider at the given tile, assuming that flow has the given direction.
 *  Depending on the direction, it trys some neighbor tiles.
 */
void RainfallRiverGenerator::MakeRiversWiderByDirection(bool river, bool valley, TileIndex tile, Direction direction,
														int reached_bound, int height, Slope slope, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, int *valley_grid,
														std::map<TileIndex, HeightAndFlow> &additional_water_tiles)
{
	uint x = TileX(tile);
	uint y = TileY(tile);
	int base_flow = water_flow[tile];

	/* We aim to declare tiles orthogonal to the flow direction river tiles.  For straight flow this is easy, however for
	 * diagonal flow we need to try some more tiles in the neighborhood in order to be sure that we choose additional tiles.
	 */
	switch (direction) {
		case DIR_S:
			if (x < MapMaxX() - 1) {
				this->MakeRiverTileWiderStraight(river, valley, TileXY(x + 1, y), base_flow, 0, -1, reached_bound, height, slope, 1,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			}
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 0, -1, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 1, 0, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			break;
		case DIR_SE: this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 1, 0, reached_bound, height, slope, 0,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles); break;

		case DIR_E:
			if (y < MapMaxX() - 1) {
				this->MakeRiverTileWiderStraight(river, valley, TileXY(x, y + 1), base_flow, 1, 0, reached_bound, height, slope, 1,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			}
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 0, 1, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 1, 0, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			break;

		case DIR_NE: this->MakeRiverTileWiderStraight(river, valley, tile, base_flow,0, 1, reached_bound, height, slope, 0,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles); break;

		case DIR_N:
			if (x > 1) {
				this->MakeRiverTileWiderStraight(river, valley, TileXY(x - 1, y), base_flow, 0, 1, reached_bound, height, slope, 1,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			}
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 0, 1, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, -1, 0, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			break;

		case DIR_NW: this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, -1, 0, reached_bound, height, slope, 0,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles); break;

		case DIR_W:
			if (y > 1) {
				this->MakeRiverTileWiderStraight(river, valley, TileXY(x, y - 1), base_flow, -1, 0, reached_bound, height, slope, 1,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			}
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 0, -1, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, -1, 0, reached_bound, height, slope, 1, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			break;

		case DIR_SW : this->MakeRiverTileWiderStraight(river, valley, tile, base_flow, 0, -1, reached_bound, height, slope, 0,
								  water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles); break;
		default: NOT_REACHED();
	}
}

/** This function aims at making rivers with huge flow wider.  The background is just: The Amazonas is a bit wider than the small
 *  river in your backyard.
 *
 *  The width of a river as generated by this function is a logarithmic function of its flow.  For this purpose, in the settings
 *  a multiplier is defined which is iteratively multiplied with the water flow necessary to form a river at all.
 *  Example: If the water flow necessary for a river is 300, and the multiplier is ten, then rivers with flow greater than 3000
 *  will be made two tiles wide by this function, rivers with flow greater than 30000 three tiles wide, and so on.
 *
 *  By using such a logarithmic approach, the danger that a river with huge flow exceeds all bounds and becomes maybe some dozens
 *  of tiles wide doesn´t exist.
 *
 *  @param water_flow the water flow as calculated before
 *  @param water_info the status information (where are rivers, flow direction, etc.) as calculated before
 *  @param water_tiles the tiles declared water so far, this function will only inspect such tiles
 */
void RainfallRiverGenerator::DoGenerateWiderRivers(bool river, bool valley, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, int *valley_grid, std::vector<TileWithHeightAndFlow> &water_tiles)
{
	std::map<TileIndex, HeightAndFlow> additional_water_tiles = std::map<TileIndex, HeightAndFlow>();
	for (int n = 0; n < (int)water_tiles.size(); n++) {
		TileIndex tile = water_tiles[n].tile;

		/* Determine the width the flow of the current tile is sufficient for */
		int reached_bound = this->GetWideRiverBoundForFlow(water_flow[tile]);

		/* We have to do something with this river tile, if it either needs to become width > 1 (rivers with width == 1 are already
		 * calculated completely at this point), or we need to generate a wider valley.
		 */
		if (reached_bound > 1 || _settings_newgame.game_creation.rainfall.wider_valleys_enabled > 0) {
			int height;
			Slope slope = GetTileSlope(tile, &height);

			if (   (IsOrdinaryLakeTile(water_info, tile) || IsLakeCenter(water_info, tile))
				&&  IsGuaranteed(water_info, tile)) {
				/* Guaranteed lake tiles: Also process them, to avoid paths of guaranteed lake tiles being the smallest parts of a whole river.
				 * This can happen especially if lakes are reduced to their guaranteed tiles using the settings
				 */

				/* Lake tiles don´t have reliable flow direction information, as paths of guaranteed lake tiles are calculated
				 * without any regard to flow directions.  Thus, inspect all neighbor tiles, filter for those that are also guaranteed
				 * and have sufficient flow, calculate the directions to those neighbor tiles, and then work as if it was a river
				 * with that direction.
				 */
				TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;
				StoreAllNeighborTiles(tile, neighbor_tiles);
				for (int n = DIR_BEGIN; n < DIR_END; n++) {
					if (neighbor_tiles[n] != INVALID_TILE && IsGuaranteed(water_info, neighbor_tiles[n]) && water_flow[neighbor_tiles[n]] >= this->GetFlowNeededForWideRiverBound(reached_bound)) {
						Direction direction = GetDirection(tile, neighbor_tiles[n]);
						DEBUG(map, 9, "Making lake guaranteed path from (%i,%i) to (%i,%i) with direction %s wider; it will receive width %i for flow %i",
										  TileX(tile), TileY(tile), TileX(neighbor_tiles[n]), TileY(neighbor_tiles[n]), DirectionToString(direction), reached_bound, water_flow[tile]);
						this->MakeRiversWiderByDirection(river, valley, tile, direction, reached_bound, height, slope, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
					}
				}
			} else {
				/* The river case */

				DEBUG(map, 9, "River at (%i,%i) will receive width %i for flow %i", TileX(tile), TileY(tile), reached_bound, water_flow[tile]);

				/* We have to have a look on the direction of the river.  Do our very best to choose the tiles near the river that
			     * give the impression of a wide river when being declared river tiles.
				 * Note that we always choose tiles at the same side of the river (e.g., in direction north-west to south-east, we always choose the south-west tile
				 * for width 2), as the player cannot see anyway which river tile was the original one.
				 */
				Direction direction = GetFlowDirection(water_info, tile);
				this->MakeRiversWiderByDirection(river, valley, tile, direction, reached_bound, height, slope, water_flow, water_info, define_lakes_iterator, valley_grid, additional_water_tiles);
			}
		}
	}

	for (std::map<TileIndex, HeightAndFlow>::const_iterator it = additional_water_tiles.begin(); it != additional_water_tiles.end(); it++) {
		TileIndex curr_tile = it->first;
		int desired_height = it->second.height;
		int flow = it->second.flow;

		TerraformTileToSlope(curr_tile, desired_height, SLOPE_FLAT);
		water_tiles.push_back(TileWithHeightAndFlow(curr_tile, 0, flow));

		DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Terraformed tile (%i,%i) to height %i with flow %i while making rivers wider", TileX(curr_tile), TileY(curr_tile), desired_height, flow);
	}
}

/** Modifies the valley grid.  Performs the given number of steps.  In each step, a rectangular section
 *  of the given radius will be altered.  Each value in that region will be altered by the same offset, that
 *  is chosen in a random manner out of the range [-max_offset / 2, max_offset / 2].
 *  If init is true, only values that are -1 will be altered.
 *  Values will never be allowed to leave the range 0..1000.
 */
void RainfallRiverGenerator::ModifyValleyGrid(int *valley_grid, int number_of_steps, int radius, int max_offset, bool init)
{
	for (int n = 0; n < number_of_steps; n++) {
		ValleyGridIndex center = RandomRange(GetNumberOfValleyGrids());
		int grid_x = ValleyGridX(center);
		int grid_y = ValleyGridY(center);

		int offset = (int)RandomRange(max_offset) - max_offset / 2;

		DEBUG(map, 9, "Choosing center (%i,%i), radius %i, offset %i", grid_x, grid_y, radius, offset);

		for (int x = max(0, grid_x - radius); x < min((int)(MapSizeX() / VALLEY_GRID_SIZE), grid_x + radius); x++) {
			for (int y = max(0, grid_y - radius); y < min((int)(MapSizeY() / VALLEY_GRID_SIZE), grid_y + radius); y++) {
				ValleyGridIndex v = ValleyGridXY(x, y);
				if (init) {
					if (valley_grid[v] == -1) {
						valley_grid[v] = Clamp(500 + offset, 0, 1000);
					}
				} else {
	  				valley_grid[v] = Clamp(valley_grid[v] + offset, 0, 1000);
				}

				DEBUG(map, 9, ".... Modifying (%i,%i) to %i", x, y, valley_grid[v]);
			}
		}
	}
}

/** Initializes the grid of multipliers used for generating wider valley widths.
 *  In a first step, large sections of 10x10 grid sections (i.e. 160x160 tiles) are
 *  chosen in a random manner, and initialized to the same value.  An already distributed
 *  value will *not* be overwritten again in this step.  Thus, we get a mosaic of bigger
 *  and smaller areas set to the same value.  Indices that receive no value will be set to
 *  the default value 500 (i.e. half the default width) afterwards.  This way, things get
 *  less random if the wider_valleys_randomness is decreased.  The default value 1000 is
 *  chosen such that each index will in average be attempted to be written two times.
 *  But... things are random.
 *
 *  Then, in two further steps, bigger and smaller sections of the grid are altered by
 *  applying some bigger or smaller offset, that is also chosen in a random manner.
 *  This way, the bigger wider_valleys_randomness is, the more diverse the values will
 *  become, as the areas overwrite each other more often.
 *
 *  Example: The idea is that wider_valleys_randomness trigers grid values like:
 *
 *           0                  100                1000                10000
 *
 *    500 500 500 500     351 351 500 500     779 779 779 500      779 939 339 509
 *    500 500 500 500     351 351 500 500     830 850 500 500      123  84 393 129
 *    500 500 500 500     500 500 500 500     112 449 531 493      549 129 789 999
 *    500 500 500 500     500 500 500 893     112 449 531 505      123 299 505 431
 */
void RainfallRiverGenerator::InitializeValleyGrid(int *valley_grid)
{
	for (ValleyGridIndex v = 0; v < GetNumberOfValleyGrids(); v++) {
		valley_grid[v] = -1;
	}

	int randomness = _settings_newgame.game_creation.rainfall.wider_valleys_randomness;
	this->ModifyValleyGrid(valley_grid, (randomness * (GetNumberOfValleyGrids() / 200)) / 1000, 10, 1000, true);

	for (ValleyGridIndex v = 0; v < GetNumberOfValleyGrids(); v++) {
		if (valley_grid[v] == -1) {
			valley_grid[v] = 500;
		}
	}

	this->ModifyValleyGrid(valley_grid, (randomness * (GetNumberOfValleyGrids() / 100)) / 1000, 8, 600, false);
	this->ModifyValleyGrid(valley_grid, (randomness * (GetNumberOfValleyGrids() / 30)) / 1000, 4, 200, false);

	for (ValleyGridIndex v = 0; v < GetNumberOfValleyGrids(); v++) {
		DEBUG(map, 9, "Result: ValleyGrid(%i,%i) = %i", ValleyGridX(v), ValleyGridY(v), valley_grid[v]);
	}
}

void RainfallRiverGenerator::GenerateWiderRivers(int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator, std::vector<TileWithHeightAndFlow> &water_tiles)
{
	/* Set up multipliers on wide valley widths in a probabilistic manner.  Controlled by the wider_valleys_randomness.  Each index of the grid
	 * corresponds to a 16x16 section of tiles on map.  I.e. the aim is generating larger sections of map with the same kind of behaviour.
	 * E.g. one valley that is quite narrow, and 100 tiles away another valley that is quite wide.
	 */
	int *valley_grid = CallocT<int>(GetNumberOfValleyGrids());
	this->InitializeValleyGrid(valley_grid);

	/* If configured, generate wider rivers or valleys.  The scheme for aquiring the affected tiles is the same
	 * in both cases.
	 */
	if (_settings_newgame.game_creation.rainfall.wider_rivers_enabled > 0) {
		this->DoGenerateWiderRivers(true, false, water_flow, water_info, define_lakes_iterator, valley_grid, water_tiles);
	}
	if (_settings_newgame.game_creation.rainfall.wider_valleys_enabled > 0) {
		this->DoGenerateWiderRivers(false, true, water_flow, water_info, define_lakes_iterator, valley_grid, water_tiles);
	}

	delete valley_grid;
}

/* ========================================================= */
/* ======= Prepare Lakes, Terraform to surface height ====== */
/* ========================================================= */

/** This function discards a random region of a lake, starting at a given tile.
 *  @param start_tile start tile of the discarded region
 *  @param total_number not more than this number of tiles will be discarded
 *  @param lake_tiles tiles of the lake at hand
 *  @param guaranteed_water_tiles guaranteed water tiles, they will never be discarded
 *  @param discarded_lake_tiles set of the so far discarded tiles, will be subsequently expanded
 *  @param stop_if_guaranteed if this flag is false, the discarded region will expand over guaranteed lake tiles while of course not discarding them.
 *                            If the flag is true, the neighborhood of such a tile will not be considered for expansion.
 *                            Example: Tiles AGB, G is guaranteed, starting at A.  B can only be discarded if the flag is false.
 */
void LakeModificator::DiscardRegion(TileIndex start_tile, int total_number, std::set<TileIndex>* lake_tiles, std::set<TileIndex> &guaranteed_water_tiles, std::set<TileIndex> &discarded_lake_tiles,
									bool stop_if_guaranteed)
{
	DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "Will discard a region of at most %i tiles starting at tile (%i,%i)",
						total_number, TileX(start_tile), TileY(start_tile));

	/* Array that will be filled when the neighbors of a tile are calculated. */
	TileIndex neighbor_tiles[DIR_COUNT] = { INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE };

	/* Tiles this call to this function has already processed.  We will never inspect a tile twice in one call to the function. */
	std::set<TileIndex> tiles_already_processed = std::set<TileIndex>();

	/* Tiles that are chosen as candidate tiles for discarding them.  They form the outer boundary of the so far inspected region. */
	std::set<TileIndex> curr_edge_tiles = std::set<TileIndex>();
	curr_edge_tiles.insert(start_tile);

	int number_of_tiles_left = total_number;

	/* Expand the region, until either no more tiles can be found, or the limit is reached. */
	while (curr_edge_tiles.size() > 0 && number_of_tiles_left > 0) {

		/* In each iteration of the while loop, process only a random fraction of the curr_edge_tiles.
		 * This aims at expanding the region in an irregular way.  If we would always choose all tiles,
		 * we would form a circle (given that all tiles are suitable).  By choosing just some, we expand
		 * sometimes here and sometimes there.
		 */
		std::vector<TileWithValue> tiles_this_time = std::vector<TileWithValue>();
		for (std::set<TileIndex>::const_iterator it = curr_edge_tiles.begin(); it != curr_edge_tiles.end(); it++) {
			if (RandomRange(3) == 0) {
				tiles_this_time.push_back(TileWithValue(*it, RandomRange(10000)));
			}
		}

		/* Also, process the tiles in arbitrary order, in order to exclude any bias
		 * favouring one direction over another (this might happen, as tiles in a set
		 * are implicitely sorted by TileIndex, thus without sorting by a random value,
		 * we would always process them in the same order.
		 */
		std::sort(tiles_this_time.begin(), tiles_this_time.end());

		DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, ".... Starting iteration with " PRINTF_SIZE " edge tiles; choosing " PRINTF_SIZE " of them by chance.",
								curr_edge_tiles.size(), tiles_this_time.size());

		for (int n = 0; n < (int)tiles_this_time.size() && number_of_tiles_left > 0; n++) {
			TileIndex tile = tiles_this_time[n].tile;
			DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "........ Inspecting tile (%i,%i), %i tiles left", TileX(tile), TileY(tile), number_of_tiles_left);

			/* Discard the tile, if not guaranteed and not yet discarded. */
			if (guaranteed_water_tiles.find(tile) == guaranteed_water_tiles.end() && discarded_lake_tiles.find(tile) == discarded_lake_tiles.end()) {
				DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "............. Discarding it.");
				discarded_lake_tiles.insert(tile);
				number_of_tiles_left--;
			}

			curr_edge_tiles.erase(tile);
			tiles_already_processed.insert(tile);

			StoreStraightNeighborTiles(tile, neighbor_tiles);
			for (int n = DIR_BEGIN; n < DIR_END; n++) {
				if (neighbor_tiles[n] != INVALID_TILE) {
					TileIndex neighbor_tile = neighbor_tiles[n];

					/* Record a neighbor tile if it is member of the lake, not yet processed, and (only if the flag is set) not guaranteed. */
					if (lake_tiles->find(neighbor_tile) != lake_tiles->end() && tiles_already_processed.find(neighbor_tile) == tiles_already_processed.end()
							&& (!stop_if_guaranteed || guaranteed_water_tiles.find(neighbor_tile) == guaranteed_water_tiles.end())) {
						curr_edge_tiles.insert(neighbor_tile);
						DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "............. Storing neighbor tile (%i,%i) for later processing.", TileX(neighbor_tile), TileY(neighbor_tile));
					}
				}
			}
		}
	}
}

void OnlyGuaranteedLakeModificator::ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
													   std::set<TileIndex> &discarded_lake_tiles)
{
	std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		if (guaranteed_water_tiles.find(*it) == guaranteed_water_tiles.end()) {
			discarded_lake_tiles.insert(*it);
		}
	}
}

void IslandLakeModificator::ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
													   std::set<TileIndex> &discarded_lake_tiles)
{
	std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		if (RandomRange(MAX_RAINFALL_PROBABILITY) < _settings_newgame.game_creation.rainfall.lake_island_probability) {
			TileIndex tile = *it;
			TileIndex max_size = RandomRange(_settings_newgame.game_creation.rainfall.lake_island_max_size);

			DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "Will discard at most %i lake tiles to form a (potential) island near (%i,%i in lake (%i,%i)",
						max_size, TileX(tile), TileY(tile), TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()));
			this->DiscardRegion(tile, max_size, lake_tiles, guaranteed_water_tiles, discarded_lake_tiles, true);
		}
	}
}

void ExpandShoreLakeModificator::ModifyLake(Lake *lake, std::map<TileIndex, TileIndex> &inflow_tile_to_center, std::set<TileIndex> &guaranteed_water_tiles,
													   std::set<TileIndex> &discarded_lake_tiles)
{
	std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		TileIndex tile = *it;
		TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;
		StoreStraightNeighborTiles(tile, neighbor_tiles);

		int number_of_land_tiles = 0;
		for (int n = DIR_BEGIN; n < DIR_END; n++) {
			if (neighbor_tiles[n] != INVALID_TILE && lake_tiles->find(neighbor_tiles[n]) == lake_tiles->end()) {
				number_of_land_tiles++;
			}
		}

		if (number_of_land_tiles > 0 && RandomRange(MAX_RAINFALL_PROBABILITY) < _settings_newgame.game_creation.rainfall.lake_shore_probability) {
			TileIndex max_size = RandomRange(_settings_newgame.game_creation.rainfall.lake_shore_max_size);

			DEBUG(map, RAINFALL_DISCARDED_LAKE_REGION_LOG_LEVEL, "Will discard at most %i lake tiles to expand the shore near (%i,%i in lake (%i,%i)",
						max_size, TileX(tile), TileY(tile), TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()));
			this->DiscardRegion(tile, max_size, lake_tiles, guaranteed_water_tiles, discarded_lake_tiles, true);
		}
	}
}

/** This function prepares a lake for actually being filled with water tiles using MakeRiver.
 *  @param tile an active lake center
 *  @param water_flow the water flow calculated before
 *  @param water_info the water information calculated before
 *  @param lake_iterator the DefineLakesIterator that initially defined where lakes spread, what tiles they have,
 *                       and where their outflow is
 *  @param extra_water_tiles a set of tiles, where this function must insert any tile declared to be a lake tile
 */
void RainfallRiverGenerator::PrepareLake(TileIndex tile, int *water_flow, byte *water_info, DefineLakesIterator *lake_iterator, std::vector<TileWithValue> &extra_water_tiles)
{
	Lake *lake = lake_iterator->GetLake(tile);

	std::set<TileIndex>* lake_tiles = lake->GetLakeTiles();

	DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Will calculate guaranteed lake tiles for lake at (%i,%i)", TileX(tile), TileY(tile));
 	lake->PrintToDebug(RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, 9);

	TileIndex outflow_tile = lake->GetOutflowTile();
	if (outflow_tile == INVALID_TILE) {
		std::set<TileIndex>::const_iterator lake_tiles_it = lake_tiles->begin();
		std::advance(lake_tiles_it, RandomRange(lake_tiles->size()));
		outflow_tile = *lake_tiles_it;
		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Choosing tile (%i,%i) as outflow tile by chance, for the sake of having a connected lake.", TileX(outflow_tile), TileY(outflow_tile));
	}

	/* Determine the inflow tiles of the lake, and at the same time, calculate the corresponding center tile.
	 */
	std::map<TileIndex, TileIndex> inflow_tile_to_center = std::map<TileIndex, TileIndex>();
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		TileIndex curr_tile = *it;
		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Inspecting lake tile (%i,%i)", TileX(curr_tile), TileY(curr_tile));

		StoreAllNeighborTiles(curr_tile, neighbor_tiles);
		for (int n = DIR_BEGIN; n < DIR_END; n++) {
			if (neighbor_tiles[n] != INVALID_TILE
				&& lake_tiles->find(neighbor_tiles[n]) == lake_tiles->end()
				&& (outflow_tile == INVALID_TILE || outflow_tile != neighbor_tiles[n])
				&& (    (IsRiver(water_info, neighbor_tiles[n]) && lake_tiles->find(AddFlowDirectionToTile(neighbor_tiles[n], water_info[neighbor_tiles[n]])) != lake_tiles->end())
					 ||  IsOrdinaryLakeTile(water_info, neighbor_tiles[n])
					 ||  IsConsumedLakeCenter(water_info, neighbor_tiles[n])
					 || (IsActiveLakeCenter(water_info, neighbor_tiles[n]) && water_flow[neighbor_tiles[n]] >= _settings_newgame.game_creation.rainfall.flow_for_river))) {

				DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Pre-considering tile (%i,%i) for inflow", TileX(curr_tile), TileY(curr_tile));
				TileIndex center_tile = GetLakeCenterForTile(curr_tile, water_info);
				if (center_tile != INVALID_TILE && lake_tiles->find(center_tile) != lake_tiles->end()) {
					inflow_tile_to_center[curr_tile] = center_tile;
					DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Considering tile (%i,%i) an inflow tile, corresponding center tile (%i,%i)", TileX(curr_tile), TileY(curr_tile),
								  TileX(center_tile), TileY(center_tile));
				}
				break;
			}
		}
	}

	/* Afterwards, we maybe will improve using some heuristic algorithms.  They might add peninsulas and islands,
     * dig the outflow lower into the terrain, maybe drop the lake at all and transform it into a river basin flowing
	 * through a plain having the heightlevel of the lake surface.
     *
     * However, one thing is forbidden for those algorithms: They may not cut the connection between inflow and outflow.
     * Thus, our aim here is calculating paths of guaranteed water tiles between inflow and outflow.
     */

	/* Keep track about what tiles we already declared to be guaranteed. */
	std::set<TileIndex> guaranteed_water_tiles = std::set<TileIndex>();

	/* Obviously, this is only necessary if there is actually an outflow.  Copy the inflow tiles, as the following
	 * algorithm will step-wise empty the set, and the following lake modificators may need them.
	 */
	std::map<TileIndex, int> inflow_tile_to_flow = std::map<TileIndex, int>();
	for (std::map<TileIndex, TileIndex>::const_iterator it = inflow_tile_to_center.begin(); it != inflow_tile_to_center.end(); it++) {
		TileIndex inflow_tile = it->first;
		TileIndex center_tile = it->second;

		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Processing inflow tile (%i,%i) with corresponding center (%i,%i)",
					TileX(inflow_tile), TileY(inflow_tile), TileX(center_tile), TileY(center_tile));

		TileIndex curr_tile = inflow_tile;
		while (curr_tile != center_tile) {
			/* If flow direction is diagonal, we make one of the corresponding corner tiles guaranteed, as this situation
			 * might otherwise split up the lake.
			 */
			MarkCornerTileGuaranteed(water_flow, water_info, lake_tiles, guaranteed_water_tiles, curr_tile, DIR_E, DIR_NE, DIR_SE);
			MarkCornerTileGuaranteed(water_flow, water_info, lake_tiles, guaranteed_water_tiles, curr_tile, DIR_W, DIR_NW, DIR_SW);
			MarkCornerTileGuaranteed(water_flow, water_info, lake_tiles, guaranteed_water_tiles, curr_tile, DIR_N, DIR_NW, DIR_NE);
			MarkCornerTileGuaranteed(water_flow, water_info, lake_tiles, guaranteed_water_tiles, curr_tile, DIR_S, DIR_SW, DIR_SE);

			guaranteed_water_tiles.insert(curr_tile);
			MarkGuaranteed(water_info, curr_tile);
			DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "........ Marking (%i,%i) a guaranteed lake tile.", TileX(curr_tile), TileY(curr_tile));

			curr_tile = AddFlowDirectionToTile(curr_tile, water_info[curr_tile]);
		}

		guaranteed_water_tiles.insert(center_tile);
		MarkGuaranteed(water_info, center_tile);
		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "........ Marking (%i,%i) a guaranteed lake tile.", TileX(center_tile), TileY(center_tile));

		if (outflow_tile != INVALID_TILE && DistanceManhattan(inflow_tile, center_tile) >= DistanceManhattan(inflow_tile, outflow_tile)) {
			inflow_tile_to_flow[inflow_tile] = water_flow[inflow_tile];
			DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Making (%i,%i) a guaranteed center with flow %i", TileX(inflow_tile), TileY(inflow_tile), water_flow[inflow_tile]);
			DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Adding (%i,%i) to inflow_tile_to_flow since it is nearer to the outflow than to its center.",
						TileX(inflow_tile), TileY(inflow_tile));
		}
		inflow_tile_to_flow[center_tile] = water_flow[center_tile];
		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Making (%i,%i) a guaranteed center with flow %i", TileX(center_tile), TileY(center_tile), water_flow[center_tile]);
	}

	/* Make paths towards the outflow_tile guaranteed. */
	if (outflow_tile != INVALID_TILE) {
		DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Outflow tile is (%i,%i), inflow tiles left: " PRINTF_SIZE "", TileX(outflow_tile), TileY(outflow_tile), inflow_tile_to_flow.size());

		/* Guaranteed tiles on paths from an inflow tile towards the corresponding lake center tile */
		std::set<TileIndex> guaranteed_tiles_for_inflow = std::set<TileIndex>();
		guaranteed_tiles_for_inflow.insert(guaranteed_water_tiles.begin(), guaranteed_water_tiles.end());

		/* In order to declare not too many tiles guaranteed, we want to offer them the possibility to join.  E.g. if two lake centers (active and consumed)
		 * are located relatively close to each other, but far away from the outflow tile, they can largely share a path towards the outflow tile.
		 * Register not every tile in that set (as this would become expensive in terms of computation), but at least some.
		 */
		std::set<TileIndex> candidate_endpoints = std::set<TileIndex>();
		candidate_endpoints.insert(outflow_tile);

		/* Proceed until all inflow tiles are processed. */
		while (inflow_tile_to_flow.size() > 0) {
			TileIndex nearest_outflow_tile = INVALID_TILE;

			/* Choose an inflow tile to process, start with those with the biggest flow, to set up the paths for big rivers flowing
			 * through a lake first, and then the paths for small side rivers.
			 */
			TileIndex chosen_inflow_tile = INVALID_TILE;
			int max_flow = -1;
			for (std::map<TileIndex, int>::const_iterator it = inflow_tile_to_flow.begin(); it != inflow_tile_to_flow.end(); it++) {
				TileIndex curr_tile = it->first;
				int curr_flow = it->second;
				if (curr_flow > max_flow) {
					max_flow = curr_flow;
					chosen_inflow_tile = curr_tile;
				}
			}

			/* If negative flow somehow enters the calculation, we might not choose any tile.
			 * For the sake of robustness, just take the first tile in that case.
			 * (deliberately no assertion, as a logger gives bigger chances to understand what might have gone wrong
			 *  on a 1024x1024 map than just aborting.  And we can do something defined anyway).
			 */
			if (chosen_inflow_tile == INVALID_TILE) {
				chosen_inflow_tile = inflow_tile_to_flow.begin()->first;
				DEBUG(map, 0, "Warning: No inflow tile could be chosen, taking the first one at (%i,%i) mapped to flow %i",
							  TileX(chosen_inflow_tile), TileY(chosen_inflow_tile), inflow_tile_to_flow[chosen_inflow_tile]);
			}

			/* Now find out the nearest candidate endpoint.  (either outflow tile, or some tile on a path). */
			int distance = -1;
			for (std::set<TileIndex>::const_iterator it2 = candidate_endpoints.begin(); it2 != candidate_endpoints.end(); it2++) {
				int candidate_distance = DistanceManhattan(chosen_inflow_tile, *it2);
				if (distance == -1 || distance < candidate_distance) {
					distance = candidate_distance;
					nearest_outflow_tile = *it2;
				}
			}

			DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Making path from (%i,%i) to (%i,%i) with flow %i guaranteed",
						  TileX(chosen_inflow_tile), TileY(chosen_inflow_tile), TileX(nearest_outflow_tile), TileY(nearest_outflow_tile), max_flow);

			/* Calculate a path. */
			std::vector<TileIndex> path = std::vector<TileIndex>();
			bool path_found = RainfallRiverGenerator::CalculateLakePath(*lake_tiles, chosen_inflow_tile, nearest_outflow_tile, path, -1);
			if (path_found) {
				int flow = max_flow;
				DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Path with " PRINTF_SIZE " tiles found", path.size());

				/* Step through the path, and make tiles guaranteed. */
				for (int z = 0; z < (int)path.size(); z++) {
					TileIndex curr_tile = path[z];

					/* Paths connect the center tiles with the outflow tile in that direction.
					 * The center tiles were connected with the inflow tiles above.
					 * If we find an already guaranteed lake tile, we want to stop, in order to declare as less tiles as possible guaranteed.
					 * However, we need to exclude the case that we have found a tile on the path from an inflow tile to its center tile, since for such tiles,
					 * we don´t have any guarantee that they are already connected with the outflow tile.
					 */
					if (guaranteed_water_tiles.find(curr_tile) != guaranteed_water_tiles.end() && guaranteed_tiles_for_inflow.find(curr_tile) == guaranteed_tiles_for_inflow.end()) {
						break;
					}

					/* Choose some candiate endpoints, see above for a more detailed explanation. */
					if (z > 0 && (z % 7 == 0)) {
						candidate_endpoints.insert(path[z]);
						DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Marking (%i,%i) a candidate endpoint", TileX(path[z]), TileY(path[z]));
					}
					if (lake->IsLakeTile(curr_tile)) {
						/* The path might end in the outflow tile, which is not part of the lake.  Don´t declare that one
						 * a guaranteed lake tile either.
						 */
						DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, ".... Marking (%i,%i) a guaranteed water tile", TileX(curr_tile), TileY(curr_tile));
						guaranteed_water_tiles.insert(curr_tile);
						MarkGuaranteed(water_info, curr_tile);
						water_flow[curr_tile] = max(water_flow[curr_tile], flow);
						DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Setting water_flow of guaranteed (%i,%i) to %i, based on flow %i of (%i,%i)",
										TileX(curr_tile), TileY(curr_tile), water_flow[curr_tile], flow, TileX(chosen_inflow_tile), TileY(chosen_inflow_tile));
					}
				}
			} else {
				/* This should not happen, as our lakes are supposed to be connected (at least here, before potential islands etc. enter the scene).
				 * Again: Deliberately no assertion.
				 */
				DEBUG(map, 0, "WARNING: No path could be found between (%i,%i) and (%i,%i), lake tiles are:", TileX(chosen_inflow_tile), TileY(chosen_inflow_tile),
																											   TileX(nearest_outflow_tile), TileY(nearest_outflow_tile));
				for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
					DEBUG(map, 0, "(%i,%i)", TileX(*it), TileY(*it));
				}
			}

			inflow_tile_to_flow.erase(chosen_inflow_tile);
			DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Nearest_inflow_tile (%i,%i) has been processed", TileX(chosen_inflow_tile), TileY(chosen_inflow_tile));
		}
	}

	/* Set of discarded lake tiles, will be filled by LakeModificators. */
	std::set<TileIndex> discarded_lake_tiles = std::set<TileIndex>();

	if (RandomRange(MAX_RAINFALL_PROBABILITY) < _settings_newgame.game_creation.rainfall.lake_reduce_to_guaranteed_probability) {
		OnlyGuaranteedLakeModificator only_guaranteed_lake_modificator;
		only_guaranteed_lake_modificator.ModifyLake(lake, inflow_tile_to_center, guaranteed_water_tiles, discarded_lake_tiles);
	}

	IslandLakeModificator island_lake_modificator;
	island_lake_modificator.ModifyLake(lake, inflow_tile_to_center, guaranteed_water_tiles, discarded_lake_tiles);

	ExpandShoreLakeModificator expand_shore_lake_modificator;
	expand_shore_lake_modificator.ModifyLake(lake, inflow_tile_to_center, guaranteed_water_tiles, discarded_lake_tiles);

	/* Terraform all lake tiles to the surface height of the lake and perform some final bookkeeping. */
	int surface_height = lake->GetSurfaceHeight();
	lake_tiles = lake->GetLakeTiles();
	DEBUG(map, RAINFALL_GUARANTEED_LAKE_TILES_LOG_LEVEL, "Will terraform lake (%i,%i) to surface height %i", TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()), surface_height);
	DEBUG(map, RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL, "Will terraform lake (%i,%i) with " PRINTF_SIZE " lake tiles to height %i",
			  TileX(lake->GetCenterTile()), TileY(lake->GetCenterTile()), lake_tiles->size(), surface_height);
	for (std::set<TileIndex>::const_iterator it = lake_tiles->begin(); it != lake_tiles->end(); it++) {
		TileIndex lake_tile = *it;
		TerraformTileToSlope(lake_tile, surface_height, SLOPE_FLAT);


		DEBUG(map, RAINFALL_TERRAFORM_FOR_LAKES_LOG_LEVEL, ".... Terraformed tile (%i,%i) to height %i", TileX(lake_tile), TileY(lake_tile), surface_height);

		/* Make guaranteed lake tiles water without any further condition.  If not guaranteed, discarded lake tiles will not become water under any
	     * circumstances.  Furthermore, not-guaranteed lake tiles may not be higher than the surface height.
		 */
		if (guaranteed_water_tiles.find(lake_tile) != guaranteed_water_tiles.end() || discarded_lake_tiles.find(lake_tile) == discarded_lake_tiles.end()) {
			extra_water_tiles.push_back(TileWithValue(lake_tile, water_flow[tile]));
			water_flow[lake_tile] = water_flow[tile];
			MarkProcessed(water_info, lake_tile);
		}
	}
}

/* ========================================================= */
/* ======= Prepare River, Choose Diagonal Extra Tiles ====== */
/* ========================================================= */

/** In the situation that a river flows diagonal, this function decides which of the straight neighbor tiles
 *  beneith that diagonal direction is declared an extra river tile.
 */
void RainfallRiverGenerator::ChooseTileForExtraRiver(TileIndex tile,
									TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT],
									Direction diagonal_direction, Direction straight_direction_one, Direction straight_direction_two,
									bool inflow_pure, bool *choose_tile_one, bool *choose_tile_two)
{
	/* For shortness of the calling function, we check here wether we actually have a diagonal pure inflow tile. */
	if (inflow_pure && !*choose_tile_one && !*choose_tile_two) {
		/* The diagonal tile exists since the inflow is pure, and the straight tiles exist since the diagonal tile exists. */
		Slope neighbor_slope_one = neighbor_slopes[straight_direction_one];
		Slope neighbor_slope_two = neighbor_slopes[straight_direction_two];
		int neighbor_height_one = neighbor_heights[straight_direction_one];
		int neighbor_height_two = neighbor_heights[straight_direction_two];

		if (neighbor_height_one > neighbor_height_two) {
			/* If the candiate tiles differ in their height as of GetTileZ, choose the upper one, as this allows as to lower landscape instead of make it higher. */
			*choose_tile_one = true;
		} else if (neighbor_height_two > neighbor_height_one) {
			/* Ditto */
			*choose_tile_two = true;
		} else {
			/* If both tiles have equal height, take the lower one, measured in raised corners. */
			int ascended_one = this->GetNumberOfAscendedSlopes(neighbor_slope_one);
			int ascended_two = this->GetNumberOfAscendedSlopes(neighbor_slope_two);
		    if (ascended_one < ascended_two) {
				*choose_tile_one = true;
			} else {
				*choose_tile_two = true;
			}
		}
	}
}

/** This function adds additional straight neighbor river tiles for river tiles with diagonal flow.
 */
void RainfallRiverGenerator::PrepareRiverTile(TileIndex tile, int flow, int *water_flow, byte *water_info, std::vector<TileWithValue> &extra_river_tiles)
{
	DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Generating river tile for (%i,%i) with flow %i", TileX(tile), TileY(tile), flow);

	/* First find all neighbor tiles that have a not-yet-processed river.  We will try our very best in this
	 * function to do terraforming in a way that is friendly towards placing a river on those tiles.
	 */
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;
	Slope neighbor_slopes[DIR_COUNT] = { SLOPE_FLAT, SLOPE_FLAT, SLOPE_FLAT, SLOPE_FLAT,
								 SLOPE_FLAT, SLOPE_FLAT, SLOPE_FLAT, SLOPE_FLAT };
	int neighbor_heights[DIR_COUNT] = { -1, -1, -1, -1, -1, -1, -1, -1 };
	bool invalidate_mask[DIR_COUNT] = { true, true, true, true, true, true, true, true };

	StoreAllNeighborTiles(tile, neighbor_tiles);
	StoreSlopes(neighbor_tiles, neighbor_slopes, neighbor_heights);

	/* Set up a second array, just containing the relevant neighbor tiles, i.e. the ones where actually a river we have to take care of is located. */
	TileIndex water_neighbor_tiles[DIR_COUNT] = { neighbor_tiles[0], neighbor_tiles[1], neighbor_tiles[2], neighbor_tiles[3],
											 neighbor_tiles[4], neighbor_tiles[5], neighbor_tiles[6], neighbor_tiles[7] };
	for (uint n = DIR_BEGIN; n < DIR_END; n++) {
		if (neighbor_tiles[n] != INVALID_TILE) {
			invalidate_mask[n] = !IsWaterTile(water_info, neighbor_tiles[n]);
		}
	}
	InvalidateTiles(water_neighbor_tiles, invalidate_mask);


	/* Properties of our tile */
	int height;
	Slope slope = GetTileSlope(tile, &height);

	DebugTileInfo(RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, tile, slope, height, neighbor_tiles, neighbor_slopes, neighbor_heights);

	/* Diagonal flow that needs adding river tiles as straight neighbor tiles. */
	bool north_inflow_pure = this->IsPureDiagonalFlow(tile, water_neighbor_tiles, water_flow, water_info, DIR_N, DIR_NW, DIR_NE);
	bool west_inflow_pure = this->IsPureDiagonalFlow(tile, water_neighbor_tiles, water_flow, water_info, DIR_W, DIR_NW, DIR_SW);
	bool east_inflow_pure = this->IsPureDiagonalFlow(tile, water_neighbor_tiles, water_flow, water_info, DIR_E, DIR_NE, DIR_SE);
	bool south_inflow_pure = this->IsPureDiagonalFlow(tile, water_neighbor_tiles, water_flow, water_info, DIR_S, DIR_SW, DIR_SE);

	DEBUG(map, 9, "Tile (%i,%i): pure (%i,%i,%i,%i)", TileX(tile), TileY(tile), north_inflow_pure, west_inflow_pure, east_inflow_pure, south_inflow_pure);

	/* If two adjacent diagonal tiles have both pure inflow, we don´t look at the slopes, but choose the tile in between. */
	bool add_river_nw = north_inflow_pure && west_inflow_pure;
	bool add_river_ne = north_inflow_pure && east_inflow_pure;
	bool add_river_sw = south_inflow_pure && west_inflow_pure;
	bool add_river_se = south_inflow_pure && east_inflow_pure;

	DEBUG(map, 9, "Tile (%i,%i), simple: add (%i,%i,%i,%i)", TileX(tile), TileY(tile), add_river_nw, add_river_ne, add_river_sw, add_river_se);

	/* If the simple strategy above didn´t help, take a decision based on the slopes. */
	this->ChooseTileForExtraRiver(tile, neighbor_tiles, neighbor_slopes, neighbor_heights, DIR_N, DIR_NW, DIR_NE, north_inflow_pure, &add_river_nw, &add_river_ne);
	this->ChooseTileForExtraRiver(tile, neighbor_tiles, neighbor_slopes, neighbor_heights, DIR_W, DIR_NW, DIR_SW, west_inflow_pure, &add_river_nw, &add_river_sw);
	this->ChooseTileForExtraRiver(tile, neighbor_tiles, neighbor_slopes, neighbor_heights, DIR_E, DIR_NE, DIR_SE, east_inflow_pure, &add_river_ne, &add_river_se);
	this->ChooseTileForExtraRiver(tile, neighbor_tiles, neighbor_slopes, neighbor_heights, DIR_S, DIR_SW, DIR_SE, south_inflow_pure, &add_river_sw, &add_river_se);

	DEBUG(map, 9, "Tile (%i,%i), based on slopes: add (%i,%i,%i,%i)", TileX(tile), TileY(tile), add_river_nw, add_river_ne, add_river_sw, add_river_se);

	/* Update the water_neighbor_tiles array, as those extra straight neighbor tiles weren´t known when we calculated it. */
	this->DeclareNeighborTileWater(water_neighbor_tiles, neighbor_tiles, add_river_nw, DIR_NW);
	this->DeclareNeighborTileWater(water_neighbor_tiles, neighbor_tiles, add_river_ne, DIR_NE);
	this->DeclareNeighborTileWater(water_neighbor_tiles, neighbor_tiles, add_river_sw, DIR_SW);
	this->DeclareNeighborTileWater(water_neighbor_tiles, neighbor_tiles, add_river_se, DIR_SE);

	/* Mark extra water tiles being introduced because of straight neighbor tiles as being processed and being rivers. */
	this->SetExtraNeighborTilesProcessed(water_neighbor_tiles, water_info, extra_river_tiles, add_river_nw, DIR_NW, flow);
	this->SetExtraNeighborTilesProcessed(water_neighbor_tiles, water_info, extra_river_tiles, add_river_ne, DIR_NE, flow);
	this->SetExtraNeighborTilesProcessed(water_neighbor_tiles, water_info, extra_river_tiles, add_river_sw, DIR_SW, flow);
	this->SetExtraNeighborTilesProcessed(water_neighbor_tiles, water_info, extra_river_tiles, add_river_se, DIR_SE, flow);

	MarkProcessed(water_info, tile);
}

/** This function determines, which tiles are planned to become river or lake, based on the settings (i.e. minimum flow
 *  for river).  The tiles are added to the given vector of water_tiles.
 */
void RainfallRiverGenerator::DeterminePlannedWaterTiles(std::vector<TileWithHeightAndFlow> &water_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator)
{
	/* In this step of the calculation, we leave the heightlevel iterator scheme we used above.
	 * We now have calculated flow values, they grow downwards with the water flow, we know where
	 * lakes are supposed to be, and which surface level they have.
     * However, we still need to perform some terraforming on the small scale, since above, we
	 * generated flow in particular for tiles that are not suitable for rivers and lakes.
	 * Our strategy is lowering tiles where needed to make space for rivers, but ascending
	 * landscape in lake basins in order to get a proper lake surface.
	 * To make this work, we process and maybe terraform tiles bottom-up, and for equal
	 * heightlevels, we first have a look at tiles with big flow.
	 * The following loop records all relevant tiles in a vector, and after that, the vector
	 * will be sorted accordingly.
	 */
	for (uint x = 0; x < MapSizeX(); x++) {
		for (uint y = 0; y < MapSizeY(); y++) {
			TileIndex tile = TileXY(x, y);
			if (IsActiveLakeCenter(water_info, tile) && define_lakes_iterator->HasSurfaceHeightForLake(tile)) {
				/* Record lake centers with the calculated surface heigth */
				uint surface_height = (int)define_lakes_iterator->GetSurfaceHeightForLake(tile);
				water_tiles.push_back(TileWithHeightAndFlow(tile, surface_height, water_flow[tile]));
				DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Recorded tile (%i,%i) as lake center, surface_height %i, flow %i", TileX(tile), TileY(tile), surface_height, water_flow[tile]);
			} else if (water_flow[tile] >= _settings_newgame.game_creation.rainfall.flow_for_river && !IsOrdinaryLakeTile(water_info, tile) && !IsConsumedLakeCenter(water_info, tile)) {
				/* All other tiles with enough flow, that are not in lakes are considered rivers. */
				water_tiles.push_back(TileWithHeightAndFlow(tile, GetTileZ(tile), water_flow[tile]));
				DeclareRiver(water_info, tile);
				DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Recorded tile (%i,%i) as river tile, height %i, flow %i", TileX(tile), TileY(tile), GetTileZ(tile), water_flow[tile]);
			}
		}
	}

	std::sort(water_tiles.begin(), water_tiles.end());
}

/** This function performs some preparation work on both rivers and lakes.  E.g., adding additional tiles to rivers, if they flow diagonal, or
 *  determining, which tiles are necessary to keep a lake connected.
 */
void RainfallRiverGenerator::PrepareRiversAndLakes(std::vector<TileWithHeightAndFlow> &water_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator,
												   std::vector<TileWithValue> &extra_river_tiles)
{
	for (uint n = 0; n < water_tiles.size(); n++) {
		TileWithHeightAndFlow tile_info = water_tiles[n];
		TileIndex tile = tile_info.tile;
		int flow = tile_info.flow;

		if (IsActiveLakeCenter(water_info, tile)) {
			this->PrepareLake(tile, water_flow, water_info, define_lakes_iterator, extra_river_tiles);
		} else {
			this->PrepareRiverTile(tile, flow, water_flow, water_info, extra_river_tiles);
		}
	}
}

/** This function adds the tiles stored in extra_river_tiles with flow zero to the given vector of water_tiles.
 */
void RainfallRiverGenerator::AddExtraRiverTilesToWaterTiles(std::vector<TileWithHeightAndFlow> &water_tiles, std::vector<TileWithValue> &extra_river_tiles)
{
	/* The above algorithm may produce extra river tiles to transform diagonal flow into flow over tile edges (and not corners).
	 * Just add them at the end of the vector here, the sort order isn´t relevant here any longer.  Also, below we only need
	 * tile, but no longer surface_height and flow.
	 */
	for (std::vector<TileWithValue>::const_iterator it = extra_river_tiles.begin(); it != extra_river_tiles.end(); it++) {
		TileWithValue tile_with_value = *it;
		water_tiles.push_back(TileWithHeightAndFlow(tile_with_value.tile, 0, tile_with_value.value));
	}
}

/** This function makes all tiles planned to become river actually river in the OpenTTD sense.
 *  Though usually, only tiles with valid slope will be passed to that function, it performs a check
 *  wether slope is indeed valid, and prints a warning to the log if not.
 */
void RainfallRiverGenerator::GenerateRiverTiles(std::vector<TileWithHeightAndFlow> &water_tiles, byte *water_info)
{
	/* Here, finally, we actually generate the water.
	 * Note that this step is separated from the previous one, in order to have the chance to do a last check, wether slope is actually correct.
	 * After all, the above algorithm has to perform some terraforming, and guaranteeing that not accidentally some river tile is terraformed
	 * to the wrong slope as side effect of another terraforming operation is not quite trivial.
	 * By calling MakeWater here, after a final check, we make things safe.
	 */
	for (TileIndex tile = 0; tile < MapSize(); tile++) {
		if (WasProcessed(water_info, tile)) {
			Slope slope = GetTileSlope(tile);
			if (slope == SLOPE_FLAT || slope == SLOPE_NE || slope == SLOPE_SE || slope == SLOPE_NW || slope == SLOPE_SW) {
				MakeRiver(tile, Random());
			} else {
				DEBUG(map, 0, "WARNING: Tile (%i,%i) was marked to become water, but has wrong slope %s .... Ignored.", TileX(tile), TileY(tile), SlopeToString(GetTileSlope(tile)));
			}
		}
	}
}

/* ========================================================= */
/* ================ Guaranteed lake tiles ================== */
/* ========================================================= */

void RainfallRiverGenerator::MarkCornerTileGuaranteed(int *water_flow, byte *water_info, std::set<TileIndex>* lake_tiles, std::set<TileIndex> &guaranteed_water_tiles,
													  TileIndex tile, Direction direction,
													  Direction alternative_direction_one, Direction alternative_direction_two)
{
	if (GetFlowDirection(water_info, tile) == direction) {
		TileIndex alternative_one = AddDirectionToTile(tile, alternative_direction_one);
		if (lake_tiles->find(alternative_one) == lake_tiles->end()) {
			alternative_one = INVALID_TILE;
		}

		TileIndex alternative_two = AddDirectionToTile(tile, alternative_direction_two);
		if (lake_tiles->find(alternative_two) == lake_tiles->end()) {
			alternative_two = INVALID_TILE;
		}

		if (alternative_one != INVALID_TILE) {
			if (alternative_two != INVALID_TILE) {
				if (RandomRange(2) == 0) {
					guaranteed_water_tiles.insert(alternative_one);
					MarkGuaranteed(water_info, alternative_one);
				} else {
					guaranteed_water_tiles.insert(alternative_two);
					MarkGuaranteed(water_info, alternative_two);
				}
			} else {
				guaranteed_water_tiles.insert(alternative_one);
				MarkGuaranteed(water_info, alternative_one);
			}
		} else {
			if (alternative_two != INVALID_TILE) {
				guaranteed_water_tiles.insert(alternative_two);
				MarkGuaranteed(water_info, alternative_two);
			} else {
				/* Ignore this case.  It typically occurs if a river flows into a lake, and out again, before finally ending up in the lake.
				 * this is possible, as lake expansion doesn´t work along flow paths, but in a more random manner.
				 * The case isn´t a problem either, as no lake modificator can ever discard a river tile, i.e. from player perspective there
				 * will be a connection.
				 */
			}
		}
	}
}

/* ========================================================= */
/* ======= Fix problem tiles using local terraforming ====== */
/* ========================================================= */

/** Tries to improves a problem tile (i.e. a planned river tile with invalid slope) by performing terraforming on the small scale.
 *  See FixByLocalTerraforming for the general idea.
 *  @param tile some tile
 *  @param flow amount of flow to be used for new water tiles
 *  @param tiles_fixed tiles which became / become valid as side effect during the current calculation.
 *  @param new_problem_tiles tiles that were valid before, but became / become invalid during the calculation.  Such tiles can exist, if more tiles get better than worse.
 *  @param water_flow the water flow array
 *  @param water_info the water info array
 *  @param define_lakes_iterator the DefineLakesIterator with information about lakes
 *  @param only_self if true, schemes terraforming another tile than the given one will not be used
 *  @param only_improve if true, results that make things better by improving tiles, but at the same time making some other tiles worse are not allowed.
 *  @param make_water_afterwards if true, the tile is made water afterwards
 *  @return wether a terraforming that improves the overall situation could be executed
 */
bool RainfallRiverGenerator::ImproveByTerraforming(TileIndex tile, int flow, std::set<TileIndex> &tiles_fixed, std::set<TileIndex> &new_problem_tiles, int *water_flow, byte *water_info,
												   DefineLakesIterator *define_lakes_iterator, bool only_self, bool only_improve, bool make_water_afterwards)
{
	int height = TileHeight(tile);
	Slope slope = GetTileSlope(tile);

	DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "......... Processing tile (%i,%i) with (%s,%i)", TileX(tile), TileY(tile), SlopeToString(slope), height);

	/* The list of simulated terraforming operations.  Each of its indices must match the corresponding index in the TerraformingScheme vector
	 * for the current slope.  If this condition is violated, you will get segmentation faults from the code below!
	 */
	std::vector<TerraformingAction> actions = std::vector<TerraformingAction>();

	for (uint z = 0; z < this->slope_to_schemes[slope].size(); z++) {

		/*  The scheme contains a desired slope, a desired height difference, and a dx/dy value to be applied on the tile position.
		 *  It will take the height of the current tile, adds the height difference, then walks to a neighbor tile based on the
		 *  dx/dy value, and finally terraforms that tile to the desired slope / desired height combination.
		 *  Remarks:
		 *  - The most frequent case is dx,dy = (0,0).
		 *  - However, some configurations of tiles can be better solved when one tries to terraform a neighbor tile.
		 *  - As we don´t know anything about its slope (and thus the height of its northern corner), we decide its height based on our height.
		 */
		TerraformingScheme scheme = this->slope_to_schemes[slope][z];
		int desired_height = height + scheme.delta_height;

		int tx = TileX(tile) + scheme.dx;
		int ty = TileY(tile) + scheme.dy;
		TileIndex terraform_tile = (tx > 0 && tx < (int)MapMaxX() && ty > 0 && ty < (int)MapMaxY()) ? TileXY(tx, ty) : INVALID_TILE;

		/* Bookkeeping */
		actions.push_back(TerraformingAction(scheme.slope, desired_height));

		if (terraform_tile == INVALID_TILE || (only_self && (scheme.dx != 0 || scheme.dy != 0))) {
			/*  If the step to the terraform tile leads outside map, we can´t do anything.  Also, if we want to make the tile water afterwards,
			 *  we explicitely want to terraform this tile (as we use this mode in situations with a pretty clear picture of the situations).
			 */
			actions[z].success = false;
		} else {
			actions[z].success = SimulateTerraformTileToSlope(terraform_tile, desired_height, scheme.slope, actions[z].terraformer_state);
		}

		if (actions[z].success) {
			/* Terraforming is possible (most probably it is, as there are not many reason for a failed terraforming in river generation, where
			 * no houses, industries, etc. exist yet), now evaluate it.
			 */
			this->RegisterTilesAffectedByTerraforming(actions[z].terraformer_state, actions[z].affected_tiles, water_info, 0);

			/* Number of tiles that become better */
			int fixed_tiles = 0;

			/* Number of tiles that become worse */
			int number_of_new_problem_tiles = 0;

			/* We consider the steep slopes, and the SLOPE_EW, SLOPE_NS as extra bad slopes, since they are sometimes difficult to fix by
			 * local terraformings that are improving the overall situation, in terms of number of invalid slopes.
			 * Thus, we count the number of such slopes before and afterwards, and if it decreases, we take a TerraformingAction even if
			 * it has score < 0.  By only doing this if the number of such slopes decreases, we get the guarantee that the local
			 * terraforming iteration terminates - we cannot decrease the number of such slopes forever.
		     */
			int no_longer_extra_bad_slope = 0;
			int now_extra_bad_slope = 0;

			for (std::set<TileIndex>::const_iterator it2 = actions[z].affected_tiles.begin(); it2 != actions[z].affected_tiles.end(); it2++) {
				/* affected_tile is a lake/river tile, because RegisterTilesAffectedByTerraforming already filters for that */
				TileIndex affected_tile = *it2;

				int curr_height;
				Slope current_slope = GetTileSlope(affected_tile, &curr_height);

				int planned_height = actions[z].terraformer_state.GetTileZ(affected_tile);
				Slope planned_slope = actions[z].terraformer_state.GetPlannedSlope(affected_tile);

				/* Don´t demolish lakes, and some other checks that lead to considering the action as not possible. */
				if (affected_tile != tile
					&& (curr_height != planned_height || current_slope != planned_slope)
                    &&  (IsOrdinaryLakeTile(water_info, affected_tile) || IsLakeCenter(water_info, affected_tile))
				    && WasProcessed(water_info, affected_tile)
					&& (planned_height != define_lakes_iterator->GetLake(affected_tile)->GetSurfaceHeight() || planned_slope != SLOPE_FLAT)) {
						actions[z].success = false;
						DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, ".... Case %s, (%s,%i), (%i,%i): Trying to terraform lake tile (%i,%i) to (%s,%i), but surface_height is %i; discarding this",
										SlopeToString(slope), SlopeToString(scheme.slope), scheme.delta_height, scheme.dx, scheme.dy, TileX(affected_tile), TileY(affected_tile),
										SlopeToString(planned_slope), planned_height, define_lakes_iterator->GetLake(affected_tile)->GetSurfaceHeight());
						break;
				}

				/* Don´t allow to raise coast tiles that are planned to become river tiles.  This can cut the connection between river and ocean. */
				if (IsTileType(affected_tile, MP_WATER) && IsCoastTile(affected_tile) && curr_height == 0 && planned_height > 0) {
					DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "Don´t allow to raise (%i,%i) to height > 0; discarding this", TileX(affected_tile), TileY(affected_tile));
					actions[z].success = false;
					break;
				}

				/* Now perform the bookkeeping */
				bool planned_slope_valid = IsValidSlopeForRiver(planned_slope);
				bool current_slope_valid = IsValidSlopeForRiver(current_slope);
				bool planned_slope_extra_bad = IsSteepSlope(planned_slope) || planned_slope == SLOPE_NS || planned_slope == SLOPE_EW;
				bool current_slope_extra_bad = IsSteepSlope(current_slope) || current_slope == SLOPE_NS || current_slope == SLOPE_EW;

				if (!planned_slope_valid && current_slope_valid) {
					number_of_new_problem_tiles++;
				} else if (planned_slope_valid && !current_slope_valid) {
					fixed_tiles++;
				}
				if (!planned_slope_extra_bad && current_slope_extra_bad) {
					no_longer_extra_bad_slope++;
				} else if (planned_slope_extra_bad && !current_slope_extra_bad) {
					now_extra_bad_slope++;
				}
			}

			/* Basically, the number of tiles that becomes better, minus the number of tiles that becomes worse.
			 * If we restrict things to the own tile, we know for sure that it would be better afterwards, and thus
			 * add one to the score.
			 * (situation we want to avoid using that: a score of zero ends up in doing simply nothing...)
			 */
			actions[z].score = fixed_tiles - number_of_new_problem_tiles + (only_self ? 1 : 0);
			actions[z].extra_bad_score = no_longer_extra_bad_slope - now_extra_bad_slope;

			if (only_improve && number_of_new_problem_tiles > 0) {
				actions[z].success = false;
			}

			DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, ".... Case %s, (%s,%i), (%i,%i): Terraforming (%i,%i) from (%s, %i) to (%s,%i) would make %i tiles better, %i tiles worse => score = %i (extra_bad_score = %i)",
						  SlopeToString(slope), SlopeToString(scheme.slope), scheme.delta_height, scheme.dx, scheme.dy, TileX(terraform_tile), TileY(terraform_tile),
						  SlopeToString(slope), height, SlopeToString(scheme.slope), desired_height, fixed_tiles, number_of_new_problem_tiles, actions[z].score, actions[z].extra_bad_score);
		}
	}

	/* Sort.  Successful actions come first, and among them, higher scores come before lower score */
	std::sort(actions.begin(), actions.end());

	/* Actions are sorted descending by score.  Usually, the first index matches, however
	 * the case that all actions have negative score, but a later one is the first one with extra_bad_score > 0
	 * is possible.
	 */
	int chosen_action_index = -1;
	for (int n = 0; n < (int)actions.size(); n++) {
		if (actions[n].success && ((actions[n].score > 0 && actions[n].extra_bad_score >= 0) || actions[n].extra_bad_score > 0)) {
			chosen_action_index = n;
			break;
		}
	}

	if (chosen_action_index >= 0) {
		/* Execute the best terraforming */

		DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "........ Choose to terraform to (%s,%i); score %i; extra_bad_score %i",
							SlopeToString(actions[chosen_action_index].slope), actions[chosen_action_index].height, actions[chosen_action_index].score, actions[chosen_action_index].extra_bad_score);

		ExecuteTerraforming(actions[chosen_action_index].terraformer_state);

		/* ... and perform the bookkeeping */
		for (std::set<TileIndex>::const_iterator it2 = actions[chosen_action_index].affected_tiles.begin(); it2 != actions[chosen_action_index].affected_tiles.end(); it2++) {
			TileIndex affected_tile = *it2;
			Slope current_slope = GetTileSlope(affected_tile);

			bool current_slope_valid = IsValidSlopeForRiver(current_slope);
			if (!current_slope_valid) {
				DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "............ Adding affected tile (%i,%i) with new slope %s to problem tiles.",
																TileX(affected_tile), TileY(affected_tile), SlopeToString(current_slope));
				new_problem_tiles.insert(affected_tile);
				tiles_fixed.erase(affected_tile);
			} else {
				tiles_fixed.insert(affected_tile);
			}
		}

		/* Make the tile water */
		if (make_water_afterwards) {
			if (!IsTileSuitableForRiver(tile)) {
				DEBUG(map, 0, "WARNING: Tile (%i,%i) is not suitable for water after terraforming it.  Might be a problem as the calling code relies on that.", TileX(tile), TileY(tile));
			}

			DeclareRiver(water_info, tile);
			MarkProcessed(water_info, tile);
			water_flow[tile] = flow;
		}

		return true;
	} else {
		return false;
	}
}

/** This function tries to make problem tiles suitable for rivers by performing local terraforming operations.  For each problematic slope, a lookup table
 *  contains a number of possible terraforming operations, that might improve the situation.  Each of those will be evaluated for some tile at hand,
 *  and if any of them improves the situation, the best one will be executed.
 *  The criterium for this is the number of tiles that would be fixed, minus the number of tiles that were fixed before, but would no longer be ok for water afterwards.
 *  By only applying terraforming operations that improve the situation (i.e. the score described above must be > 0) we have the guarantee that the
 *  algorithm will eventually terminate.
 */
void RainfallRiverGenerator::FixByLocalTerraforming(std::set<TileIndex> &problem_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator)
{
	DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "Starting FixByLocalTerraforming with " PRINTF_SIZE " problem tiles.", problem_tiles.size());

	std::set<TileIndex> new_problem_tiles = std::set<TileIndex>();
	bool any_tile_improved = false;
	int number_of_iterations = 0;

	do {
		any_tile_improved = false;
		std::set<TileIndex> tiles_fixed = std::set<TileIndex>();
		DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, ".. Starting iteration %i with " PRINTF_SIZE " problem tiles.", number_of_iterations, problem_tiles.size());

		for (std::set<TileIndex>::const_iterator it = problem_tiles.begin(); it != problem_tiles.end(); it++) {
			TileIndex tile = *it;

			bool success = this->ImproveByTerraforming(tile, water_flow[tile], tiles_fixed, new_problem_tiles, water_flow, water_info, define_lakes_iterator, false, false, false);

			if (success) {
				any_tile_improved = true;
			} else {
				new_problem_tiles.insert(tile);
			}
		}

		/* Finally replace the contents of the problem_tiles set with the contents for the next iteration. */
		problem_tiles.clear();
		for (std::set<TileIndex>::const_iterator it = new_problem_tiles.begin(); it != new_problem_tiles.end(); it++) {
			if (tiles_fixed.find(*it) == tiles_fixed.end()) {
				problem_tiles.insert(*it);
			} else {
				DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "........ Ignoring tile (%i,%i) for the next iteration, since it was fixed.", TileX(*it), TileY(*it));
			}
		}
		new_problem_tiles.clear();
		number_of_iterations++;
	} while (any_tile_improved);


	DEBUG(map, RAINFALL_LOCAL_TERRAFORM_LOG_LEVEL, "Finishing FixByLocalTerraforming after %i iterations with " PRINTF_SIZE " problem tiles.", number_of_iterations, problem_tiles.size());
}

/* ========================================================= */
/* ======= Fine tuning tiles - Lower until valid =========== */
/* ========================================================= */

/** Stores all neighbor tiles of the given tile, that are planned as water, in the given direction-indexed array of neighbor tiles.
 */
void RainfallRiverGenerator::StoreNeighborTilesPlannedForWater(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info)
{
	StoreAllNeighborTiles(tile, neighbor_tiles);
	for (int n = DIR_BEGIN; n < DIR_END; n++) {
		if (neighbor_tiles[n] != INVALID_TILE && (!WasProcessed(water_info, neighbor_tiles[n]) || !this->IsPlannedAsWater(neighbor_tiles[n], water_flow, water_info))) {
			neighbor_tiles[n] = INVALID_TILE;
		}
	}
}

/** Based on the situation on the tiles around, this function determines wether a tile can be transformed to an inclined slope of some direction
 *  in terms of water flow around.
 *  The given directions are according to the following figure:
 *
 *  DD1 D DD2
 *  ND1 T ND2
 *
 *  where: T is the tile at hand, D is the tile in direction, DD1 in diagonal_direction_one, DD2 in diagonal_direction_two,
 *         ND1 in neighbor_direction_one, ND2 in neighbor_direction_two.
 *
 *  The directions have to be given exactly according to the figure above, just rotated by e.g. 45 degrees clockwise for a north east inclined slope.
 *
 *  @param tile some tile, T in the above figure
 *  @param water_neighbor_tiles neighbor tiles array of all neighbor tiles planend to become river
 *  @param water_flow the water flow array
 *  @param water_info the water info array
 *  @param direction the direction towards tile D in the above figure
 *  @param slope the current slope of tile T
 *  @param desired_slope the inclined slope to check for
 *  @param min_diagonal_height the minimum allowed height for DD1 and DD2 after the operation
 *  @param neighbor_direction_one direction towards ND1 in the above figure
 *  @param neighbor_direction_two direction towards ND2 in the above figure
 *  @param diagonal_direction_one direction towards DD1 in the above figure
 *  @param diagonal_direction_two direction towards DD2 in the above figure
 */
bool RainfallRiverGenerator::IsInclinedSlopePossible(TileIndex tile,
													 TileIndex water_neighbor_tiles[DIR_COUNT], int *water_flow, byte *water_info, Direction direction, Slope slope, Slope desired_slope, int min_diagonal_height,
													 Direction neighbor_direction_one, Direction neighbor_direction_two,
													 Direction diagonal_direction_one, Direction diagonal_direction_two)
{
	/* Only allow lowering things, i.e. the planned slope must be a sub-slope of the current slope. */
	bool sub_slope = (slope | desired_slope) == slope;

	/* If the neighbor tile is not planned to become water, we don´t care about it.  If it has the same slope, everything is perfect.
	 * Leaves open the case that it is planned to become water, and its GetMinTileZ is at least as high as the upper end of our planned inclined slope.
	 * Then the corresponding lowering action is ok either.
	 */
	bool nd1_ok = (water_neighbor_tiles[neighbor_direction_one] == INVALID_TILE
						|| GetTileSlope(water_neighbor_tiles[neighbor_direction_one]) == desired_slope || GetTileZ(water_neighbor_tiles[neighbor_direction_one]) >= min_diagonal_height);
	bool nd2_ok = (water_neighbor_tiles[neighbor_direction_two] == INVALID_TILE || GetTileSlope(water_neighbor_tiles[neighbor_direction_two]) == desired_slope
						|| GetTileZ(water_neighbor_tiles[neighbor_direction_two]) >= min_diagonal_height);

	/* If the diagonal neighbor tile is not planned to become water, we don´t care about it.  And if it is at least as high as the upper end of our inclined slope, everything is fine. */
	bool dd1_ok = (water_neighbor_tiles[diagonal_direction_one] == INVALID_TILE || GetTileZ(water_neighbor_tiles[diagonal_direction_one]) >= min_diagonal_height);
	bool dd2_ok = (water_neighbor_tiles[diagonal_direction_two] == INVALID_TILE || GetTileZ(water_neighbor_tiles[diagonal_direction_two]) >= min_diagonal_height);

	bool result = sub_slope && nd1_ok && nd2_ok && dd1_ok && dd2_ok;

	/* If at the upper end of our planned inclined slope a river is located, it shouldn´t have bigger flow - this would violate prevention of upwards flow. */
	bool d_is_river = result && water_neighbor_tiles[direction] != INVALID_TILE && IsRiver(water_info, water_neighbor_tiles[direction]);
	bool d_has_smaller_flow = false;
	if (result && d_is_river) {
		d_has_smaller_flow = water_flow[water_neighbor_tiles[direction]] <= water_flow[tile];
		result &= d_has_smaller_flow;
	}

	DEBUG(map, 9, "IsInclinedSlopePossible(%s) calculates (%i - %i,%i,%i,%i - %i,%i) and answers %i",
					SlopeToString(desired_slope),
					sub_slope, nd1_ok, nd2_ok, dd1_ok, dd2_ok, d_is_river, d_has_smaller_flow,
					result);
	return result;
}

/** Determines all tiles affected by the given TerraformerState, and adds them to the given affected_tiles set.
 *  Affected means, their slope / height changes, they are higher than a given height, and optionally, they are planned to become river.
 *  @param terraformer_state terraformer state
 *  @param affected_tiles determined tiles will be added to this set
 *  @param water_info the water info array
 *  @param min_height minimum GetTileZ tiles must have (after the terraforming) to be considered
 *  @param only_processed_tiles if true, only tiles with WasProcessed == true will be considered
 */
void RainfallRiverGenerator::RegisterTilesAffectedByTerraforming(TerraformerState &terraformer_state, std::set<TileIndex> &affected_tiles, byte *water_info, int min_height, bool only_processed_tiles)
{
	/* Usual terraforming only cares about tiles whose height in terms of the map array changes.
	 * We here care about all neighbor tiles whose slope changes, which is a weaker condition.
	 * Thus, we first have to expand the affected neighborhood as stored in terraformer_state.tile_to_new_height
	 * by all tiles whose slope might change.
	 */
	std::vector<TileIndex> additional_tiles = std::vector<TileIndex>();
	for (TileIndexToHeightMap::const_iterator it = terraformer_state.tile_to_new_height.begin(); it != terraformer_state.tile_to_new_height.end(); it++) {
		TileIndex curr_tile = it->first;
		TileIndex nw_tile = TileY(curr_tile) > 1 ? curr_tile + TileDiffXY(0, -1) : INVALID_TILE;
		TileIndex ne_tile = TileX(curr_tile) > 1 ? curr_tile + TileDiffXY(-1, 0) : INVALID_TILE;
		TileIndex n_tile = TileX(curr_tile) > 1 && TileY(curr_tile) > 1 ? curr_tile + TileDiffXY(-1, -1) : INVALID_TILE;
		additional_tiles.push_back(nw_tile);
		additional_tiles.push_back(ne_tile);
		additional_tiles.push_back(n_tile);
	}
	for (int n = 0; n < (int)additional_tiles.size(); n++) {
		TileIndex curr_tile = additional_tiles[n];
		if (curr_tile != INVALID_TILE && terraformer_state.tile_to_new_height.find(curr_tile) == terraformer_state.tile_to_new_height.end()) {
			terraformer_state.tile_to_new_height[curr_tile] = TileHeight(curr_tile);
			DEBUG(map, 9, ".... registered additional tile (%i,%i) with height %i", TileX(curr_tile), TileY(curr_tile), TileHeight(curr_tile));
		}
	}

	for (TileIndexToHeightMap::const_iterator it = terraformer_state.tile_to_new_height.begin(); it != terraformer_state.tile_to_new_height.end(); it++) {
		TileIndex curr_tile = it->first;
		if (!only_processed_tiles || WasProcessed(water_info, curr_tile)) {
			int height;
			Slope slope = GetTileSlope(curr_tile, &height);
			int planned_height = terraformer_state.GetTileZ(curr_tile);
			Slope planned_slope = terraformer_state.GetPlannedSlope(curr_tile);

			if (     planned_height >= min_height
				 && (height != planned_height || slope != planned_slope)) {
				affected_tiles.insert(curr_tile);
			}
		}
	}
}

/* Problem: The algorithm for making tile slopes appropriate for rivers by lowering some tiles may accidentally lower large parts of a
 * wide river, since it doesn´t ever find a straight line where river can ascend.  Thus, if a diagonal neighbor tile is also water, we
 * look wether we have water in both adjacent directions, and if yes, we terraform the (higher) tiles of the direction with fewer tiles
 * to the current height.
 * Example: (R are river tiles at height 2, T is the tile at hand at height 1, r are river tiles at height 1).
 *
 * RRRRRRTrrr   RRRRRRrrrr    RRRRRrrrrr    RRRRRRrrrr
 * RRRRRRRrrr   RRRRRRRrrr    RRRRRRrrrr    RRRRRRrrrr
 * RRRRRRRrrr   RRRRRRRrrr    RRRRRRRrrr    RRRRRRrrrr
 *
 *    (1)         (2)            (3)          (4)
 *
 * The algorithm above lowers T, marks the R beneith of it problem tiles, and starts again.  The result might be the situation in (2).
 * Then it runs again, and produces (3).  Problem: It fails to produce a straight line of tiles, where the river can ascend from height 1 to 2.
 * Thus the river is terraformed to height 1 to the left, until some special river shape ends that situation.
 *
 * Our algorithm here produces (4) instead of (2), and thus avoids that problem.
 *
 *  @param problem_tiles current problem tiles
 *  @param tile some tile, with SLOPE_FLAT
 *  @param water_info the water info array
 *  @param x_offset x offset, either 1 or -1
 *  @param y_offset y offset, either 1 or -1
 *  @param affected_tiles set of tiles affected by terrafomring
 */
void RainfallRiverGenerator::LowerTileForDiagonalWater(std::set<TileIndex> &problem_tiles, TileIndex tile, byte *water_info, int x_offset, int y_offset, std::set<TileIndex> &affected_tiles)
{
	int ref_height = GetTileZ(tile);

	uint x = TileX(tile);
	uint y = TileY(tile);

	/* Take care of the map edge */
	if (x + x_offset > 0 && x + x_offset < MapMaxX() - 1 && y + y_offset > 0 && y + y_offset < MapMaxY() - 1) {
		TileIndex diagonal_tile = TileXY(x + x_offset, y + y_offset);

		/* We only have a potential problem, if the diagonal tile is water */
		if (WasProcessed(water_info, diagonal_tile)) {

			/* Count number of water tiles in both x_offset and y_offset direction with no other tile in between */
			int x_tiles = 0;
			for (uint curr_x = x + x_offset; curr_x > 0 && curr_x < MapMaxX() - 1 && WasProcessed(water_info, TileXY(curr_x, y)); curr_x += x_offset) {
				x_tiles++;
			}
			int y_tiles = 0;
			for (uint curr_y = y + y_offset; curr_y > 0 && curr_y < MapMaxY() - 1 && WasProcessed(water_info, TileXY(x, curr_y)); curr_y += y_offset) {
				y_tiles++;
			}

			/* If only one direction has water tiles, everything is fine */
			if (x_tiles > 0 && y_tiles > 0) {
				DEBUG(map, 9, ".... Processing %i tiles in x direction and %i tiles in y direction because of diagonal water at tile (%i,%i) with x_offset %i and y_offset %i",
								x_tiles, y_tiles, x, y, x_offset, y_offset);

				/* Choose the direction with less tiles, and terraform all those tiles to be flat, and of equal height as the given tile */
				if (x_tiles < y_tiles) {
					for (uint curr_x = x + x_offset; curr_x > 0 && curr_x < MapMaxX() - 1 && WasProcessed(water_info, TileXY(curr_x, y)); curr_x += x_offset) {
						TileIndex curr_tile = TileXY(curr_x, y);
						if (GetTileZ(curr_tile) >= ref_height) {
							TerraformerState terraformer_state;
							bool success = SimulateTerraformTileToSlope(curr_tile, ref_height, SLOPE_FLAT, terraformer_state);
							if (!success) {
								DEBUG(map, 9, "WARNING: Could not terraform tile (%i,%i) to height %i and slope %s while fine-tuning water tiles.",
									  TileX(curr_tile), TileY(curr_tile), ref_height, SlopeToString(SLOPE_FLAT));
							} else {
								this->RegisterTilesAffectedByTerraforming(terraformer_state, affected_tiles, water_info, ref_height);
								DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Terraforming tile (%i,%i) to height %i during fine tuning - case diagonal", TileX(curr_tile), TileY(curr_tile), ref_height);
								ExecuteTerraforming(terraformer_state);
							}
						}
					}
				} else {
					for (uint curr_y = y + y_offset; curr_y > 0 && curr_y < MapMaxY() - 1 && WasProcessed(water_info, TileXY(x, curr_y)); curr_y += y_offset) {
						TileIndex curr_tile = TileXY(x, curr_y);
						if (GetTileZ(curr_tile) >= ref_height) {
							TerraformerState terraformer_state;
							bool success = SimulateTerraformTileToSlope(curr_tile, ref_height, SLOPE_FLAT, terraformer_state);
							if (!success) {
								DEBUG(map, 9, "WARNING: Could not terraform tile (%i,%i) to height %i and slope %s while fine-tuning water tiles.",
									  TileX(curr_tile), TileY(curr_tile), ref_height, SlopeToString(SLOPE_FLAT));
							} else {
								this->RegisterTilesAffectedByTerraforming(terraformer_state, affected_tiles, water_info, ref_height);
								DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Terraforming tile (%i,%i) to height %i during fine tuning - case diagonal", TileX(curr_tile), TileY(curr_tile), ref_height);
								ExecuteTerraforming(terraformer_state);
							}
						}
					}
				}
			}
		}
	}
}

/** This function starts with a set of problem tiles, i.e. tiles that are planned to become water, but have the wrong slope.
 *  It iteratively terraforms them until all of those tiles have valid slope, and no other tile have become invalid because of that.
 *  Our strategy is quite simple: *Only* lower tiles.  The success is guaranteed, since if the whole map would be a flat plain
 *  of some height, all tiles would be valid.  But usually, the modifications performed by this function stay on the small scale.
 *  @param problem_tiles set of problem tiles
 *  @param water_flow water flow
 *  @param water_info water info
 */
void RainfallRiverGenerator::LowerHigherWaterTilesUntilValid(std::set<TileIndex> &problem_tiles, int *water_flow, byte *water_info, DefineLakesIterator *define_lakes_iterator)
{
	std::set<TileIndex> processed_tiles = std::set<TileIndex>();

	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;

	int iteration = 0;
	int curr_height = 0;
	std::vector<TileWithValue> problem_tiles_with_height = std::vector<TileWithValue>();
	while (problem_tiles.size() > 0) {
		iteration++;

		/* Sort problem tiles by height, and in this iteration, *only* process tiles at the lowest of those heightlevels */
		problem_tiles_with_height.clear();
		for (std::set<TileIndex>::const_iterator it = problem_tiles.begin(); it != problem_tiles.end(); it++) {
			problem_tiles_with_height.push_back(TileWithValue(*it, GetTileZ(*it)));
		}
		std::sort(problem_tiles_with_height.begin(), problem_tiles_with_height.end(), std::less<TileWithValue>());

		int first_height = problem_tiles_with_height[0].value;
		curr_height = max(curr_height, first_height);
		DEBUG(map, RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL, "........ Starting iteration %i with " PRINTF_SIZE " remaining problem tiles, curr_height = %i", iteration, problem_tiles.size(), curr_height);

		/* Iteratively record water tiles affected by the terraforming.  Afterwards, inspect all of them, if now some of them have wrong slope for water,
	     * they are added to the problem tiles.  Since we only lower landscape here, new problem tiles are always somewhere upwards, never downwards.
		 */
		std::set<TileIndex> affected_tiles = std::set<TileIndex>();

		std::vector<TileIndex> tiles_this_time = std::vector<TileIndex>();
		std::set<TileIndex> candidate_tiles = std::set<TileIndex>();

		for (int n = 0; n < (int)problem_tiles_with_height.size() && (int)problem_tiles_with_height[n].value == curr_height; n++) {
			tiles_this_time.push_back(problem_tiles_with_height[n].tile);
		}

		for (int n = 0; n < (int)tiles_this_time.size(); n++) {
 			/* Assumption: dirty_tiles contains only tiles that are planned to be water. */
			TileIndex tile = tiles_this_time[n];
			int height;
			Slope slope = GetTileSlope(tile, &height);
			StoreNeighborTilesPlannedForWater(tile, neighbor_tiles, water_flow, water_info);
			processed_tiles.insert(tile);

			/* Check wether one of the inclined slopes is possible */
			bool nw_possible = this->IsInclinedSlopePossible(tile, neighbor_tiles, water_flow, water_info, DIR_NW, slope, SLOPE_NW, curr_height + 1, DIR_SW, DIR_NE, DIR_W, DIR_N);
			bool ne_possible = this->IsInclinedSlopePossible(tile, neighbor_tiles, water_flow, water_info, DIR_NE, slope, SLOPE_NE, curr_height + 1, DIR_NW, DIR_SE, DIR_N, DIR_E);
			bool se_possible = this->IsInclinedSlopePossible(tile, neighbor_tiles, water_flow, water_info, DIR_SE, slope, SLOPE_SE, curr_height + 1, DIR_NE, DIR_SW, DIR_E, DIR_S);
			bool sw_possible = this->IsInclinedSlopePossible(tile, neighbor_tiles, water_flow, water_info, DIR_SW, slope, SLOPE_SW, curr_height + 1, DIR_SE, DIR_NW, DIR_S, DIR_W);

			DEBUG(map, RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL, "............ Inspecting tile (%i,%i) of height %i and slope %s, possible: (%i,%i,%i,%i)",
							TileX(tile), TileY(tile), height, SlopeToString(slope), nw_possible, ne_possible, sw_possible, se_possible);

			int desired_height = height;
			Slope desired_slope = SLOPE_FLAT;
			if (nw_possible + ne_possible + sw_possible + se_possible > 1 || (!nw_possible && !ne_possible && !sw_possible && !se_possible)) {
				/* If multiple inclined slopes are possible, we cannot satisfy all relationships to neighbor tiles using that, and have to lower the tile to SLOPE_FLAT */
				desired_slope = SLOPE_FLAT;
			} else if (nw_possible) {
				desired_slope = SLOPE_NW;
				desired_height++;
			} else if (ne_possible) {
				desired_slope = SLOPE_NE;
				desired_height++;
			} else if (sw_possible) {
				desired_slope = SLOPE_SW;
			} else if (se_possible) {
				desired_slope = SLOPE_SE;
			}

			DEBUG(map, RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL, "................ will terraform to height %i and slope %s", desired_height, SlopeToString(desired_slope));

			/* Simulate terraforming */
			TerraformerState terraformer_state;
			std::set<TileIndex> curr_affected_tiles = std::set<TileIndex>();
			bool success = SimulateTerraformTileToSlope(tile, desired_height, desired_slope, terraformer_state);
			if (!success) {
				DEBUG(map, 9, "WARNING: Could not terraform tile (%i,%i) to height %i and slope %s while fine-tuning water tiles.",
							  TileX(tile), TileY(tile), desired_height, SlopeToString(desired_slope));
			} else {
				this->RegisterTilesAffectedByTerraforming(terraformer_state, curr_affected_tiles, water_info, height);

				/* Check wether we would influence a lake. */
				for (std::set<TileIndex>::const_iterator it = curr_affected_tiles.begin(); it != curr_affected_tiles.end(); it++) {
					TileIndex affected_tile = *it;
					if (WasProcessed(water_info, affected_tile) && (IsOrdinaryLakeTile(water_info, affected_tile) || IsLakeCenter(water_info, affected_tile))) {
						Lake *lake = define_lakes_iterator->GetLake(affected_tile);
						if (lake != NULL && lake->GetNumberOfLakeTiles() > 40) {
							int planned_height = terraformer_state.GetTileZ(affected_tile);
							Slope planned_slope = terraformer_state.GetPlannedSlope(affected_tile);
							if (planned_height != lake->GetSurfaceHeight() || planned_slope != SLOPE_FLAT) {
								DEBUG(map, 9, "Lake tile (%i,%i) would need to be terraformed for (%i,%i) to invalid slope / surface height in fine tune step.  Will not do that in order to safe the lake."
											  "This might lead to a gap in some river, but in general causes less damage to the landscape.", TileX(tile), TileY(tile),
											  TileX(affected_tile), TileY(affected_tile));
								success = false;

								MarkNotProcessed(water_info, tile);
								break;
							}
						}
					}
				}

				if (success) {
					DEBUG(map, RAINFALL_TERRAFORM_FOR_RIVERS_LOG_LEVEL, "Fine tuning terraforms tile (%i,%i) to height %i", TileX(tile), TileY(tile), desired_height);
					ExecuteTerraforming(terraformer_state);
				}
			}

			/* See comment of function LowerTileForDiagonalWater for explanation about this step */
			if (success && desired_slope == SLOPE_FLAT) {
				this->LowerTileForDiagonalWater(problem_tiles, tile, water_info, 1, 1, curr_affected_tiles);
				this->LowerTileForDiagonalWater(problem_tiles, tile, water_info, 1, -1, curr_affected_tiles);
				this->LowerTileForDiagonalWater(problem_tiles, tile, water_info, -1, 1, curr_affected_tiles);
				this->LowerTileForDiagonalWater(problem_tiles, tile, water_info, -1, -1, curr_affected_tiles);
			}

			if (success) {
				affected_tiles.insert(curr_affected_tiles.begin(), curr_affected_tiles.end());
			}

			problem_tiles.erase(tile);
		}

		/* If some of the tiles affected by terraforming now are not suitable for water any longer, add them to the problem tiles. */
		for (std::set<TileIndex>::const_iterator it = affected_tiles.begin(); it != affected_tiles.end(); it++) {
			TileIndex curr_tile = *it;

			if (WasProcessed(water_info, curr_tile)) {
				Slope slope = GetTileSlope(curr_tile);
				if (slope != SLOPE_FLAT && !IsInclinedSlope(slope)) {
					problem_tiles.insert(curr_tile);
				}
			}
		}

		DEBUG(map, 9, "After iteration %i: " PRINTF_SIZE " tiles affected, thus now " PRINTF_SIZE " problem tiles", iteration, affected_tiles.size(), problem_tiles.size());
	}
}

/** This function is meant to be called as very last step of river calculation, just before the tiles are actually declared water.
 *  Up to here, various algorithms have set up a picture about what tiles are planned to be rivers and lakes.  That picture is
 *  stored in the water_info array.
 *
 *  The problem is: Water may only be placed on flat or inclined (slope NW, NE, SW, SE) tiles.  But as those algorithms address the
 *  problem of river and lake generation from quite different perspectives, they cannot rule out the case that water is planned
 *  on inappropriate slopes completely.  For example, on an 1024x1024 map of the European Alps, at this point about 800 to 1000 tiles
 *  will be planned as water, but have an inappropriate slope.
 *
 *  The code which actually declares tiles water (remember, up to here, we only registered "make it water later"-decisions in the
 *  water-info array) is safe against incorrect slopes.  Thus, river generation will not crash because of such tiles, but rivers
 *  will have ugly looking gaps.
 *
 *  The task of this function is terraforming landscape on the small scale, until only planned water tiles with correct slope exist.
 */
void RainfallRiverGenerator::FineTuneTilesForWater(int *water_flow, byte *water_info, std::vector<TileWithHeightAndFlow> &water_tiles, DefineLakesIterator *define_lakes_iterator)
{
	/* Find all problem tiles, i.e. tiles that are planned as water, but not suitable in terms of slope. */
	std::set<TileIndex> problem_tiles = std::set<TileIndex>();
	for (TileIndex tile = 0; tile < MapSize(); tile++) {
		if (WasProcessed(water_info, tile) && !IsTileSuitableForRiver(tile)) {
			problem_tiles.insert(tile);
		}
	}

	DEBUG(map, RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL, "Will fine-tune water tiles in order to keep rivers and lakes connected");
	DEBUG(map, RAINFALL_FINETUNE_TILES_SUMMARY_LOG_LEVEL, ".... Starting with " PRINTF_SIZE " problematic tiles", problem_tiles.size());

	this->LowerHigherWaterTilesUntilValid(problem_tiles, water_flow, water_info, define_lakes_iterator);
}

/** The generator function.  The following steps are performed in this order when generating rivers:
 *  (1) Calculate a height index, for fast iteration over all tiles of a particular heightlevel.
 *  (2) Remove tiny basins, to (a) avoid rivers ending in tiny oceans, and (b) avoid generating too many senseless lakes.
 *  (3) Recalculate HeightIndex and NumberOfLowerTiles, as step (2) performed terraforming, and thus invalidated them.
 *  (4) Calculate flow.  Each tiles gains one unit of flow, while flowing downwards, it sums up.
 *  (5) Modify flow paths, to make the result look like rivers with curves and corners, and not like straight channels.
 *  (6) Define lakes, i.e. decide which lake covers which tiles, and decide about the lake surface height.
 *  (7) Prepare rivers and lakes.  Add additional corner tiles to rivers, if flow is diagonal.  Terraform to lake surface height.
 *  (8) Generate wider rivers and valleys.
 *  (9) Find problem tiles, i.e. tiles whose slope is not suitable for becoming river.
 * (10) Try to fix them using terraforming on the small scale.  Do (among a number of possible actions) what fixes most problem tiles.
 * (12) Lower tiles with not yet valid slope, until they are suitable for river.
 * (19) Finally make all tiles planned to become river/lakes river tiles in OpenTTD sense.
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

	/* (5) Make flow paths look more random, i.e. more realistic.  Necessary, since the number of lower paths (i.e.,
     *     series of tiles with descending number-of-lower-tiles) are rather straight towards the ocean.  And we want
     *     rivers, not just straight channels.
     */
	this->ModifyFlow(water_flow, water_info);

	/* (6) Define lakes, i.e. decide which tile is assigned to which lake, and which surface height lakes gain.
     *     Lakes start growing at points, where the number-of-lower-tiles iterator reported zero lower tiles.
	 *     Most code here is about correct bookkeeping regarding how much flow flows through the lakes.
	 *     In particular, the case of joining neighbor lakes is non-trivial with respect to this.
	 */
	DefineLakesIterator *define_lakes_iterator = new DefineLakesIterator(height_index, calculated_number_of_lower_tiles, water_flow, water_info);
	for (int h = _settings_game.construction.max_heightlevel; h >= 0; h--) {
		define_lakes_iterator->Calculate(h);
	}

	/* (7) Prepare rivers and lakes, e.g. add additional corner tiles if flow direction is diagonal,
	 *     and terraform lakes to their surface height.
     */
	std::vector<TileWithHeightAndFlow> water_tiles = std::vector<TileWithHeightAndFlow>();
	std::vector<TileWithValue> extra_river_tiles = std::vector<TileWithValue>();
	this->lake_connected_component_calculator = new LakeConnectedComponentCalculator(water_info);
	this->DeterminePlannedWaterTiles(water_tiles, water_flow, water_info, define_lakes_iterator);
	this->PrepareRiversAndLakes(water_tiles, water_flow, water_info, define_lakes_iterator, extra_river_tiles);
	this->AddExtraRiverTilesToWaterTiles(water_tiles, extra_river_tiles);

	/* (8) If flow exceeds certain flow bounds, optionally generate a wide river.  Using the same algorithm,
	 *     optionally make valleys wider, to gain more space.
	 */
	this->GenerateWiderRivers(water_flow, water_info, define_lakes_iterator, water_tiles);
	this->UpdateFlow(water_flow, water_tiles);

	/* (9) Find problem tiles, i.e. tiles whose slope is not suitable for becoming river. */
	std::set<TileIndex> problem_tiles = std::set<TileIndex>();
	GetProblemTiles(water_tiles, problem_tiles, water_info);

	/* (10) Try to fix them by performing terraforming on the small scale.  I.e., take the terraforming action
	 *      out of a number of schemes for the slope at hand, that fixes most river tiles with invalid slope.
	 */
	this->FixByLocalTerraforming(problem_tiles, water_flow, water_info, define_lakes_iterator);

	/* (12) The above algorithms worked rather heuristic.  They cannot guarantee that any planned river tile actually has
     *      correct slope (i.e., flat or inclined).  Thus, here we lower tiles until everything is ok.
	 *      (with the exception that we never touch lakes, thus a few tiles that cannot be made suitable for water may remain).
	 *      Note that our goal is applying this step to as few tiles as possible, as it can damage landscape in the large
	 *      scale when applied to too many tiles.  On an 1024x1024 test map, e.g. this step is applied to 50 or 100 tiles.
	 */
	this->FineTuneTilesForWater(water_flow, water_info, water_tiles, define_lakes_iterator);

	/* (19) Finally make tiles that are planned to be river river in the OpenTTD sense. */
	this->GenerateRiverTiles(water_tiles, water_info);

	delete this->lake_connected_component_calculator;

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

RainfallRiverGenerator::RainfallRiverGenerator()
{
	for (int n = 0; n < 31; n++) {
		this->slope_to_schemes[n] = std::vector<TerraformingScheme>();
	}

	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_SW, 0));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_NW, 1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 0));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, -1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, -1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 1, 1, 0));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 1, 0, -1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 1, 1, 1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, -1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_SW, 1, 1, 1));
	this->slope_to_schemes[SLOPE_W].push_back(TerraformingScheme(SLOPE_NW, 2, -1, -1));

	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_NE, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_SE, 0));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 0));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, 0));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 1, 0, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 1, 1, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, -1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_SE, 1, 1, 1));
	this->slope_to_schemes[SLOPE_E].push_back(TerraformingScheme(SLOPE_NE, 2, -1, -1));

	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_NW, 0));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_NE, 0));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, -1, -1, 0));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, -1, 0, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, -1, -1, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 0));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_NW, 1, 1, -1));
	this->slope_to_schemes[SLOPE_N].push_back(TerraformingScheme(SLOPE_NE, 1, -1, 1));

	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_SW, 0));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_SE, 0));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 0));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, 1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 1, 1, 0));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 1, 0, 1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 1, 1, -1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, 1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_SW, 1, 1, -1));
	this->slope_to_schemes[SLOPE_S].push_back(TerraformingScheme(SLOPE_SE, 1, -1, 1));

	this->slope_to_schemes[SLOPE_EW].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_EW].push_back(TerraformingScheme(SLOPE_FLAT, 1));

	this->slope_to_schemes[SLOPE_NS].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_NS].push_back(TerraformingScheme(SLOPE_FLAT, -1));

	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, -1));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_NW, 0));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_SW, -1));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 1));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 0));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, 1));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, -1, -1, 0));
	this->slope_to_schemes[SLOPE_NWS].push_back(TerraformingScheme(SLOPE_FLAT, -1, 0, 1));

	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 1));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_SW, 0));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_SE, 0));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, -1));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 1, -1, 0));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 1, 0, -1));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 0, -1, 0));
	this->slope_to_schemes[SLOPE_WSE].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, -1));

	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, -1));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_NE, 0));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_SE, -1));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, -1));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 0));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, -1));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, -1, 1, 0));
	this->slope_to_schemes[SLOPE_SEN].push_back(TerraformingScheme(SLOPE_FLAT, -1, 0, -1));

	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, 0));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, -1));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_NW, 0));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_NE, 0));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 1));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, 0, 1, 0));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, 0, 0, 1));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, -1, 1, 0));
	this->slope_to_schemes[SLOPE_ENW].push_back(TerraformingScheme(SLOPE_FLAT, -1, 0, 1));

	this->slope_to_schemes[SLOPE_STEEP_W].push_back(TerraformingScheme(SLOPE_NW, 0));
	this->slope_to_schemes[SLOPE_STEEP_W].push_back(TerraformingScheme(SLOPE_SW, -1));
	this->slope_to_schemes[SLOPE_STEEP_W].push_back(TerraformingScheme(SLOPE_NW, 1));
	this->slope_to_schemes[SLOPE_STEEP_W].push_back(TerraformingScheme(SLOPE_SW, 0));

	this->slope_to_schemes[SLOPE_STEEP_E].push_back(TerraformingScheme(SLOPE_NE, 0));
	this->slope_to_schemes[SLOPE_STEEP_E].push_back(TerraformingScheme(SLOPE_SE, -1));
	this->slope_to_schemes[SLOPE_STEEP_E].push_back(TerraformingScheme(SLOPE_NE, 1));
	this->slope_to_schemes[SLOPE_STEEP_E].push_back(TerraformingScheme(SLOPE_SE, 0));

	this->slope_to_schemes[SLOPE_STEEP_N].push_back(TerraformingScheme(SLOPE_NW, -1));
	this->slope_to_schemes[SLOPE_STEEP_N].push_back(TerraformingScheme(SLOPE_NE, -1));
	this->slope_to_schemes[SLOPE_STEEP_N].push_back(TerraformingScheme(SLOPE_NW, 0));
	this->slope_to_schemes[SLOPE_STEEP_N].push_back(TerraformingScheme(SLOPE_NE, 0));

	this->slope_to_schemes[SLOPE_STEEP_S].push_back(TerraformingScheme(SLOPE_SW, 0));
	this->slope_to_schemes[SLOPE_STEEP_S].push_back(TerraformingScheme(SLOPE_SE, 0));
	this->slope_to_schemes[SLOPE_STEEP_S].push_back(TerraformingScheme(SLOPE_SW, 1));
	this->slope_to_schemes[SLOPE_STEEP_S].push_back(TerraformingScheme(SLOPE_SE, 1));


	/* Calculating the bounds for wider rivers. */
	this->wide_river_bounds = std::vector<int>();
	this->wide_river_bounds.push_back(0);

	/* Calculate bound as <river_width>^(multiplier / 10) */
	int64 curr_bound;
	int curr_step = 1;
	do {
		curr_bound = (int64)_settings_newgame.game_creation.rainfall.flow_for_river * (int64)pow((double)curr_step, (double)_settings_newgame.game_creation.rainfall.wider_rivers_multiplier / (double)10);
		DEBUG(misc, 9, "Generating bounds: curr_step = %i, flow_for_river = %i, flow_for_river_i64 = " OTTD_PRINTF64 ", multiplier = %i, multiplier_double = %f, exponent = %f, pow = %f"
					  "pow_i64 = " OTTD_PRINTF64 ", curr_bound = " OTTD_PRINTF64 ", INT32_MAX = %i",
					  curr_step, _settings_newgame.game_creation.rainfall.flow_for_river, (int64)_settings_newgame.game_creation.rainfall.flow_for_river,
					  _settings_newgame.game_creation.rainfall.wider_rivers_multiplier, (double)_settings_newgame.game_creation.rainfall.wider_rivers_multiplier,
					  (double)_settings_newgame.game_creation.rainfall.wider_rivers_multiplier / (double)10,
					  pow((double)curr_step, (double)_settings_newgame.game_creation.rainfall.wider_rivers_multiplier / (double)10),
					  (int64)pow((double)curr_step, (double)_settings_newgame.game_creation.rainfall.wider_rivers_multiplier / (double)10),
					  curr_bound, INT32_MAX);
		curr_step++;
		if (curr_bound < INT32_MAX) {
			this->wide_river_bounds.push_back((int)curr_bound);
		}
	} while (curr_bound < INT32_MAX);
}
