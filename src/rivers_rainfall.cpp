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
#include "terraform_func.h"
#include "tile_map.h"
#include "water_map.h"

#include "core/alloc_func.hpp"
#include "core/random_func.hpp"

#include "rivers_rainfall.h"

#include <algorithm>
#include <vector>

#include "safeguards.h"

/* Debug copies of the arrays calculated in the river generator, to allow debugging inspection via the map info window. */
int *_number_of_lower_tiles = NULL;

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

/** The generator function.  The following steps are performed in this order when generating rivers:
 *  (1) Calculate a height index, for fast iteration over all tiles of a particular heightlevel.
 *  (2) Remove tiny basins, to (a) avoid rivers ending in tiny oceans, and (b) avoid generating too many senseless lakes.
 *  (3) Recalculate HeightIndex and NumberOfLowerTiles, as step (2) performed terraforming, and thus invalidated them.
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

	/* Debug code: Store the results of the iterators in a public array, to be able to query them in the LandInfoWindow.
	 *             Without that possibility, debugging those values would be really hard. */
	if (_number_of_lower_tiles != NULL) {
		free(_number_of_lower_tiles);
		_number_of_lower_tiles = NULL;
	}

	_number_of_lower_tiles = CallocT<int>(MapSizeX() * MapSizeY());
	for (uint n = 0; n < MapSizeX() * MapSizeY(); n++) {
		_number_of_lower_tiles[n] = calculated_number_of_lower_tiles[n];
	}

	/*** (Debugging code end ***/

	/* Delete iterators last, as the data arrays constructed by them are in use outside them */
	delete lower_iterator;
	delete height_index;
}
