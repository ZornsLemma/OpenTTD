/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file landscape_util.cpp Utility code related to inspecting and modifying the landscape, mainly during map generation.  */

#include "stdafx.h"
#include "debug.h"
#include "map_func.h"
#include "slope_func.h"
#include "tile_map.h"
#include "core/alloc_func.hpp"
#include "core/random_func.hpp"

#include "landscape_util.h"

#include "safeguards.h"

/*=================================== HeightIndex functions =======================================*/
/*=================================================================================================*/

/** Allocates an index array as described in the class comment of HeightIndex.
 *  @return index array as described in the class comment of HeightIndex
 */
byte** HeightIndex::ConstructHeightArray()
{
	int log_x = MapLogX();
	int log_y = MapLogY();
	int min_log = min(log_x, log_y);

	byte** height = CallocT<byte*>(min_log + 1);
	for (int n = 1; n <= min_log; n++) {
		int size = (MapSizeX() * MapSizeY()) / (1 << (n + n));;
		DEBUG(map, 9, "height[%i] receives size %i, where log_x = %i, log_y = %i", n, size, log_x, log_y);
		height[n] = CallocT<byte>(size);
	}

	return height;
}

/** Frees an index array as described in the class comment of HeightIndex.
 *  @param height_array height_array as described
 */
void HeightIndex::FreeHeightArray(byte** height_array)
{
	int min_log = min(MapLogX(), MapLogY());
	for (int n = 1; n <= min_log; n++) {
		DEBUG(map, 9, "Freeing %i", n);
		free(height_array[n]);
	}
	free(height_array);
}

/** Fills min_heigth and max_height index arrays as described in the class comment of HeightIndex.
 *
 *  Is called automatically from the constructor, but can be called at any later time to recalculate
 *  the index e.g. after some terraforming was done, without having to free/reallocate memory.
 *
 *  The precondition for the latter usecase obviously is that the map size did not change.
 */
void HeightIndex::Recalculate()
{
	uint min_log = min(MapLogX(), MapLogY());
	int step_size = 1;
	int prev_size_x = MapSizeX();
	int prev_size_y = MapSizeY();
	int curr_size_x = MapSizeX();
	int curr_size_y = MapSizeY();
	for (uint n = 1; n <= min_log; n++) {
		/* In step n, we fill this->min_height and this->max_height for grid rectangles of size 2^n times 2^n.
         */
		step_size <<= 1;   // The size of that grid rectangles, i.e. 2^n
		curr_size_x >>= 1; // The dimensions of the height array calculated in this step, i.e. MapSizeX() / 2^n and MapSizeY() / 2^n.
		curr_size_y >>= 1; // prev_size_x and prev_size_y are the same, but the values from the previous loop iteration.

		/* The arrays calculated in this, and in the previous loop iteration. */
		byte* curr_min_height = this->min_height[n];
		byte* prev_min_height = this->min_height[n - 1];
		byte* curr_max_height = this->max_height[n];
		byte* prev_max_height = this->max_height[n - 1];

		/* Step with the given step size over the whole map. */
		for (int x = 0; x < (int)MapSizeX(); x += step_size) {
			for (int y = 0; y < (int)MapSizeY(); y += step_size) {
				byte min_height = MAX_TILE_HEIGHT;
				byte max_height = 0;
				if (n == 1) {
					/* For grids of size 2x2, call GetTileZ for all four tiles of the grid, and take the minimum and maximum of those values. */
					for (int xx = x; xx < x + 2; xx++) {
						for (int yy = y; yy < y + 2; yy++) {
							byte height = GetTileZ(TileXY(xx, yy));
							DEBUG(map, 9, "Inspecting tile xx = %i, yy = %i, index = %x, height = %i", xx, yy, TileXY(xx, yy), height);
							min_height = min(min_height, height);
							max_height = max(max_height, height);
						}
					}
				} else {
					/* For bigger grids, take the values from the grids of the previous iteration.  I.e., take the minimum of all four minimums
					 * calculated in the previous iteration for the four sub-grids of the current grid, and do the same for the maxima.
					 */
					int start_x = x >> (n - 1);
					int start_y = y >> (n - 1);
					DEBUG(map, 9, "startX = %i, startY = %i, prev_size_x = %i, n = %i", start_x, start_y, prev_size_x, n);
					for (int xx = start_x; xx < start_x + 2; xx++) {
						for (int yy = start_y; yy < start_y + 2; yy++) {
							byte prev_min_h = prev_min_height[yy * prev_size_x + xx];
							byte prev_max_h = prev_max_height[yy * prev_size_x + xx];
							DEBUG(map, 9, "Inspecting section xx = %i, yy = %i, min_h = %i, max_h = %i", xx, yy, prev_min_h, prev_max_h);
							min_height = min(min_height, prev_min_h);
							max_height = max(max_height, prev_max_h);
						}
					}
				}
				DEBUG(map, 9, "y = %i, sizeX = %i, x = %i, n = %i, index = %i, step_size = %i", y, MapSizeX(), x, n, (y / step_size) * curr_size_x + x / step_size, step_size);

				/* In either case, store the calculated minima and maxima in the array at the position corresponding to the storage scheme
				 * described in the class comment of HeightIndex.
				 */
				curr_min_height[(y / step_size) * curr_size_x + x / step_size] = min_height;
				curr_max_height[(y / step_size) * curr_size_x + x / step_size] = max_height;

				DEBUG(map, 9, "n: %i, x = %i, y = %i, min: %i, max: %i", n, x, y, min_height, max_height);
			}
		}
		prev_size_x >>= 1;
		prev_size_y >>= 1;
	}
}

/** Allocates and calculates an HeightIndex as described in the class comment.
 */
HeightIndex::HeightIndex()
{
	this->min_height = this->ConstructHeightArray();
	this->max_height = this->ConstructHeightArray();

	this->Recalculate();
}

/** Frees all space allocated by the HeightIndex.
 */
HeightIndex::~HeightIndex()
{
	this->FreeHeightArray(this->min_height);
	this->FreeHeightArray(this->max_height);
}

/*================================= HeightIterator functions ======================================*/
/*=================================================================================================*/

/** This function, called for some index point (x,y) in the height arrays the HeightIndex relies on,
 *  performs all necessary recursive calls, until at the bottom level of recursion, ProcessTile is
 *  called for exactly the desired tiles.  For each (x,y) at given depth this function receives, it
 *  has four candidate (x1,y1), (x2,y2), (x3,y3) and (x4,y4) at the next level of recursion.  It
 *  checks them using the min_height and max_height bounds stored in the HeightIndex.  The recursive
 *  call is performed if and only if min_height(xi,yi) <= heightlevel and max_height(xi,yi) >= heightlevel
 *  according to the HeightIndex.
 *
 *  @param heightlevel finally call ProcessTile only for tiles of this heightlevel
 *  @param depth the log_2 of the grid sections in terms of the HeightIndex, this function in this level
 *         of recursion operates on
 *  @param x x coordinate in the HeightIndex at current level of recursion
 *  @param y y coordinate in the HeightIndex at current level of recursion
 */
void HeightLevelIterator::CalculateRecursive(int heightlevel, int depth, int x, int y)
{
	if (depth == 0) {
		/* We ended up at grid level zero.  I.e. from a call with depth == 1, this code is
		 * called for exactly four Tiles, out of which at least one has the searched
		 * heightlevel.  Filter for that heightlevel, and if heightlevel match, finally
	     * call ProcessTile.
		 */
		TileIndex tile = TileXY(x, y);

		int tile_height;
		Slope slope = GetTileSlope(tile, &tile_height);

		/* Only call ProcessTile if the heightlevel fits,
		 * and additinally ignore the technically existing, but actually invisible tiles shortly outside map */
		if (heightlevel == tile_height && x > 0 && y > 0 && x < (int)MapMaxX() && y < (int)MapMaxY()) {
			this->ProcessTile(tile, slope);
		}
	} else {
	    /* Fetch the index arrays using inline getters */
		byte** min_height_array = this->height_index->GetMinHeightArray();
		byte** max_height_array = this->height_index->GetMaxHeightArray();

		/* We inspect a quadratic section of 2^depth tiles.  min_height_array and
		 * max_height_array tell us wether that grid section contains at least one
		 * tile of the searched heightlevel.  If yes, we make a recursive call,
		 * if no, we return and never touch that grid section again in this particular
		 * call to Calculate().
		 */
		int row_length = 1 << (MapLogX() - depth);
		byte min_height = min_height_array[depth][y * row_length + x];
		byte max_height = max_height_array[depth][y * row_length + x];

		DEBUG(map, 9, "CalculateRecursive: [depth %i][min_height %i][max_height %i][x %i][y %i]",
						depth, min_height, max_height, x, y);

		if ((byte)heightlevel >= min_height && (byte)heightlevel <= max_height) {
			this->CalculateRecursive(heightlevel, depth - 1, 2 * x,     2 * y);
			this->CalculateRecursive(heightlevel, depth - 1, 2 * x + 1, 2 * y);
			this->CalculateRecursive(heightlevel, depth - 1, 2 * x,     2 * y + 1);
			this->CalculateRecursive(heightlevel, depth - 1, 2 * x + 1, 2 * y + 1);
		}
	}
}

/** Do the calculations of this iterator, for all Tiles of the given heightlevel.
 *  To do things as efficient as possible, use the index information in the HeightIndex passed
 *  to the constructor.
 *
 *  @param heightlevel call ProcessTile function only for Tiles with the given heightlevel
 */
void HeightLevelIterator::Calculate(int heightlevel)
{
	uint min_log = min(MapLogX(), MapLogY());
	for (int y = 0; y < 1 << (MapLogY() - min_log); y++) {
		for (int x = 0; x < 1 << (MapLogX() - min_log); x++) {
			DEBUG(map, 9, "Calling CalculateRecursive for height = %i, min_log = %i, x = %i, y = %i", heightlevel, min_log, x, y);
			this->CalculateRecursive(heightlevel, min_log, x, y);
		}
	}
}

/*============================ Inspecting the neighborhood of tiles ================================*/
/*=================================================================================================*/

Direction GetOppositeDirection(Direction direction_index)
{
	return (Direction)((direction_index + 4) % DIR_COUNT);
}

/** Stores the TileIndex of all straight neighbor tiles of the given tile into the given TileIndex array,
 *  at the indices given by enum Direction.  If a tile doesn´t exist, since it is beyond the map edge,
 *  INVALID_TILE is stored.
 *  Straight neighbor tiles are the north-east, north-west, south-east and south-west neighbor tiles.
 *  @param tile the tile
 *  @param neighbor_tiles array of length 8
 */
void StoreStraightNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT])
{
	int x = TileX(tile);
	int y = TileY(tile);

	neighbor_tiles[DIR_NE] = x > 1                  ? TileXY(x - 1, y) : INVALID_TILE;
	neighbor_tiles[DIR_NW] = y > 1                  ? TileXY(x, y - 1) : INVALID_TILE;
	neighbor_tiles[DIR_SW] = x < (int)MapMaxX() - 1 ? TileXY(x + 1, y) : INVALID_TILE;
	neighbor_tiles[DIR_SE] = y < (int)MapMaxY() - 1 ? TileXY(x, y + 1) : INVALID_TILE;
}

/** Stores the TileIndex of all diagonal neighbor tiles of the given tile into the given TileIndex array,
 *  at the indices given by enum Direction.  If a tile doesn´t exist, since it is beyond the map edge,
 *  INVALID_TILE is stored.
 *  Diagonal neighbor tiles are the northern, western, eastern and southern neighbor tiles.
 *  @param tile the tile
 *  @param neighbor_tiles array of length 8
 */
void StoreDiagonalNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT])
{
	int x = TileX(tile);
	int y = TileY(tile);

	neighbor_tiles[DIR_N] = x > 1 && y > 1                                   ? TileXY(x - 1, y - 1) : INVALID_TILE;
	neighbor_tiles[DIR_E]  = x > 1 && y < (int)MapMaxY() - 1                  ? TileXY(x - 1, y + 1) : INVALID_TILE;
	neighbor_tiles[DIR_W]  = x < (int)MapMaxX() - 1 && y > 1                  ? TileXY(x + 1, y - 1) : INVALID_TILE;
	neighbor_tiles[DIR_S] = x < (int)MapMaxX() - 1 && y < (int)MapMaxY() - 1 ? TileXY(x + 1, y + 1) : INVALID_TILE;
}

/** Stores the TileIndex of all neighbor tiles of the given tile, at the indices given by enum Direction.
 *  If tile doesn´t exist since it is beyond the map edge, INVALID_TILE is stored.
 *  @param tile the tile
 *  @param neighbor_tiles array of length 8
 */
void StoreAllNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT])
{
	StoreStraightNeighborTiles(tile, neighbor_tiles);
	StoreDiagonalNeighborTiles(tile, neighbor_tiles);
}

/** Given an array of neighbor tiles of some tile, this function calculates and stores both
 *  slope and (minimum) height of the neighbor tiles into the given arrays.  Only indices
 *  with a value not equal to INVALID_TILE are considered.
 *  @param neighbor_tiles array of neighbor tiles
 *  @param neighbor_slopes to be filled array of neighbor slopes
 *  @param neighbor_heights to be filled array of neighbor heights
 */
void StoreSlopes(TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT])
{
	for (uint n = DIR_BEGIN; n < DIR_END; n++) {
		if (neighbor_tiles[n] != INVALID_TILE) {
			neighbor_slopes[n] = GetTileSlope(neighbor_tiles[n], &neighbor_heights[n]);
		}
	}
}

/** Given an array of neighbor tiles of some tile, this function replaces all tiles,
 *  for which the corresponding index in the invalidate_mask array is true by the
 *  INVALID_TILE.
 *  @param neighbor_tiles array of neighbor tiles
 *  @param invalidate_mask invalidate mask as described
 */
void InvalidateTiles(TileIndex neighbor_tiles[DIR_COUNT], bool invalidate_mask[DIR_COUNT])
{
	for (uint n = DIR_BEGIN; n < DIR_END; n++) {
		if (invalidate_mask[n]) {
			neighbor_tiles[n] = INVALID_TILE;
		}
	}
}

/** Returns the direction from the given source tile towards the given destination tile.
 */
Direction GetDirection(TileIndex source_tile, TileIndex dest_tile)
{
	int sx = TileX(source_tile);
	int sy = TileY(source_tile);
	int dx = TileX(dest_tile);
	int dy = TileY(dest_tile);
	if (dx < sx) {
		if (dy < sy) {
			return DIR_N;
		} else if (dy == sy) {
			return DIR_NE;
		} else {
			return DIR_E;
		}
	} else if (dx == sx) {
		if (dy < sy) {
			return DIR_NW;
		} else if (dy == sy) {
			assert(false);
		} else {
			return DIR_SE;
		}
	} else {
		if (dy < sy) {
			return DIR_W;
		} else if (dy == sy) {
			return DIR_SW;
		} else {
			return DIR_S;
		}
	}

	/* Should never be reached.  All cases are handled above; here we only want to avoid the compiler warning. */
	return DIR_W;
}

/** Given an inclined slope, this function returns the direction where the lower tile is located (relative to the given tile).
 *  @param inclined_slope any inclined slope
 *  @return direction towards the lower tile
 */
Direction GetLowerDirectionForInclinedSlope(Slope inclined_slope)
{
	switch(inclined_slope) {
		case SLOPE_NW: return DIR_SE;
		case SLOPE_NE: return DIR_SW;
		case SLOPE_SW: return DIR_NE;
		case SLOPE_SE: return DIR_NW;
		default: assert(false);
				 /* Should never be reached.  But avoid the compiler warning. */
				 return DIR_SW;
	}
}

/*================================ Angles <--> DirectionIndices ===================================*/
/*=================================================================================================*/

/** Returns an angle (on the scale 0..360 degrees) from the given direction.  North is zero degrees, east 90 degrees, and so on.
 */
int GetAngleFromDirection(Direction direction)
{
	switch(direction) {
		case DIR_N      : return 0;
		case DIR_NE : return 45;
		case DIR_E       : return 90;
		case DIR_SE : return 135;
		case DIR_S      : return 180;
		case DIR_SW : return 225;
		case DIR_W       : return 270;
		case DIR_NW : return 315;
		default: assert(false);
				 /* Should never be reached.  But avoid the compiler warning. */
				 return 0;
	}
}

/** Derives a Direction from the given angle.  Each Direction is
 *  assigned a region of size 45 degrees symmetric around the angle numbers used
 *  in GetAngleFromDirection.
 *  @param angle angle
 *  @return the Direction as described
 */
Direction GetDirectionFromAngle(int angle)
{
	while (angle < 0) {
		angle += 360;
	}
	angle = angle % 360;

	if (angle < 158) {
		if (angle < 68) {
			return angle < 23 ? DIR_N : DIR_NE;
		} else {
			return angle < 113 ? DIR_E : DIR_SE;
		}
	} else if (angle < 338) {
		if (angle < 248) {
			return angle < 203 ? DIR_S : DIR_SW;
		} else {
			return angle < 293 ? DIR_W : DIR_NW;
		}
	} else {
		return DIR_N;
	}
}

/** Given an array of tiles, this function fills a passed array with those tiles, but sorted
 *  by their closeness to the given angle.  Angle regions are as of GetDirectionFromAngle.
 *  The decision wether first the neighbor tile in negative angle direction, or the corresponding
 *  one in positive angle direction is added, is taken by random.
 *  Note that the resulting array can contain INVALID_TILES, i.e. code using this function must
 *  test for that.
 *  @param tiles array of eight tiles
 *  @param angle some angle
 *  @param sorted_tiles array to be filled as described
 */
void SortTilesByAngle(TileIndex tiles[DIR_COUNT], int angle, TileIndex sorted_tiles[DIR_COUNT])
{
	int n = 0;
	Direction start_direction = GetDirectionFromAngle(angle);
	sorted_tiles[n] = tiles[start_direction];
	n++;
	Direction curr_prev_direction = start_direction;
	Direction curr_next_direction = start_direction;
	for (int z = 0; z < 3; z++) {
		curr_prev_direction = GetPrevDirection(curr_prev_direction);
		curr_next_direction = GetNextDirection(curr_next_direction);
		int r = RandomRange(2);
		sorted_tiles[n] =     r == 0 ? tiles[curr_prev_direction] : tiles[curr_next_direction];
		sorted_tiles[n + 1] = r == 0 ? tiles[curr_next_direction] : tiles[curr_prev_direction];
		n += 2;
	}
	curr_prev_direction = GetPrevDirection(curr_prev_direction);
	sorted_tiles[n] = tiles[curr_prev_direction];
}

/*=================================== Debug helper functions ======================================*/
/*=================================================================================================*/

/** Returns the given slope as const char*.
 *  (meant for debugging)
 *  @param slope some slope
 *  @return slope as const char*
 */
const char* SlopeToString(Slope slope)
{
	switch (slope) {
		case SLOPE_FLAT:  return "SLOPE_FLAT";
		case SLOPE_W:  return "SLOPE_W";
		case SLOPE_S:  return "SLOPE_S";
		case SLOPE_E:  return "SLOPE_E";
		case SLOPE_N:  return "SLOPE_N";
		case SLOPE_NW:  return "SLOPE_NW";
		case SLOPE_SW:  return "SLOPE_SW";
		case SLOPE_SE:  return "SLOPE_SE";
		case SLOPE_NE:  return "SLOPE_NE";
		case SLOPE_EW:  return "SLOPE_EW";
		case SLOPE_NS:  return "SLOPE_NS";
		case SLOPE_NWS:  return "SLOPE_NWS";
		case SLOPE_WSE:  return "SLOPE_WSE";
		case SLOPE_SEN:  return "SLOPE_SEN";
		case SLOPE_ENW:  return "SLOPE_ENW";
		case SLOPE_STEEP_W:  return "SLOPE_STEEP_W";
		case SLOPE_STEEP_S:  return "SLOPE_STEEP_S";
		case SLOPE_STEEP_E:  return "SLOPE_STEEP_E";
		case SLOPE_STEEP_N:  return "SLOPE_STEEP_N";
		default: return "Unknown";
	}
}

const char* DirectionToString(Direction direction) {
	switch(direction) {
		case DIR_N      : return "NORTH";
		case DIR_NE : return "NORTH_EAST";
		case DIR_E       : return "EAST";
		case DIR_SE : return "SOUTH_EAST";
		case DIR_S      : return "SOUTH";
		case DIR_SW : return "SOUTH_WEST";
		case DIR_W       : return "WEST";
		case DIR_NW : return "NORTH_WEST";
		default: return "Unknown";
	}
}

/** Prints the neighborhood of a tile (slopes, heights) to DEBUG in a short one-line-style.
 */
void DebugTileInfo(int level, TileIndex tile, Slope slope, int height, TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT])
{
	DEBUG(map, level, "TILE (%i,%i)=(%s,%i): N=(%s,%i),NW=(%s,%i),NE=(%s,%i),W=(%s,%i),E=(%s,%i),SW=(%s,%i),SE=(%s,%i),S=(%s,%i)",
				   TileX(tile), TileY(tile),
				   SlopeToString(slope), height,
				   neighbor_tiles[DIR_N] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_N]) : "NONE",
				   neighbor_tiles[DIR_N] != INVALID_TILE ? neighbor_heights[DIR_N] : -1,
				   neighbor_tiles[DIR_NW] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_NW]) : "NONE",
				   neighbor_tiles[DIR_NW] != INVALID_TILE ? neighbor_heights[DIR_NW] : -1,
				   neighbor_tiles[DIR_NE] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_NE]) : "NONE",
				   neighbor_tiles[DIR_NE] != INVALID_TILE ? neighbor_heights[DIR_NE] : -1,
				   neighbor_tiles[DIR_W] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_W]) : "NONE",
				   neighbor_tiles[DIR_W] != INVALID_TILE ? neighbor_heights[DIR_W] : -1,
				   neighbor_tiles[DIR_E] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_E]) : "NONE",
				   neighbor_tiles[DIR_E] != INVALID_TILE ? neighbor_heights[DIR_E] : -1,
				   neighbor_tiles[DIR_SW] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_SW]) : "NONE",
				   neighbor_tiles[DIR_SW] != INVALID_TILE ? neighbor_heights[DIR_SW] : -1,
				   neighbor_tiles[DIR_SE] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_SE]) : "NONE",
				   neighbor_tiles[DIR_SE] != INVALID_TILE ? neighbor_heights[DIR_SE] : -1,
				   neighbor_tiles[DIR_S] != INVALID_TILE ? SlopeToString(neighbor_slopes[DIR_S]) : "NONE",
				   neighbor_tiles[DIR_S] != INVALID_TILE ? neighbor_heights[DIR_S] : -1);
}


/** Performs a breadth first search, starting from the given tile
 *  @param from_tile start tile
 *  @return wether we find the destination.  May always return false, if the goal is just doing some
 *          work on all found tiles, instead of finding some particular tile
 */
bool BreadthFirstSearch::PerformSearch(TileIndex from_tile)
{
	std::set<TileIndex> dirty_tiles = std::set<TileIndex>();
	dirty_tiles.insert(from_tile);
	return this->DoPerformSearch(dirty_tiles);
}

/** Performs a breadth first search, starting with the given set of tiles
 *  @param from_tiles set of start tiles
 *  @return wether we find the destination.  May always return false, if the goal is just doing some
 *          work on all found tiles, instead of finding some particular tile
 */
bool BreadthFirstSearch::PerformSearch(std::set<TileIndex> &from_tiles)
{
	std::set<TileIndex> dirty_tiles = std::set<TileIndex>();
	dirty_tiles.insert(from_tiles.begin(), from_tiles.end());
	return this->DoPerformSearch(dirty_tiles);
}

/** Performs a breadth first search, starting with the given vector of tiles
 *  @param from_tiles vector of start tiles
 *  @return wether we find the destination.  May always return false, if the goal is just doing some
 *          work on all found tiles, instead of finding some particular tile
 */
bool BreadthFirstSearch::PerformSearch(std::vector<TileIndex> &from_tiles)
{
	std::set<TileIndex> dirty_tiles = std::set<TileIndex>();
	dirty_tiles.insert(from_tiles.begin(), from_tiles.end());
	return this->DoPerformSearch(dirty_tiles);
}

/** Actually perform the breadth first search, starting with the given set of dirty tiles.
 *  @param dirty_tiles tiles to start with
 *  @return wether we find the destination.  May always return false, if the goal is just doing some
 *          work on all found tiles, instead of finding some particular tile
 */
bool BreadthFirstSearch::DoPerformSearch(std::set<TileIndex> &dirty_tiles)
{
	this->iteration = 0;
	TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;

	while(dirty_tiles.size() > 0) {
		std::vector<TileIndex> next_dirty_tiles = std::vector<TileIndex>();

		for (std::set<TileIndex>::const_iterator it2 = dirty_tiles.begin(); it2 != dirty_tiles.end(); it2++) {
			TileIndex curr_tile = *it2;

			bool found = this->ProcessTile(curr_tile);
			if (found) {
				/* Success, found what we searched for */
				return true;
			}

			for (int n = 0; n < DIR_COUNT; n++) {
				neighbor_tiles[n] = INVALID_TILE;
			}

			/* Scan neighbor tiles. */
			this->StoreNeighborTiles(curr_tile, neighbor_tiles);
			for (int n = 0; n < DIR_COUNT; n++) {
				TileIndex neighbor_tile = neighbor_tiles[n];

				/* If the neighbor tile is ok for our search, record it for the next iteration */
				if (neighbor_tile != INVALID_TILE
					  && TakeNeighborTileIntoAccount(neighbor_tile)
					  && dirty_tiles.find(neighbor_tile) == dirty_tiles.end()) {
					next_dirty_tiles.push_back(neighbor_tile);
				}
			}
		}

		dirty_tiles.clear();
		dirty_tiles.insert(next_dirty_tiles.begin(), next_dirty_tiles.end());

		this->iteration++;
	}

	return false;
}
