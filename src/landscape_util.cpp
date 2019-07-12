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
#include "tile_map.h"
#include "core/alloc_func.hpp"

#include "landscape_util.h"

#include "safeguards.h"

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

