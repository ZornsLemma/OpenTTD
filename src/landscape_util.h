/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file landscape_util.h Utility code related to inspecting and modifying the landscape, mainly during map generation. */

#ifndef LANDSCAPE_UTIL_H
#define LANDSCAPE_UTIL_H

#include "map_func.h"
#include "slope_type.h"

/** The purpose of a HeightIndex is support for efficient access to all tiles
 *  of a given height.  E.g. a HeightIterator, which offers an interface for
 *  iterating over all tiles of a given height, is backed by a HeightIndex.
 *  Height here always refers to GetTileZ, not to TileHeight.
 *
 *  Internally, a HeightIndex uses two arrays of byte arrays for storing
 *  the minimum and maximum height of certain sections of the landscape.
 *  More precisely, min_height[n] (n element of 1 .. min(MapLogX(), MapLogY()))
 *  is an array storing the minimum heights of all grids of size 2^n tiles.
 *
 *  Example: For a 128x64 map, min_height is built as follows:
 *  - min_height[0] = NULL as always (to avoid the extra offset, simplifying calculations)
 *  - min_height[1] = byte[2048] storing the minimum height in all 2x2 sections of the map
 *  - min_height[2] = byte[512]  storing the minimum height in all 4x4 sections of the map
 *  - min_height[3] = byte[128]  storing the minimum height in all 8x8 sections of the map
 *  - min_height[4] = byte[32]   storing the minimum height in all 16x16 sections of the map
 *  - min_height[5] = byte[8]    storing the minimum height in all 32x32 sections of the map
 *  - min_height[6] = byte[2]    storing the minimum height in all 64x64 sections of the map
 *
 *  max_height works exactly the same way.
 *
 *  Thus, the memory footprint of the index is a bit more than 5/8 the memory footprint of
 *  storing all heightlevels of the map itself.
 *
 *  In the example above,
 *  - min_height[6][0] = 0, min_height[6][1] = 2, and
 *  - max_height[6][0] = 4, max_height[6][1] = 14 might indicate that in the north-west section
 *  of the map, a rather flat area is located, whereas in the south-east section a high mountain
 *  is located.
 *
 *  Thus, an iterator which is told "iterate over all tiles of height 14" can already discard
 *  half the map at the top level of recursion, tiles of height 14 can only be located
 *  in section (64,0) to (127,63).  By applying the same idea to all levels of recursion,
 *  iterating over all tiles of a given height using that index is considerably cheaper than a
 *  scan of all tiles.
 */
struct HeightIndex {

private:
	byte** min_height; ///< minimum height of certain rectangular sections of map, as described above
	byte** max_height; ///< maximum height of certain rectangular sections of map, as described above

	byte** ConstructHeightArray();
	void FreeHeightArray(byte** height_array);

public:
	HeightIndex();
	~HeightIndex();

	void Recalculate();

	/** Accessor function to the min_height array.  We offer access to the whole array,
     *  since this makes HeightIterators a bit more efficient.
     *  @return the min_height array
     */
	inline byte** GetMinHeightArray() { return this->min_height; }

	/** Accessor function to the max_height array.
     *  @return the max_height array
     */
	inline byte** GetMaxHeightArray() { return this->max_height; }
};

/** A HeightLevelIterator iterates over all Tiles with a specified height, using the index information
 *  in a given HeightIndex.  See the HeightIndex documentation for more information.
 *
 *  Based on the underlying HeightIndex, the iterator can exclude large sections of the map without
 *  having a detailed look, which makes things much more efficient.
 *
 *  The typical usecase is doing something first for all tiles of height 0, then for all tiles of
 *  height one, and so on...  The "doing something" is subclass-specific, i.e. this is an abstract
 *  class you should subclass but never instantiate.
 */
struct HeightLevelIterator {

private:
	HeightIndex *height_index;

	void CalculateRecursive(int heightlevel, int depth, int x, int y);

protected:

	virtual void ProcessTile(TileIndex tile, Slope slope) = 0;

	/** Constructs a new HeightLevelIterator based on a given HeightIndex.
     *  @param height_index the height_index to be used by this iterator
	 */
	HeightLevelIterator(HeightIndex *height_index) { this->height_index = height_index; }

public:
	/** Empty virtual destructor to make the compiler happy.
	 */
	virtual ~HeightLevelIterator() {}

	void Calculate(int heightlevel);
};

#endif /* LANDSCAPE_UTIL_H */
