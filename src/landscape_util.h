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

#include "direction_type.h"
#include "map_func.h"
#include "slope_func.h"
#include "slope_type.h"
#include "tile_map.h"
#include "debug.h"

#include <set>
#include <vector>

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

/** Small helper struct for sorting TileIndices using some given value.
 */
struct TileWithValue {
	TileIndex tile;
	uint value;

	TileWithValue(TileIndex tile, uint value)
	{
		this->tile = tile;
		this->value = value;
	}

	bool operator < (const TileWithValue& other) const
	{
        return (this->value < other.value);
	}

	bool operator > (const TileWithValue& other) const
    {
        return (this->value > other.value);
    }
};

Direction GetOppositeDirection(Direction direction_index);

#define EMPTY_NEIGHBOR_TILES { INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE, INVALID_TILE }

void StoreStraightNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT]);
void StoreDiagonalNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT]);
void StoreAllNeighborTiles(TileIndex tile, TileIndex neighbor_tiles[DIR_COUNT]);
void StoreSlopes(TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT]);
void InvalidateTiles(TileIndex neighbor_tiles[DIR_COUNT], bool invalidate_mask[DIR_COUNT]);

const char* SlopeToString(Slope slope);

inline bool IsValidSlopeForRiver(Slope s) { return s == SLOPE_FLAT || IsInclinedSlope(s); }
inline bool IsTileSuitableForRiver(TileIndex tile) { return IsValidSlopeForRiver(GetTileSlope(tile)); }

void DebugTileInfo(int level, TileIndex tile, Slope slope, int height, TileIndex neighbor_tiles[DIR_COUNT], Slope neighbor_slopes[DIR_COUNT], int neighbor_heights[DIR_COUNT]);

/** This class provides functionality for determining a connected component on the map.
 *  Which tiles are recognized is subclass-specific (e.g. all tiles of a certain
 *  heightlevel), furthermore the kind of container where the tiles belonging to the
 *  connected component are to be stored is also subclass-specific via the template-parameter.
 *
 *  Note that collecting additional information beside the TileIndex into that container
 *  is not forbidden.
 */
template <class T>
struct ConnectedComponentCalculator {

private:
	/** If and only if this variable is true, the calculator will not just consider straight
	 *  neighbors, but also diagonal neighbors.
	 */
	bool use_diagonal_neighbors;

protected:
	/** If and only if this function returns true for the tile at hand, it can belong
     *  to the connected component.
	 *
     *  NOTE: This function should usually check wether the tile is already registered.
     *  If so, it should return false to prevent endless recursion.
     *
     *  @param container the so far registered tiles of the connected component
     *  @param tile the candidate tile
	 *  @param prev_tile tile visited before the current tile, i.e. in the parent call of the recursion.
	 *            In some circumstances, the way the two tiles are connected may influence the decision.
     *  @return if the tile can belong to the connected component
     */
	virtual bool RecognizeTile(T &container, TileIndex tile, TileIndex prev_tile) = 0;

	/** Stores the given tile in the container.
     *  @param container the container
     *  @param tile the new tile
     */
	virtual void StoreInContainer(T &container, TileIndex tile) = 0;

public:
	ConnectedComponentCalculator(bool use_diagonal_neighbors = true) { this->use_diagonal_neighbors = use_diagonal_neighbors; }
	virtual ~ConnectedComponentCalculator() {}

	/** Stores a connected component around the given tile in the given container.
	 *  Calls itself recursively for appropriate neighbor tiles, until the whole
	 *  connected component is determined.
     *  Implementation note: I searched the right syntax for placing this templated
     *  function into landscape_util.cpp without success - is it possible at all?
	 *  @param container the initially empty container
	 *  @param tile the starting tile
     *  @param max_container_size optional maximum container size to prevent memory overflows,
	 *                            use this if aborting the calculation at some point in time is
	 *                            better than correctly adding millions of tiles to the contains,
	 *                            at the risk of hard abortion because of memory overflow.
	 *  @param prev_tile Tile visited before the current tile, i.e. in the parent call of the recursion.
	 */
	void StoreConnectedComponent(T &container, TileIndex tile, uint max_container_size = 0, TileIndex prev_tile = INVALID_TILE)
	{
		TileIndex neighbor_tiles[DIR_COUNT] = EMPTY_NEIGHBOR_TILES;

		std::set<TileIndex> dirty_tiles = std::set<TileIndex>();
		dirty_tiles.insert(tile);

		while (dirty_tiles.size() > 0) {
			std::vector<TileIndex> next_dirty_tiles = std::vector<TileIndex>();

			for (std::set<TileIndex>::const_iterator it2 = dirty_tiles.begin(); it2 != dirty_tiles.end(); it2++) {
				TileIndex curr_tile = *it2;

				this->StoreInContainer(container, curr_tile);
				if (max_container_size != 0 && container.size() >= max_container_size) {
					return;
				}

				if (this->use_diagonal_neighbors) {
					StoreAllNeighborTiles(curr_tile, neighbor_tiles);
				} else {
					StoreStraightNeighborTiles(curr_tile, neighbor_tiles);
				}

				for (uint n = 0; n < DIR_COUNT; n++) {
					TileIndex neighbor_tile = neighbor_tiles[n];

					/* Note that RecognizeTile ususally needs to perform a check wether we already have
					 * have registered the tile.  Otherwise, we would generate an endless recursion here.
					 */
					if (neighbor_tile != INVALID_TILE && this->RecognizeTile(container, neighbor_tile, curr_tile)) {
						next_dirty_tiles.push_back(neighbor_tile);
					}
				}
			}

			dirty_tiles.clear();
			dirty_tiles.insert(next_dirty_tiles.begin(), next_dirty_tiles.end());
		}
	}
};

#endif /* LANDSCAPE_UTIL_H */
