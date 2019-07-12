/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rivers_path.cpp The original river generator based on path-finding. */

#include "stdafx.h"

#include "genworld.h"
#include "landscape.h"
#include "landscape_type.h"
#include "map_func.h"
#include "slope_func.h"
#include "slope_type.h"
#include "tile_map.h"
#include "tile_type.h"
#include "water.h"
#include "water_map.h"

#include "core/mem_func.hpp"
#include "core/random_func.hpp"
#include "pathfinder/npf/aystar.h"

#include "rivers_path.h"

#include <list>
#include <set>

#include "safeguards.h"

/**
 * Find the spring of a river.
 * @param tile The tile to consider for being the spring.
 * @param user_data Ignored data.
 * @return True iff it is suitable as a spring.
 */
bool PathRiverGenerator::FindSpring(TileIndex tile, void *user_data)
{
	int referenceHeight;
	if (!IsTileFlat(tile, &referenceHeight) || IsWaterTile(tile)) return false;

	/* In the tropics rivers start in the rainforest. */
	if (_settings_game.game_creation.landscape == LT_TROPIC && GetTropicZone(tile) != TROPICZONE_RAINFOREST) return false;

	/* Are there enough higher tiles to warrant a 'spring'? */
	uint num = 0;
	for (int dx = -1; dx <= 1; dx++) {
		for (int dy = -1; dy <= 1; dy++) {
			TileIndex t = TileAddWrap(tile, dx, dy);
			if (t != INVALID_TILE && GetTileMaxZ(t) > referenceHeight) num++;
		}
	}

	if (num < 4) return false;

	/* Are we near the top of a hill? */
	for (int dx = -16; dx <= 16; dx++) {
		for (int dy = -16; dy <= 16; dy++) {
			TileIndex t = TileAddWrap(tile, dx, dy);
			if (t != INVALID_TILE && GetTileMaxZ(t) > referenceHeight + 2) return false;
		}
	}

	return true;
}

void PathRiverGenerator::GenerateRivers()
{
	int amount = _settings_game.game_creation.amount_of_rivers;
	if (amount == 0) return;

	uint wells = ScaleByMapSize(4 << _settings_game.game_creation.amount_of_rivers);
	SetGeneratingWorldProgress(GWP_RIVER, wells + 256 / 64); // Include the tile loop calls below.

	for (; wells != 0; wells--) {
		IncreaseGeneratingWorldProgress(GWP_RIVER);
		for (int tries = 0; tries < 128; tries++) {
			TileIndex t = RandomTile();
			if (!CircularTileSearch(&t, 8, PathRiverGenerator::FindSpring, NULL)) continue;
			if (this->FlowRiver(t, t)) break;
		}
	}

	/* Run tile loop to update the ground density. */
	for (uint i = 0; i != 256; i++) {
		if (i % 64 == 0) IncreaseGeneratingWorldProgress(GWP_RIVER);
		RunTileLoop();
	}
}

/**
 * Check whether a river at begin could (logically) flow down to end.
 * @param begin The origin of the flow.
 * @param end The destination of the flow.
 * @return True iff the water can be flowing down.
 */
bool PathRiverGenerator::FlowsDown(TileIndex begin, TileIndex end)
{
	assert(DistanceManhattan(begin, end) == 1);

	int heightBegin;
	int heightEnd;
	Slope slopeBegin = GetTileSlope(begin, &heightBegin);
	Slope slopeEnd   = GetTileSlope(end, &heightEnd);

	return heightEnd <= heightBegin &&
			/* Slope either is inclined or flat; rivers don't support other slopes. */
			(slopeEnd == SLOPE_FLAT || IsInclinedSlope(slopeEnd)) &&
			/* Slope continues, then it must be lower... or either end must be flat. */
			((slopeEnd == slopeBegin && heightEnd < heightBegin) || slopeEnd == SLOPE_FLAT || slopeBegin == SLOPE_FLAT);
}

/**
 * Make a connected lake; fill all tiles in the circular tile search that are connected.
 * @param tile The tile to consider for lake making.
 * @param user_data The height of the lake.
 * @return Always false, so it continues searching.
 */
bool PathRiverGenerator::MakeLake(TileIndex tile, void *user_data)
{
	uint height = *(uint*)user_data;
	if (!IsValidTile(tile) || TileHeight(tile) != height || !IsTileFlat(tile)) return false;
	if (_settings_game.game_creation.landscape == LT_TROPIC && GetTropicZone(tile) == TROPICZONE_DESERT) return false;

	for (DiagDirection d = DIAGDIR_BEGIN; d < DIAGDIR_END; d++) {
		TileIndex t2 = tile + TileOffsByDiagDir(d);
		if (IsWaterTile(t2)) {
			MakeRiver(tile, Random());
			return false;
		}
	}

	return false;
}

/**
 * Try to flow the river down from a given begin.
 * @param spring The springing point of the river.
 * @param begin  The begin point we are looking from; somewhere down hill from the spring.
 * @return True iff a river could/has been built, otherwise false.
 */
bool PathRiverGenerator::FlowRiver(TileIndex spring, TileIndex begin)
{
	#define SET_MARK(x) marks.insert(x)
	#define IS_MARKED(x) (marks.find(x) != marks.end())

	uint height = TileHeight(begin);
	if (IsWaterTile(begin)) return DistanceManhattan(spring, begin) > _settings_game.game_creation.min_river_length;

	std::set<TileIndex> marks;
	SET_MARK(begin);

	/* Breadth first search for the closest tile we can flow down to. */
	std::list<TileIndex> queue;
	queue.push_back(begin);

	bool found = false;
	uint count = 0; // Number of tiles considered; to be used for lake location guessing.
	TileIndex end;
	do {
		end = queue.front();
		queue.pop_front();

		uint height2 = TileHeight(end);
		if (IsTileFlat(end) && (height2 < height || (height2 == height && IsWaterTile(end)))) {
			found = true;
			break;
		}

		for (DiagDirection d = DIAGDIR_BEGIN; d < DIAGDIR_END; d++) {
			TileIndex t2 = end + TileOffsByDiagDir(d);
			if (IsValidTile(t2) && !IS_MARKED(t2) && PathRiverGenerator::FlowsDown(end, t2)) {
				SET_MARK(t2);
				count++;
				queue.push_back(t2);
			}
		}
	} while (!queue.empty());

	if (found) {
		/* Flow further down hill. */
		found = this->FlowRiver(spring, end);
	} else if (count > 32) {
		/* Maybe we can make a lake. Find the Nth of the considered tiles. */
		TileIndex lakeCenter = 0;
		int i = RandomRange(count - 1) + 1;
		std::set<TileIndex>::const_iterator cit = marks.begin();
		while (--i) cit++;
		lakeCenter = *cit;

		if (IsValidTile(lakeCenter) &&
				/* A river, or lake, can only be built on flat slopes. */
				IsTileFlat(lakeCenter) &&
				/* We want the lake to be built at the height of the river. */
				TileHeight(begin) == TileHeight(lakeCenter) &&
				/* We don't want the lake at the entry of the valley. */
				lakeCenter != begin &&
				/* We don't want lakes in the desert. */
				(_settings_game.game_creation.landscape != LT_TROPIC || GetTropicZone(lakeCenter) != TROPICZONE_DESERT) &&
				/* We only want a lake if the river is long enough. */
				DistanceManhattan(spring, lakeCenter) > _settings_game.game_creation.min_river_length) {
			end = lakeCenter;
			MakeRiver(lakeCenter, Random());
			uint range = RandomRange(8) + 3;
			CircularTileSearch(&lakeCenter, range, PathRiverGenerator::MakeLake, &height);
			/* Call the search a second time so artefacts from going circular in one direction get (mostly) hidden. */
			lakeCenter = end;
			CircularTileSearch(&lakeCenter, range, PathRiverGenerator::MakeLake, &height);
			found = true;
		}
	}

	marks.clear();
	if (found) this->BuildRiver(begin, end);
	return found;
}

/* AyStar callback for checking whether we reached our destination. */
int32 PathRiverGenerator::River_EndNodeCheck(AyStar *aystar, OpenListNode *current)
{
	return current->path.node.tile == *(TileIndex*)aystar->user_target ? AYSTAR_FOUND_END_NODE : AYSTAR_DONE;
}

/* AyStar callback for getting the cost of the current node. */
int32 PathRiverGenerator::River_CalculateG(AyStar *aystar, AyStarNode *current, OpenListNode *parent)
{
	return 1 + RandomRange(_settings_game.game_creation.river_route_random);
}

/* AyStar callback for getting the estimated cost to the destination. */
int32 PathRiverGenerator::River_CalculateH(AyStar *aystar, AyStarNode *current, OpenListNode *parent)
{
	return DistanceManhattan(*(TileIndex*)aystar->user_target, current->tile);
}

/* AyStar callback for getting the neighbouring nodes of the given node. */
void PathRiverGenerator::River_GetNeighbours(AyStar *aystar, OpenListNode *current)
{
	TileIndex tile = current->path.node.tile;

	aystar->num_neighbours = 0;
	for (DiagDirection d = DIAGDIR_BEGIN; d < DIAGDIR_END; d++) {
		TileIndex t2 = tile + TileOffsByDiagDir(d);
		if (IsValidTile(t2) && PathRiverGenerator::FlowsDown(tile, t2)) {
			aystar->neighbours[aystar->num_neighbours].tile = t2;
			aystar->neighbours[aystar->num_neighbours].direction = INVALID_TRACKDIR;
			aystar->num_neighbours++;
		}
	}
}

/* AyStar callback when an route has been found. */
void PathRiverGenerator::River_FoundEndNode(AyStar *aystar, OpenListNode *current)
{
	for (PathNode *path = &current->path; path != NULL; path = path->parent) {
		TileIndex tile = path->node.tile;
		if (!IsWaterTile(tile)) {
			MakeRiver(tile, Random());
			/* Remove desert directly around the river tile. */
			CircularTileSearch(&tile, 5, RiverModifyDesertZone, NULL);
		}
	}
}

/**
 * Simple hash function for river tiles to be used by AyStar.
 * @param tile The tile to hash.
 * @param dir The unused direction.
 * @return The hash for the tile.
 */
uint PathRiverGenerator::River_Hash(uint tile, uint dir)
{
	return GB(TileHash(TileX(tile), TileY(tile)), 0, PathRiverGenerator::RIVER_HASH_SIZE);
}

/**
 * Actually build the river between the begin and end tiles using AyStar.
 * @param begin The begin of the river.
 * @param end The end of the river.
 */
void PathRiverGenerator::BuildRiver(TileIndex begin, TileIndex end)
{
	AyStar finder;
	MemSetT(&finder, 0);
	finder.CalculateG = PathRiverGenerator::River_CalculateG;
	finder.CalculateH = PathRiverGenerator::River_CalculateH;
	finder.GetNeighbours = PathRiverGenerator::River_GetNeighbours;
	finder.EndNodeCheck = PathRiverGenerator::River_EndNodeCheck;
	finder.FoundEndNode = PathRiverGenerator::River_FoundEndNode;
	finder.user_target = &end;

	finder.Init(PathRiverGenerator::River_Hash, 1 << RIVER_HASH_SIZE);

	AyStarNode start;
	start.tile = begin;
	start.direction = INVALID_TRACKDIR;
	finder.AddStartNode(&start, 0);
	finder.Main();
	finder.Free();
}
