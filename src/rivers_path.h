/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rivers_path.h The original river generator based on path-finding. */

#ifndef RIVERS_PATH_H
#define RIVERS_PATH_H

#include "genworld.h"
#include "pathfinder/npf/aystar.h"

/** The original, quite simple river generator that basically works by calculating paths between tiles.
 */
struct PathRiverGenerator : public RiverGenerator {

private:
	static bool FindSpring(TileIndex tile, void *user_data);
	static bool FlowsDown(TileIndex begin, TileIndex end);
	static bool MakeLake(TileIndex tile, void *user_data);

	static const uint RIVER_HASH_SIZE = 8; ///< The number of bits the hash for river finding should have.

	static int32 River_EndNodeCheck(AyStar *aystar, OpenListNode *current);
	static int32 River_CalculateG(AyStar *aystar, AyStarNode *current, OpenListNode *parent);
	static int32 River_CalculateH(AyStar *aystar, AyStarNode *current, OpenListNode *parent);
	static void River_GetNeighbours(AyStar *aystar, OpenListNode *current);
	static void River_FoundEndNode(AyStar *aystar, OpenListNode *current);
	static uint River_Hash(uint tile, uint dir);

	bool FlowRiver(TileIndex spring, TileIndex begin);
	void BuildRiver(TileIndex begin, TileIndex end);

public:
	PathRiverGenerator() {}
	virtual void GenerateRivers();


};

#endif /* RIVERS_PATH_H */
