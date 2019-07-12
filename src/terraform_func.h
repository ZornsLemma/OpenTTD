/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file terraform_gui.h GUI stuff related to terraforming. */

#ifndef TERRAFORM_FUNC_H
#define TERRAFORM_FUNC_H

#include "slope_type.h"
#include "tile_type.h"

#include <map>
#include <set>

/** Set of tiles. */
typedef std::set<TileIndex> TileIndexSet;
/** Mapping of tiles to their height. */
typedef std::map<TileIndex, int> TileIndexToHeightMap;

/** State of the terraforming. */
struct TerraformerState {
	TileIndexSet dirty_tiles;                ///< The tiles that need to be redrawn.
	TileIndexToHeightMap tile_to_new_height; ///< The tiles for which the height has changed.

	Slope GetPlannedSlope(TileIndex tile);
};

bool SimulateTerraformTileToSlope(TileIndex tile, int desired_height, Slope desired_slope, TerraformerState &terraformer_state);
void ExecuteTerraforming(TerraformerState &terraformer_state, bool clear_in_execute = true);
bool TerraformTileToSlope(TileIndex tile, int desired_height, Slope desired_slope);

#endif /* TERRAFORM_FUNC_H */
