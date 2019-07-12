/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file terraform_cmd.cpp Commands related to terraforming. */

#include "stdafx.h"
#include "command_func.h"
#include "tunnel_map.h"
#include "bridge_map.h"
#include "viewport_func.h"
#include "genworld.h"
#include "object_base.h"
#include "company_base.h"
#include "company_func.h"

#include "table/strings.h"
#include "terraform_func.h"
#include "tile_map.h"

#include "debug.h"

#include <map>
#include <set>

#include "safeguards.h"

TileIndex _terraform_err_tile; ///< first tile we couldn't terraform

Slope TerraformerState::GetPlannedSlope(TileIndex tile)
{
	assert(tile < MapSize());

	if (!IsInnerTile(tile)) {
		return SLOPE_FLAT;
	} else {
		TileIndex north_tile = tile;
		TileIndex west_tile = tile + TileDiffXY(1, 0);
		TileIndex east_tile = tile + TileDiffXY(0, 1);
		TileIndex south_tile = tile + TileDiffXY(1, 1);
		int hnorth = this->tile_to_new_height.find(north_tile) != this->tile_to_new_height.end() ? this->tile_to_new_height[north_tile] : TileHeight(north_tile);
		int hwest  = this->tile_to_new_height.find(west_tile)  != this->tile_to_new_height.end() ? this->tile_to_new_height[west_tile]  : TileHeight(west_tile);
		int heast  = this->tile_to_new_height.find(east_tile)  != this->tile_to_new_height.end() ? this->tile_to_new_height[east_tile]  : TileHeight(east_tile);
		int hsouth = this->tile_to_new_height.find(south_tile) != this->tile_to_new_height.end() ? this->tile_to_new_height[south_tile] : TileHeight(south_tile);
		return GetTileSlopeGivenHeight(hnorth, hwest, heast, hsouth, NULL);
	}
}

/**
 * Gets the TileHeight (height of north corner) of a tile as of current terraforming progress.
 *
 * @param ts TerraformerState.
 * @param tile Tile.
 * @return TileHeight.
 */
static int TerraformGetHeightOfTile(const TerraformerState *ts, TileIndex tile)
{
	TileIndexToHeightMap::const_iterator it = ts->tile_to_new_height.find(tile);
	return it != ts->tile_to_new_height.end() ? it->second : TileHeight(tile);
}

/**
 * Stores the TileHeight (height of north corner) of a tile in a TerraformerState.
 *
 * @param ts TerraformerState.
 * @param tile Tile.
 * @param height New TileHeight.
 */
static void TerraformSetHeightOfTile(TerraformerState *ts, TileIndex tile, int height)
{
	ts->tile_to_new_height[tile] = height;
}

/**
 * Adds a tile to the "tile_table" in a TerraformerState.
 *
 * @param ts TerraformerState.
 * @param tile Tile.
 * @ingroup dirty
 */
static void TerraformAddDirtyTile(TerraformerState *ts, TileIndex tile)
{
	ts->dirty_tiles.insert(tile);
}

/**
 * Adds all tiles that incident with the north corner of a specific tile to the "tile_table" in a TerraformerState.
 *
 * @param ts TerraformerState.
 * @param tile Tile.
 * @ingroup dirty
 */
static void TerraformAddDirtyTileAround(TerraformerState *ts, TileIndex tile)
{
	/* Make sure all tiles passed to TerraformAddDirtyTile are within [0, MapSize()] */
	if (TileY(tile) >= 1) TerraformAddDirtyTile(ts, tile + TileDiffXY( 0, -1));
	if (TileY(tile) >= 1 && TileX(tile) >= 1) TerraformAddDirtyTile(ts, tile + TileDiffXY(-1, -1));
	if (TileX(tile) >= 1) TerraformAddDirtyTile(ts, tile + TileDiffXY(-1,  0));
	TerraformAddDirtyTile(ts, tile);
}

/**
 * Terraform the north corner of a tile to a specific height.
 *
 * @param ts TerraformerState.
 * @param tile Tile.
 * @param height Aimed height.
 * @return Error code or cost.
 */
static CommandCost TerraformTileHeight(TerraformerState *ts, TileIndex tile, int height, bool error_if_no_effect = true)
{
	assert(tile < MapSize());

	/* Check range of destination height */
	if (height < 0) return_cmd_error(STR_ERROR_ALREADY_AT_SEA_LEVEL);
	if (height > _settings_game.construction.max_heightlevel) return_cmd_error(STR_ERROR_TOO_HIGH);

	/*
	 * Check if the terraforming has any effect.
	 * This can only be true, if multiple corners of the start-tile are terraformed (i.e. the terraforming is done by towns/industries etc.).
	 * In this case the terraforming should fail. (Don't know why.)
	 */
	if (error_if_no_effect && height == TerraformGetHeightOfTile(ts, tile)) return CMD_ERROR;

	/* Check "too close to edge of map". Only possible when freeform-edges is off. */
	uint x = TileX(tile);
	uint y = TileY(tile);
	if (!_settings_game.construction.freeform_edges && ((x <= 1) || (y <= 1) || (x >= MapMaxX() - 1) || (y >= MapMaxY() - 1))) {
		/*
		 * Determine a sensible error tile
		 */
		if (x == 1) x = 0;
		if (y == 1) y = 0;
		_terraform_err_tile = TileXY(x, y);
		return_cmd_error(STR_ERROR_TOO_CLOSE_TO_EDGE_OF_MAP);
	}

	/* Mark incident tiles that are involved in the terraforming. */
	TerraformAddDirtyTileAround(ts, tile);

	/* Store the height modification */
	TerraformSetHeightOfTile(ts, tile, height);

	CommandCost total_cost(EXPENSES_CONSTRUCTION);

	/* Increment cost */
	total_cost.AddCost(_price[PR_TERRAFORM]);

	/* Recurse to neighboured corners if height difference is larger than 1 */
	{
		const TileIndexDiffC *ttm;

		TileIndex orig_tile = tile;
		static const TileIndexDiffC _terraform_tilepos[] = {
			{ 1,  0}, // move to tile in SE
			{-2,  0}, // undo last move, and move to tile in NW
			{ 1,  1}, // undo last move, and move to tile in SW
			{ 0, -2}  // undo last move, and move to tile in NE
		};

		for (ttm = _terraform_tilepos; ttm != endof(_terraform_tilepos); ttm++) {
			tile += ToTileIndexDiff(*ttm);

			if (tile >= MapSize()) continue;
			/* Make sure we don't wrap around the map */
			if (Delta(TileX(orig_tile), TileX(tile)) == MapSizeX() - 1) continue;
			if (Delta(TileY(orig_tile), TileY(tile)) == MapSizeY() - 1) continue;

			/* Get TileHeight of neighboured tile as of current terraform progress */
			int r = TerraformGetHeightOfTile(ts, tile);
			int height_diff = height - r;

			/* Is the height difference to the neighboured corner greater than 1? */
			if (abs(height_diff) > 1) {
				/* Terraform the neighboured corner. The resulting height difference should be 1. */
				height_diff += (height_diff < 0 ? 1 : -1);
				CommandCost cost = TerraformTileHeight(ts, tile, r + height_diff);
				if (cost.Failed()) return cost;
				total_cost.AddCost(cost);
			}
		}
	}

	return total_cost;
}

/**  Performs various checks related to tunnels and bridges necessary during terraforming.
 *   @param ts the terraformer state
 *   @param total_cost cost so far
 *   @param direction direction from CmdTerraformLand, plus the possibility to pass 0 for "unknown".
 *   @param flags DoCommandFlags
 */
CommandCost ProcessTunnelsBridgesObjectsForTerraforming(TerraformerState &ts, CommandCost total_cost, int direction = 0, DoCommandFlag flags = DC_NONE)
{
	/* Check if the terraforming is valid wrt. tunnels, bridges and objects on the surface
	 * Pass == 0: Collect tileareas which are caused to be auto-cleared.
	 * Pass == 1: Collect the actual cost. */
	for (int pass = 0; pass < 2; pass++) {
		for (TileIndexSet::const_iterator it = ts.dirty_tiles.begin(); it != ts.dirty_tiles.end(); it++) {
			TileIndex tile = *it;

			assert(tile < MapSize());
			/* MP_VOID tiles can be terraformed but as tunnels and bridges
			 * cannot go under / over these tiles they don't need checking. */
			if (IsTileType(tile, MP_VOID)) continue;

			/* Find new heights of tile corners */
			int z_N = TerraformGetHeightOfTile(&ts, tile + TileDiffXY(0, 0));
			int z_W = TerraformGetHeightOfTile(&ts, tile + TileDiffXY(1, 0));
			int z_S = TerraformGetHeightOfTile(&ts, tile + TileDiffXY(1, 1));
			int z_E = TerraformGetHeightOfTile(&ts, tile + TileDiffXY(0, 1));

			/* Find min and max height of tile */
			int z_min = min(min(z_N, z_W), min(z_S, z_E));
			int z_max = max(max(z_N, z_W), max(z_S, z_E));

			/* Compute tile slope */
			Slope tileh = (z_max > z_min + 1 ? SLOPE_STEEP : SLOPE_FLAT);
			if (z_W > z_min) tileh |= SLOPE_W;
			if (z_S > z_min) tileh |= SLOPE_S;
			if (z_E > z_min) tileh |= SLOPE_E;
			if (z_N > z_min) tileh |= SLOPE_N;

			if (pass == 0) {
				/* Check if bridge would take damage */
				if (IsBridgeAbove(tile)) {
					int bridge_height = GetBridgeHeight(GetSouthernBridgeEnd(tile));

					/* Check if bridge would take damage. */
					if (direction >= 0 && bridge_height <= z_max) {
						_terraform_err_tile = tile; // highlight the tile under the bridge
						return_cmd_error(STR_ERROR_MUST_DEMOLISH_BRIDGE_FIRST);
					}

					/* Is the bridge above not too high afterwards? */
					if (direction <= 0 && bridge_height > (z_min + _settings_game.construction.max_bridge_height)) {
						_terraform_err_tile = tile;
						return_cmd_error(STR_ERROR_BRIDGE_TOO_HIGH_AFTER_LOWER_LAND);
					}
				}
				/* Check if tunnel would take damage */
				if (direction <= 0 && IsTunnelInWay(tile, z_min)) {
					_terraform_err_tile = tile; // highlight the tile above the tunnel
					return_cmd_error(STR_ERROR_EXCAVATION_WOULD_DAMAGE);
				}
			}

			/* Is the tile already cleared? */
			const ClearedObjectArea *coa = FindClearedObject(tile);
			bool indirectly_cleared = coa != NULL && coa->first_tile != tile;

			/* Check tiletype-specific things, and add extra-cost */
			const bool curr_gen = _generating_world;
			if (_game_mode == GM_EDITOR) _generating_world = true; // used to create green terraformed land
			DoCommandFlag tile_flags = flags | DC_AUTO | DC_FORCE_CLEAR_TILE;
			if (pass == 0) {
				tile_flags &= ~DC_EXEC;
				tile_flags |= DC_NO_MODIFY_TOWN_RATING;
			}
			CommandCost cost;
			if (indirectly_cleared) {
				cost = DoCommand(tile, 0, 0, tile_flags, CMD_LANDSCAPE_CLEAR);
			} else {
				cost = _tile_type_procs[GetTileType(tile)]->terraform_tile_proc(tile, tile_flags, z_min, tileh);
			}
			_generating_world = curr_gen;
			if (cost.Failed()) {
				_terraform_err_tile = tile;
				return cost;
			}
			if (pass == 1) total_cost.AddCost(cost);
		}
	}
	return total_cost;
}

/** Executes the terraforming as planned and recorded in the given terraformer_state.
 *  @param terraformer_state terraformer state
 */
void ExecuteTerraforming(TerraformerState &terraformer_state, bool clear_in_execute)
{
	for (TileIndexToHeightMap::const_iterator it = terraformer_state.tile_to_new_height.begin(); it != terraformer_state.tile_to_new_height.end(); it++) {
		TileIndex tile = it->first;
		int height = it->second;

		SetTileHeight(tile, (uint)height);
	}

	if (clear_in_execute) {
		for (TileIndexSet::const_iterator it = terraformer_state.dirty_tiles.begin(); it != terraformer_state.dirty_tiles.end(); it++) {
			TileIndex tile = *it;

			// TODO: Extract code to helper function, as it is copied from ProcessTunnelsBridgesObjectsForTerraforming

			if (IsTileType(tile, MP_VOID)) continue;

			/* Find new heights of tile corners */
			int z_N = terraformer_state.GetHeightOfTile(tile + TileDiffXY(0, 0));
			int z_W = terraformer_state.GetHeightOfTile(tile + TileDiffXY(1, 0));
			int z_S = terraformer_state.GetHeightOfTile(tile + TileDiffXY(1, 1));
			int z_E = terraformer_state.GetHeightOfTile(tile + TileDiffXY(0, 1));
			/* Find min and max height of tile */
			int z_min = min(min(z_N, z_W), min(z_S, z_E));
			int z_max = max(max(z_N, z_W), max(z_S, z_E));

			/* Compute tile slope */
			Slope tileh = (z_max > z_min + 1 ? SLOPE_STEEP : SLOPE_FLAT);
			if (z_W > z_min) tileh |= SLOPE_W;
			if (z_S > z_min) tileh |= SLOPE_S;
			if (z_E > z_min) tileh |= SLOPE_E;
			if (z_N > z_min) tileh |= SLOPE_N;

			/* Is the tile already cleared? */
			const ClearedObjectArea *coa = FindClearedObject(tile);
			bool indirectly_cleared = coa != NULL && coa->first_tile != tile;

			if (indirectly_cleared) {
				DoCommand(tile, 0, 0, DC_EXEC, CMD_LANDSCAPE_CLEAR);
			} else {
				_tile_type_procs[GetTileType(tile)]->terraform_tile_proc(tile, DC_EXEC, z_min, tileh);
			}
		}
	}
}

/**
 * Terraform land
 * @param tile tile to terraform
 * @param flags for this command type
 * @param p1 corners to terraform (SLOPE_xxx)
 * @param p2 direction; eg up (non-zero) or down (zero)
 * @param text unused
 * @return the cost of this operation or an error
 */
CommandCost CmdTerraformLand(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	_terraform_err_tile = INVALID_TILE;

	CommandCost total_cost(EXPENSES_CONSTRUCTION);
	int direction = (p2 != 0 ? 1 : -1);
	TerraformerState ts;

	/* Compute the costs and the terraforming result in a model of the landscape */
	if ((p1 & SLOPE_W) != 0 && tile + TileDiffXY(1, 0) < MapSize()) {
		TileIndex t = tile + TileDiffXY(1, 0);
		CommandCost cost = TerraformTileHeight(&ts, t, TileHeight(t) + direction);
		if (cost.Failed()) return cost;
		total_cost.AddCost(cost);
	}

	if ((p1 & SLOPE_S) != 0 && tile + TileDiffXY(1, 1) < MapSize()) {
		TileIndex t = tile + TileDiffXY(1, 1);
		CommandCost cost = TerraformTileHeight(&ts, t, TileHeight(t) + direction);
		if (cost.Failed()) return cost;
		total_cost.AddCost(cost);
	}

	if ((p1 & SLOPE_E) != 0 && tile + TileDiffXY(0, 1) < MapSize()) {
		TileIndex t = tile + TileDiffXY(0, 1);
		CommandCost cost = TerraformTileHeight(&ts, t, TileHeight(t) + direction);
		if (cost.Failed()) return cost;
		total_cost.AddCost(cost);
	}

	if ((p1 & SLOPE_N) != 0) {
		TileIndex t = tile + TileDiffXY(0, 0);
		CommandCost cost = TerraformTileHeight(&ts, t, TileHeight(t) + direction);
		if (cost.Failed()) return cost;
		total_cost.AddCost(cost);
	}

	total_cost = ProcessTunnelsBridgesObjectsForTerraforming(ts, total_cost, direction, flags);
	if (total_cost.Failed()) {
		return total_cost;
	}

	Company *c = Company::GetIfValid(_current_company);
	if (c != NULL && GB(c->terraform_limit, 16, 16) < ts.tile_to_new_height.size()) {
		return_cmd_error(STR_ERROR_TERRAFORM_LIMIT_REACHED);
	}

	if (flags & DC_EXEC) {
		/* change the height */
		ExecuteTerraforming(ts, false);

		/* Finally mark the dirty tiles dirty */
		for (TileIndexSet::const_iterator it = ts.dirty_tiles.begin(); it != ts.dirty_tiles.end(); it++) {
			MarkTileDirtyByTile(*it);

			int height = TerraformGetHeightOfTile(&ts, *it);

			/* Now, if we alter the height of the map edge, we need to take care
			 * about repainting the affected areas outside map as well.
			 * Remember:
			 * Outside map, we assume that our landscape descends to
			 * height zero as fast as possible.
			 * Those simulated tiles (they don't exist as datastructure,
			 * only as concept in code) need to be repainted properly,
			 * otherwise we will get ugly glitches.
			 *
			 * Furthermore, note that we have to take care about the possibility,
			 * that landscape was higher before the change,
			 * so also tiles a bit outside need to be repainted.
			 */
			int x = TileX(*it);
			int y = TileY(*it);
			if (x == 0) {
				if (y == 0) {
					/* Height of the northern corner is altered. */
					for (int cx = 0; cx >= -height - 1; cx--) {
						for (int cy = 0; cy >= -height - 1; cy--) {
							/* This means, tiles in the sector north of that
							 * corner need to be repainted.
							 */
							if (cx + cy >= -height - 2) {
								/* But only tiles that actually might have changed. */
								MarkTileDirtyByTileOutsideMap(cx, cy);
							}
						}
					}
				} else if (y < (int)MapMaxY()) {
					for (int cx = 0; cx >= -height - 1; cx--) {
						MarkTileDirtyByTileOutsideMap(cx, y);
					}
				} else {
					for (int cx = 0; cx >= -height - 1; cx--) {
						for (int cy = (int)MapMaxY(); cy <= (int)MapMaxY() + height + 1; cy++) {
							if (cx + ((int)MapMaxY() - cy) >= -height - 2) {
								MarkTileDirtyByTileOutsideMap(cx, cy);
							}
						}
					}
				}
			} else if (x < (int)MapMaxX()) {
				if (y == 0) {
					for (int cy = 0; cy >= -height - 1; cy--) {
						MarkTileDirtyByTileOutsideMap(x, cy);
					}
				} else if (y < (int)MapMaxY()) {
					/* Nothing to be done here, we are inside the map. */
				} else {
					for (int cy = (int)MapMaxY(); cy <= (int)MapMaxY() + height + 1; cy++) {
						MarkTileDirtyByTileOutsideMap(x, cy);
					}
				}
			} else {
				if (y == 0) {
					for (int cx = (int)MapMaxX(); cx <= (int)MapMaxX() + height + 1; cx++) {
						for (int cy = 0; cy >= -height - 1; cy--) {
							if (((int)MapMaxX() - cx) + cy >= -height - 2) {
								MarkTileDirtyByTileOutsideMap(cx, cy);
							}
						}
					}
				} else if (y < (int)MapMaxY()) {
					for (int cx = (int)MapMaxX(); cx <= (int)MapMaxX() + height + 1; cx++) {
						MarkTileDirtyByTileOutsideMap(cx, y);
					}
				} else {
					for (int cx = (int)MapMaxX(); cx <= (int)MapMaxX() + height + 1; cx++) {
						for (int cy = (int)MapMaxY(); cy <= (int)MapMaxY() + height + 1; cy++) {
							if (((int)MapMaxX() - cx) + ((int)MapMaxY() - cy) >= -height - 2) {
								MarkTileDirtyByTileOutsideMap(cx, cy);
							}
						}
					}
				}
			}
		}

		if (c != NULL) c->terraform_limit -= (uint32)ts.tile_to_new_height.size() << 16;
	}
	return total_cost;
}


/** Function meant to be called during map generation (AND ONLY THERE!).
 *  Works like TerraformTileToSlope, except that it doesn´t yet execute the terraforming.  This leaves room for
 *  inspecting the terraformer state outside the function (e.g. for finding out which tiles were affected by
 *  the terraforming).
 *  @param tile some tile
 *  @param desired_north desired height of the northern corner
 *  @param desired_slope the desired slope of the tile
 *  @return wether the terraforming operation was successful.
 */
bool SimulateTerraformTileToSlope(TileIndex tile, int desired_north, Slope desired_slope, TerraformerState &terraformer_state)
{
	int desired_west, desired_east, desired_south;

	if (desired_slope == SLOPE_STEEP_N) {
		desired_west = desired_north - 1;
		desired_south = desired_north - 2;
		desired_east = desired_north - 1;
	} else if (desired_slope == SLOPE_STEEP_E) {
		desired_west = desired_north - 1;
		desired_south = desired_north;
		desired_east = desired_north + 1;
	} else if (desired_slope == SLOPE_STEEP_W) {
		desired_west = desired_north + 1;
		desired_south = desired_north;
		desired_east = desired_north - 1;
	} else if (desired_slope == SLOPE_STEEP_S) {
		desired_west = desired_north + 1;
		desired_south = desired_north + 2;
		desired_east = desired_north + 1;
	} else {
		int ascended_offset = (desired_slope & SLOPE_N) ? 0 : 1;
		int descended_offset = (desired_slope & SLOPE_N) ? -1 : 0;

		desired_west = desired_north + ((desired_slope & SLOPE_W) ? ascended_offset : descended_offset);
		desired_south = desired_north + ((desired_slope & SLOPE_S) ? ascended_offset : descended_offset);
		desired_east = desired_north + ((desired_slope & SLOPE_E) ? ascended_offset : descended_offset);
	}

	TileIndex west_tile = tile + TileDiffXY(1, 0);
	TileIndex east_tile = tile + TileDiffXY(0, 1);
	TileIndex south_tile = tile + TileDiffXY(1, 1);

	CommandCost cost = TerraformTileHeight(&terraformer_state, tile, desired_north, false);
	if (cost.Failed()) {
		DEBUG(map, 0, "Terraforming failed for (%i,%i) and desired_north %i", TileX(tile), TileY(tile), desired_north);
		return false;
	}
	cost = TerraformTileHeight(&terraformer_state, west_tile, desired_west, false);
	if (cost.Failed()) {
		DEBUG(map, 0, "Terraforming failed for (%i,%i) and desired_west %i", TileX(west_tile), TileY(west_tile), desired_west);
		return false;
	}
	cost = TerraformTileHeight(&terraformer_state, east_tile, desired_east, false);
	if (cost.Failed()) {
		DEBUG(map, 0, "Terraforming failed for (%i,%i) and desired_east %i", TileX(east_tile), TileY(east_tile), desired_east);
		return false;
	}
	cost = TerraformTileHeight(&terraformer_state, south_tile, desired_south, false);
	if (cost.Failed()) {
		DEBUG(map, 0, "Terraforming failed for (%i,%i) and desired_south %i", TileX(south_tile), TileY(south_tile), desired_south);
		return false;
	}

	CommandCost total_cost = ProcessTunnelsBridgesObjectsForTerraforming(terraformer_state, CommandCost(EXPENSES_CONSTRUCTION));
	if (total_cost.Failed()) {
		DEBUG(map, 0, "Terraforming failed for (%i,%i) due to bridges/tunnels", TileX(tile), TileY(tile));
		return false;
	}

	/* The company related check CmdTerraformLand also performs is not interesting for us, as this function is designed for the scenario editor. */

	return true;
}

/** Terraform function meant to be used during landscape generation (AND ONLY THERE!).
 *  Given some generator state where a clear picture exists, what height and slope
 *  some tile has to have, this function performs the necessary terraforming operations
 *  while maintaining a landscape with valid slopes.
 *  As this function is meant to be called during landscape generation, it just
 *  returns wether it was successful.  If not successful, some intermediate state
 *  (i.e. with completely valid slopes, but different from the desired state)
 *  might exist on map.  But as in the context of e.g. river generation a failing
 *  terraform operation is quite improbable to impossible anyway (no objects,
 *  cities, industries exist yet), we don´t care about that.
 *  @param tile some tile
 *  @param desired_north desired height of the northern corner
 *  @param desired_slope the desired slope of the tile
 *  @return wether the terraforming operation was successful.
 */
bool TerraformTileToSlope(TileIndex tile, int desired_height, Slope desired_slope)
{
	TerraformerState terraformer_state;
	bool result = SimulateTerraformTileToSlope(tile, desired_height, desired_slope, terraformer_state);
	if (result) {
		ExecuteTerraforming(terraformer_state, true);
		return true;
	} else {
		return false;
	}
}

/**
 * Levels a selected (rectangle) area of land
 * @param tile end tile of area-drag
 * @param flags for this command type
 * @param p1 start tile of area drag
 * @param p2 various bitstuffed data.
 *  bit      0: Whether to use the Orthogonal (0) or Diagonal (1) iterator.
 *  bits 1 - 2: Mode of leveling \c LevelMode.
 * @param text unused
 * @return the cost of this operation or an error
 */
CommandCost CmdLevelLand(TileIndex tile, DoCommandFlag flags, uint32 p1, uint32 p2, const char *text)
{
	if (p1 >= MapSize()) return CMD_ERROR;

	_terraform_err_tile = INVALID_TILE;

	/* remember level height */
	uint oldh = TileHeight(p1);

	/* compute new height */
	uint h = oldh;
	LevelMode lm = (LevelMode)GB(p2, 1, 2);
	switch (lm) {
		case LM_LEVEL: break;
		case LM_RAISE: h++; break;
		case LM_LOWER: h--; break;
		default: return CMD_ERROR;
	}

	/* Check range of destination height */
	if (h > _settings_game.construction.max_heightlevel) return_cmd_error((oldh == 0) ? STR_ERROR_ALREADY_AT_SEA_LEVEL : STR_ERROR_TOO_HIGH);

	Money money = GetAvailableMoneyForCommand();
	CommandCost cost(EXPENSES_CONSTRUCTION);
	CommandCost last_error(lm == LM_LEVEL ? STR_ERROR_ALREADY_LEVELLED : INVALID_STRING_ID);
	bool had_success = false;

	const Company *c = Company::GetIfValid(_current_company);
	int limit = (c == NULL ? INT32_MAX : GB(c->terraform_limit, 16, 16));
	if (limit == 0) return_cmd_error(STR_ERROR_TERRAFORM_LIMIT_REACHED);

	TileIterator *iter = HasBit(p2, 0) ? (TileIterator *)new DiagonalTileIterator(tile, p1) : new OrthogonalTileIterator(tile, p1);
	for (; *iter != INVALID_TILE; ++(*iter)) {
		TileIndex t = *iter;
		uint curh = TileHeight(t);
		while (curh != h) {
			CommandCost ret = DoCommand(t, SLOPE_N, (curh > h) ? 0 : 1, flags & ~DC_EXEC, CMD_TERRAFORM_LAND);
			if (ret.Failed()) {
				last_error = ret;

				/* Did we reach the limit? */
				if (ret.GetErrorMessage() == STR_ERROR_TERRAFORM_LIMIT_REACHED) limit = 0;
				break;
			}

			if (flags & DC_EXEC) {
				money -= ret.GetCost();
				if (money < 0) {
					_additional_cash_required = ret.GetCost();
					delete iter;
					return cost;
				}
				DoCommand(t, SLOPE_N, (curh > h) ? 0 : 1, flags, CMD_TERRAFORM_LAND);
			} else {
				/* When we're at the terraform limit we better bail (unneeded) testing as well.
				 * This will probably cause the terraforming cost to be underestimated, but only
				 * when it's near the terraforming limit. Even then, the estimation is
				 * completely off due to it basically counting terraforming double, so it being
				 * cut off earlier might even give a better estimate in some cases. */
				if (--limit <= 0) {
					had_success = true;
					break;
				}
			}

			cost.AddCost(ret);
			curh += (curh > h) ? -1 : 1;
			had_success = true;
		}

		if (limit <= 0) break;
	}

	delete iter;
	return had_success ? cost : last_error;
}
