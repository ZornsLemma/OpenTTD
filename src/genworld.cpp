/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file genworld.cpp Functions to generate a map. */

#include "stdafx.h"
#include "landscape.h"
#include "company_func.h"
#include "genworld.h"
#include "gfxinit.h"
#include "window_func.h"
#include "network/network.h"
#include "heightmap.h"
#include "viewport_func.h"
#include "date_func.h"
#include "engine_func.h"
#include "water.h"
#include "video/video_driver.hpp"
#include "tilehighlight_func.h"
#include "saveload/saveload.h"
#include "void_map.h"
#include "town.h"
#include "newgrf.h"
#include "core/random_func.hpp"
#include "core/backup_type.hpp"
#include "progress.h"
#include "error.h"
#include "game/game.hpp"
#include "game/game_instance.hpp"
#include "string_func.h"
#include "thread.h"

#include "safeguards.h"


void GenerateClearTile();
void GenerateIndustries();
void GenerateObjects();
void GenerateTrees();

void StartupEconomy();
void StartupCompanies();
void StartupDisasters();

void InitializeGame(uint size_x, uint size_y, bool reset_date, bool reset_settings);

/**
 * Please only use this variable in genworld.h and genworld.cpp and
 *  nowhere else. For speed improvements we need it to be global, but
 *  in no way the meaning of it is to use it anywhere else besides
 *  in the genworld.h and genworld.cpp!
 */
GenWorldInfo _gw;

/** Whether we are generating the map or not. */
bool _generating_world;

/**
 * Tells if the world generation is done in a thread or not.
 * @return the 'threaded' status
 */
bool IsGenerateWorldThreaded()
{
	return _gw.threaded && !_gw.quit_thread;
}

/**
 * Clean up the 'mess' of generation. That is, show windows again, reset
 * thread variables, and delete the progress window.
 */
static void CleanupGeneration()
{
	_generating_world = false;

	SetMouseCursorBusy(false);
	/* Show all vital windows again, because we have hidden them */
	if (_gw.threaded && _game_mode != GM_MENU) ShowVitalWindows();
	SetModalProgress(false);
	_gw.proc     = nullptr;
	_gw.abortp   = nullptr;
	_gw.threaded = false;

	DeleteWindowByClass(WC_MODAL_PROGRESS);
	ShowFirstError();
	MarkWholeScreenDirty();
}

/**
 * The internal, real, generate function.
 */
static void _GenerateWorld()
{
	/* Make sure everything is done via OWNER_NONE. */
	Backup<CompanyID> _cur_company(_current_company, OWNER_NONE, FILE_LINE);

	std::unique_lock<std::mutex> lock(_modal_progress_work_mutex, std::defer_lock);
	try {
		_generating_world = true;
		lock.lock();
		if (_network_dedicated) DEBUG(net, 1, "Generating map, please wait...");
		/* Set the Random() seed to generation_seed so we produce the same map with the same seed */
		if (_settings_game.game_creation.generation_seed == GENERATE_NEW_SEED) _settings_game.game_creation.generation_seed = _settings_newgame.game_creation.generation_seed = InteractiveRandom();
		_random.SetSeed(_settings_game.game_creation.generation_seed);
		SetGeneratingWorldProgress(GWP_MAP_INIT, 2);
		SetObjectToPlace(SPR_CURSOR_ZZZ, PAL_NONE, HT_NONE, WC_MAIN_WINDOW, 0);

		BasePersistentStorageArray::SwitchMode(PSM_ENTER_GAMELOOP);

		IncreaseGeneratingWorldProgress(GWP_MAP_INIT);
		/* Must start economy early because of the costs. */
		StartupEconomy();

		/* Don't generate landscape items when in the scenario editor. */
		if (_gw.mode == GWM_EMPTY) {
			SetGeneratingWorldProgress(GWP_OBJECT, 1);

			/* Make sure the tiles at the north border are void tiles if needed. */
			if (_settings_game.construction.freeform_edges) {
				for (uint x = 0; x < MapSizeX(); x++) MakeVoid(TileXY(x, 0));
				for (uint y = 0; y < MapSizeY(); y++) MakeVoid(TileXY(0, y));
			}

			/* Make the map the height of the setting */
			if (_game_mode != GM_MENU) FlatEmptyWorld(_settings_game.game_creation.se_flat_world_height);

			ConvertGroundTilesIntoWaterTiles();
			IncreaseGeneratingWorldProgress(GWP_OBJECT);
		} else {
			bool towns_generated = GenerateLandscape(_gw.mode);
			GenerateClearTile();

			/* If the river generator did not yet generate the towns, check wether we should do so here. */
			if (!towns_generated) {
				/* Do it except explicitely forbidden (which should only be possible in scenario editor */
				if (_settings_newgame.game_creation.town_placer != TWP_NONE) {
					towns_generated = GenerateTowns(_settings_game.economy.town_layout);
				}
			}
			/* If outside scenario editor, and still no towns generated, we have to abort here, as maps without towns make no sense. */
			if (!towns_generated && _game_mode != GM_EDITOR) {
				_cur_company.Restore();
				HandleGeneratingWorldAbortion();
				return;
			}

			/* only generate tree and industries in newgame mode. */
			if (_game_mode != GM_EDITOR) {
				GenerateIndustries();
				GenerateObjects();
				GenerateTrees();
			}
		}

		/* These are probably pointless when inside the scenario editor. */
		SetGeneratingWorldProgress(GWP_GAME_INIT, 3);
		StartupCompanies();
		IncreaseGeneratingWorldProgress(GWP_GAME_INIT);
		StartupEngines();
		IncreaseGeneratingWorldProgress(GWP_GAME_INIT);
		StartupDisasters();
		_generating_world = false;

		/* No need to run the tile loop in the scenario editor. */
		if (_gw.mode != GWM_EMPTY) {
			uint i;

			SetGeneratingWorldProgress(GWP_RUNTILELOOP, 0x500);
			for (i = 0; i < 0x500; i++) {
				RunTileLoop();
				_tick_counter++;
				IncreaseGeneratingWorldProgress(GWP_RUNTILELOOP);
			}

			if (_game_mode != GM_EDITOR) {
				Game::StartNew();

				if (Game::GetInstance() != nullptr) {
					SetGeneratingWorldProgress(GWP_RUNSCRIPT, 2500);
					_generating_world = true;
					for (i = 0; i < 2500; i++) {
						Game::GameLoop();
						IncreaseGeneratingWorldProgress(GWP_RUNSCRIPT);
						if (Game::GetInstance()->IsSleeping()) break;
					}
					_generating_world = false;
				}
			}
		}

		BasePersistentStorageArray::SwitchMode(PSM_LEAVE_GAMELOOP);

		ResetObjectToPlace();
		_cur_company.Trash();
		_current_company = _local_company = _gw.lc;

		SetGeneratingWorldProgress(GWP_GAME_START, 1);
		/* Call any callback */
		if (_gw.proc != nullptr) _gw.proc();
		IncreaseGeneratingWorldProgress(GWP_GAME_START);

		CleanupGeneration();
		lock.unlock();

		ShowNewGRFError();

		if (_network_dedicated) DEBUG(net, 1, "Map generated, starting game");
		DEBUG(desync, 1, "new_map: %08x", _settings_game.game_creation.generation_seed);

		if (_debug_desync_level > 0) {
			char name[MAX_PATH];
			seprintf(name, lastof(name), "dmp_cmds_%08x_%08x.sav", _settings_game.game_creation.generation_seed, _date);
			SaveOrLoad(name, SLO_SAVE, DFT_GAME_FILE, AUTOSAVE_DIR, false);
		}
	} catch (...) {
		BasePersistentStorageArray::SwitchMode(PSM_LEAVE_GAMELOOP, true);
		if (_cur_company.IsValid()) _cur_company.Restore();
		_generating_world = false;
		throw;
	}
}

/**
 * Set here the function, if any, that you want to be called when landscape
 * generation is done.
 * @param proc callback procedure
 */
void GenerateWorldSetCallback(GWDoneProc *proc)
{
	_gw.proc = proc;
}

/**
 * Set here the function, if any, that you want to be called when landscape
 * generation is aborted.
 * @param proc callback procedure
 */
void GenerateWorldSetAbortCallback(GWAbortProc *proc)
{
	_gw.abortp = proc;
}

/**
 * This will wait for the thread to finish up his work. It will not continue
 * till the work is done.
 */
void WaitTillGeneratedWorld()
{
	if (!_gw.thread.joinable()) return;

	_modal_progress_work_mutex.unlock();
	_modal_progress_paint_mutex.unlock();
	_gw.quit_thread = true;
	_gw.thread.join();
	_gw.threaded = false;
	_modal_progress_work_mutex.lock();
	_modal_progress_paint_mutex.lock();
}

/**
 * Initializes the abortion process
 */
void AbortGeneratingWorld()
{
	_gw.abort = true;
}

/**
 * Is the generation being aborted?
 * @return the 'aborted' status
 */
bool IsGeneratingWorldAborted()
{
	return _gw.abort;
}

/**
 * Really handle the abortion, i.e. clean up some of the mess
 */
void HandleGeneratingWorldAbortion()
{
	/* Clean up - in SE create an empty map, otherwise, go to intro menu */
	_switch_mode = (_game_mode == GM_EDITOR) ? SM_EDITOR : SM_MENU;

	if (_gw.abortp != nullptr) _gw.abortp();

	CleanupGeneration();

	if (_gw.thread.joinable() && _gw.thread.get_id() == std::this_thread::get_id()) throw OTTDThreadExitSignal();

	SwitchToMode(_switch_mode);
}

/**
 * Generate a world.
 * @param mode The mode of world generation (see GenWorldMode).
 * @param size_x The X-size of the map.
 * @param size_y The Y-size of the map.
 * @param reset_settings Whether to reset the game configuration (used for restart)
 */
void GenerateWorld(GenWorldMode mode, uint size_x, uint size_y, bool reset_settings)
{
	if (HasModalProgress()) return;
	_gw.mode   = mode;
	_gw.size_x = size_x;
	_gw.size_y = size_y;
	SetModalProgress(true);
	_gw.abort  = false;
	_gw.abortp = nullptr;
	_gw.lc     = _local_company;
	_gw.quit_thread   = false;
	_gw.threaded      = true;

	/* This disables some commands and stuff */
	SetLocalCompany(COMPANY_SPECTATOR);

	InitializeGame(_gw.size_x, _gw.size_y, true, reset_settings);
	PrepareGenerateWorldProgress();

	/* Load the right landscape stuff, and the NewGRFs! */
	GfxLoadSprites();
	LoadStringWidthTable();

	/* Re-init the windowing system */
	ResetWindowSystem();

	/* Create toolbars */
	SetupColoursAndInitialWindow();
	SetObjectToPlace(SPR_CURSOR_ZZZ, PAL_NONE, HT_NONE, WC_MAIN_WINDOW, 0);

	if (_gw.thread.joinable()) _gw.thread.join();

	if (!UseThreadedModelProgress() || !VideoDriver::GetInstance()->HasGUI() || !StartNewThread(&_gw.thread, "ottd:genworld", &_GenerateWorld)) {
		DEBUG(misc, 1, "Cannot create genworld thread, reverting to single-threaded mode");
		_gw.threaded = false;
		_modal_progress_work_mutex.unlock();
		_GenerateWorld();
		_modal_progress_work_mutex.lock();
		return;
	}

	UnshowCriticalError();
	/* Remove any open window */
	DeleteAllNonVitalWindows();
	/* Hide vital windows, because we don't allow to use them */
	HideVitalWindows();

	/* Don't show the dialog if we don't have a thread */
	ShowGenerateWorldProgress();

	/* Centre the view on the map */
	if (FindWindowById(WC_MAIN_WINDOW, 0) != nullptr) {
		ScrollMainWindowToTile(TileXY(MapSizeX() / 2, MapSizeY() / 2), true);
	}
}

std::vector<TownPlacer*> GetAllTownPlacers()
{
	std::vector<TownPlacer*> town_placers = std::vector<TownPlacer*>();
	town_placers.push_back(new HugeRiverTownPlacer());
	town_placers.push_back(new SmallRiverTownPlacer());
	town_placers.push_back(new RiverBranchTownPlacer());
	town_placers.push_back(new LakeTownPlacer());
	town_placers.push_back(new CoastPlacer());
	town_placers.push_back(new FlatLandPlacer());
	town_placers.push_back(new MountainTownPlacer());
	town_placers.push_back(new ValleyTownPlacer());
	return town_placers;
}

/* ================================ Deserializing Town Placer Config ===============================*/

static size_t FindMatchingCloseBracket(std::string &s, char open_bracket, char close_bracket, int position)
{
	int level = 1;
	while (level > 0) {
		size_t open_position = s.find(open_bracket, position);
		size_t close_position = s.find(close_bracket, position);
		if (close_position == std::string::npos) {
			/* Not found */
			return std::string::npos;
		} else if (open_position != std::string::npos && open_position < close_position) {
			/* Another opening bracket before the next closing one, i.e. brackets are nested */
			level++;
			position = open_position + 1;
		} else if (level > 1) {
			/* Closing bracket, but we are at an inner level of nesting */
			level--;
			position = close_position + 1;
		} else {
			return close_position;
		}
	}
	return std::string::npos;
}

static size_t GetVariableValuePos(std::string &s, const char* name)
{
	size_t name_pos = s.find(name);
	if (name_pos == std::string::npos) {
		return std::string::npos;
	} else {
		size_t colon_pos = s.find(':', name_pos + 1);
		return colon_pos == std::string::npos ? std::string::npos : colon_pos + 1;
	}
}

static int GetPositiveIntegerValue(std::string &s, const char* name)
{
	size_t value_pos = GetVariableValuePos(s, name);
	if (value_pos == std::string::npos) {
		DEBUG(misc, 0, "Variable not found: %s in %s", name, s.c_str());
		return -1;
	} else {
		size_t bracket_pos = s.find("}", value_pos);
		size_t comma_pos = s.find(",", value_pos);
		size_t end_pos;
		if (bracket_pos == std::string::npos && comma_pos == std::string::npos) {
			return -1;
		} else if (bracket_pos != std::string::npos && comma_pos == std::string::npos) {
			end_pos = min(bracket_pos, comma_pos);
		} else if (bracket_pos != std::string::npos) {
			end_pos = bracket_pos;
		} else {
			end_pos = comma_pos;
		}

		std::string number_string = s.substr(value_pos, end_pos - value_pos);
		return atoi(number_string.c_str());
	}
}

static TownPlacerPhase GetTownPlacerPhase(std::string &phase_block)
{
	size_t phase_pos = phase_block.find("phase");
	size_t start_pos = phase_block.find("\"", phase_pos);
	size_t end_pos = phase_block.find("\"", start_pos + 1);
	if (start_pos == std::string::npos || end_pos == std::string::npos) {
		return TPP_INVALID_PHASE;
	}

	std::string phase = phase_block.substr(start_pos + 1, end_pos - start_pos - 1);
	if (phase.find("ONE_CITY") != std::string::npos) {
		return TPP_PHASE_ONE_CITY;
	} else if (phase.find("TWO_TOWN") != std::string::npos) {
		return TPP_PHASE_TWO_TOWN;
	} else {
		return TPP_INVALID_PHASE;
	}
}

static void AddDefaultTownPlacerConfig(TownPlacerKey key, int weight, std::vector<TownPlacer*>* town_placers, std::vector<TownPlacerConfig> &configs)
{
	TownPlacer *town_placer = NULL;
	for (uint n = 0; n < town_placers->size(); n++) {
		TownPlacer* curr_placer = town_placers->at(n);
		if (curr_placer->GetKey() == key) {
			town_placer = curr_placer;
			break;
		}
	}

	if (town_placer != NULL) {
		TownPlacerConfig config = TownPlacerConfig();
		config.town_placer = town_placer->GetKey();
		config.weight = weight;
		config.parameter_map = std::map<int, int>();

		std::map<int, TownPlacerParameter> parameters = town_placer->GetParameters();
		for (std::map<int, TownPlacerParameter>::const_iterator it2 = parameters.begin(); it2 != parameters.end(); it2++) {
			int parameter_index = it2->first;
			TownPlacerParameter parameter = it2->second;
			config.parameter_map[parameter_index] = parameter.GetDefaultValue();
		}
		configs.push_back(config);
	}
}

static void DeserializeTownPlacerPhase(std::string &s, std::vector<TownPlacer*>* town_placers, std::vector<TownPlacerConfig> &configs)
{
	size_t start = s.find("{");
	while (start != std::string::npos) {
		size_t end = FindMatchingCloseBracket(s, '{', '}', start + 1);
		if (end == std::string::npos) {
			DEBUG(misc, 0, "While deserializing town placer config for a phase: Did not find matching close bracket in %s", s.c_str());
			break;
		} else {
			// E.g. placer:1,weight:100,params:[{index:0,value:1},{index:1,value:300}]
			std::string placer_block = s.substr(start + 1, end - start - 1);
			DEBUG(misc, 9, "start = " PRINTF_SIZE ", end = " PRINTF_SIZE ", placer_block = %s", start, end, placer_block.c_str());

			int placer_key = GetPositiveIntegerValue(placer_block, "placer");
			int weight = GetPositiveIntegerValue(placer_block, "weight");
			std::map<int, int> parameter_values = std::map<int, int>();

			size_t params_pos = GetVariableValuePos(placer_block, "params");
			if (params_pos == std::string::npos) {
				DEBUG(misc, 0, "No params block: %s", placer_block.c_str());
			} else {
				size_t params_end_pos = FindMatchingCloseBracket(placer_block, '[', ']', params_pos + 1);
				std::string params_block = placer_block.substr(params_pos + 1, params_end_pos - params_pos - 1);
				DEBUG(misc, 9, "Found params block %s", params_block.c_str());
				// E.g. {index:0,value:1},{index:1,value:300}

				size_t param_start = params_block.find("{");
				while (param_start != std::string::npos) {
					size_t param_end = FindMatchingCloseBracket(params_block, '{', '}', param_start + 1);
					if (param_end == std::string::npos) {
						DEBUG(map, 0, "Did not find an endpos for param %s", params_block.c_str());
					} else {
						std::string one_param_block = params_block.substr(param_start, param_end - param_start + 1);
						int index = GetPositiveIntegerValue(one_param_block, "index");
						int value = GetPositiveIntegerValue(one_param_block, "value");
						parameter_values[index] = value;

						param_start = params_block.find("{", param_end + 1);
					}
				}

				TownPlacerConfig curr_config = TownPlacerConfig();
				TownPlacer *placer = NULL;
				for (uint n = 0; n < town_placers->size(); n++) {
					if (town_placers->at(n)->GetKey() == placer_key) {
						placer = town_placers->at(n);
					}
				}
				bool all_ok = true;
				if (placer != NULL) {
					curr_config.town_placer = placer->GetKey();
				} else {
					DEBUG(map, 0, "Town placer %i is unknown. Ignoring config section %s", placer_key, placer_block.c_str());
					all_ok = false;
				}
				if (weight > 0) {
					curr_config.weight = weight;
				} else {
					DEBUG(map, 0, "Weight %i is illegal.  Ignoring config section %s", weight, placer_block.c_str());
					all_ok = false;
				}

				std::map<int, TownPlacerParameter> parameters = placer->GetParameters();
				for (std::map<int, TownPlacerParameter>::const_iterator it2 = parameters.begin(); it2 != parameters.end(); it2++) {
					int parameter_index = it2->first;
					TownPlacerParameter parameter = it2->second;

					if (parameter_values.find(parameter_index) != parameter_values.end()) {
						int value = parameter_values[parameter_index];
						if (value < 0) {
							DEBUG(map, 0, "Parameter %i could not be parsed in placer config %s", parameter_index, placer_block.c_str());
							all_ok = false;
						} else if (value < parameter.GetMinValue() || value > parameter.GetMaxValue()) {
							DEBUG(map, 0, "Parameter %i, value %i, violates its bounds (%i,%i) in placer config %s", parameter_index, value,
										  parameter.GetMinValue(), parameter.GetMaxValue(), placer_block.c_str());
							all_ok = false;
						} else {
							curr_config.parameter_map[parameter_index] = value;
						}
					} else {
						DEBUG(map, 0, "Parameter %i not found for placer config %s", parameter_index, placer_block.c_str());
						all_ok = false;
					}
				}

				if (all_ok) {
					configs.push_back(curr_config);
				}
			}
		}
		start = s.find("{", end + 1);
	}
}

void AddDefaultConfigForPhaseOne(std::vector<TownPlacer*>* town_placers, std::vector<TownPlacerConfig> &configs)
{
	AddDefaultTownPlacerConfig(TPK_HUGE, 300, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_BRANCH, 150, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_SMALL, 50, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_LAKE, 250, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_VALLEY, 100, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_COAST, 150, town_placers, configs);
}

void AddDefaultConfigForPhaseTwo(std::vector<TownPlacer*>* town_placers, std::vector<TownPlacerConfig> &configs)
{
	AddDefaultTownPlacerConfig(TPK_HUGE, 250, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_BRANCH, 50, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_SMALL, 150, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_MOUNTAIN, 100, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_LAKE, 100, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_FLAT, 100, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_VALLEY, 100, town_placers, configs);
	AddDefaultTownPlacerConfig(TPK_COAST, 150, town_placers, configs);
}

/** Deserialize the town placer config, stored in variable _settings_newgame.game_creation.rainfall.town_placers.
 *  For each phase, the configured town placer configs will be returned.
 *  Moreover, the function guarantees, that for each phase, a config will be returned - if necessary, a default config will be used for this.
 */
std::map<TownPlacerPhase, std::vector<TownPlacerConfig> > DeserializeTownPlacerConfig(std::vector<TownPlacer*>* town_placers, bool &using_default_config)
{
	using_default_config = false;

	std::map<TownPlacerPhase, std::vector<TownPlacerConfig> > phase_to_configs = std::map<TownPlacerPhase, std::vector<TownPlacerConfig> >();

	bool phases_found[2] = { false, false };

	std::string s;
	if (_settings_newgame.game_creation.rainfall.town_placers == NULL) {
		s = "[]";
	} else {
		s = std::string(_settings_newgame.game_creation.rainfall.town_placers);
	}

	size_t phase_array_start = s.find('[');
	size_t start = s.find('{', phase_array_start + 1);
	while (start != std::string::npos) {
		size_t end = FindMatchingCloseBracket(s, '{', '}', start + 1);
		if (end == std::string::npos) {
			break;
		} else {
			DEBUG(misc, 9, "start " PRINTF_SIZE ", end " PRINTF_SIZE ", phase_block %s", start, end, s.c_str());
			std::string phase_block = s.substr(start + 1, end - start - 1);
			TownPlacerPhase phase = GetTownPlacerPhase(phase_block);
			if (phase == TPP_PHASE_ONE_CITY || phase == TPP_PHASE_TWO_TOWN) {
				if (phases_found[phase]) {
					DEBUG(misc, 0, "Deserializing town placer config found duplicate block for phase %i: %s", phase, phase_block.c_str());
				} else {
					size_t phase_pos = phase_block.find("phase");
					size_t comma_pos = phase_block.find(",", phase_pos + 1);
					size_t config_pos = phase_block.find("configs", comma_pos + 1);
					size_t opening_pos = phase_block.find("[", config_pos + 1);
					size_t close_pos = FindMatchingCloseBracket(phase_block, '[', ']', opening_pos + 1);
					DEBUG(misc, 9, "Called GetConfig: opening_pos " PRINTF_SIZE ", close_pos " PRINTF_SIZE ", string %s", opening_pos, close_pos, phase_block.c_str());

					if (opening_pos == std::string::npos || close_pos == std::string::npos) {
						DEBUG(misc, 0, "Deserializing town placer config did not find valid configs block for phase %i: %s", phase, phase_block.c_str());
					} else {
						std::string config_array = phase_block.substr(opening_pos + 1, close_pos - opening_pos - 1);
						phase_to_configs[phase] = std::vector<TownPlacerConfig>();
						DeserializeTownPlacerPhase(config_array, town_placers, phase_to_configs[phase]);
						phases_found[phase] = true;
					}
				}
			} else {
				DEBUG(misc, 0, "Deserializing town placer config found illegal phase block: %s", phase_block.c_str());
			}
			start = s.find('{', end + 1);
		}
	}

	/* If config is missing, instead use a default config for the corresponding phase. */
	if (phase_to_configs.find(TPP_PHASE_ONE_CITY) == phase_to_configs.end() || phase_to_configs[TPP_PHASE_ONE_CITY].size() == 0) {
 		phase_to_configs[TPP_PHASE_ONE_CITY] = std::vector<TownPlacerConfig>();

 		AddDefaultConfigForPhaseOne(town_placers, phase_to_configs[TPP_PHASE_ONE_CITY]);
		using_default_config = true;
	}
	if (phase_to_configs.find(TPP_PHASE_TWO_TOWN) == phase_to_configs.end() || phase_to_configs[TPP_PHASE_TWO_TOWN].size() == 0) {
 		phase_to_configs[TPP_PHASE_TWO_TOWN] = std::vector<TownPlacerConfig>();

 		AddDefaultConfigForPhaseTwo(town_placers, phase_to_configs[TPP_PHASE_TWO_TOWN]);
		using_default_config = true;
	}

	return phase_to_configs;
}
