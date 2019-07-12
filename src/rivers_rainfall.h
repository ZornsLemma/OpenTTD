/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rivers_path.h River calculation based on simulated rainfall. */

#ifndef RIVERS_RAINFALL_H
#define RIVERS_RAINFALL_H

#include "genworld.h"

/* The following constants define allowed ranges and default values for all
 * configuration options of the Rainfall River Generator.  The documentation
 * here focuses on why some particular bound was chosen, not on how those settings
 * work.
 *
 * The Few / Moderate / Lot of rivers settings only influences flow necessary for river and lakes,
 * and the wider rivers setting.  All other settings affect the look of rivers and lakes,
 * but not in the first place their quantity.
 */

/** Minimal flow necessary to form a river.  Values near to zero are dangerous and thus forbidden
 *  (as they would flood the whole landscape with water.
 */
static const int MIN_FLOW_FOR_RIVER = 10;
static const int DEF_FEW_RIVERS_FLOW_FOR_RIVER = 500;      ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Few Rivers" option.
static const int DEF_MODERATE_RIVERS_FLOW_FOR_RIVER = 150; ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Moderate Rivers" option.
static const int DEF_LOT_RIVERS_FLOW_FOR_RIVER = 50;       ///< The more flow is necessary for a river, the less rivers you will get.  Constant for the "Lot of Rivers" option.
static const int MAX_FLOW_FOR_RIVER = INT32_MAX;           ///< The more flow is necessary for a river, the less rivers you will get.  High values leading to hardly any rivers are a valid choice.

static const int MIN_FLOW_PER_LAKE_VOLUME = 0;                  ///< Minimum allowed flow needed per lake volume.  Zero means "Generate an outflow for any lake" and thus is a valid choice.
static const int DEF_FEW_RIVERS_FLOW_PER_LAKE_VOLUME = 200;     ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Few Rivers" options.
static const int DEF_MODERATE_RIVERS_FLOW_PER_LAKE_VOLUME = 50; ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Moderate Rivers" options.
static const int DEF_LOT_RIVERS_FLOW_PER_LAKE_VOLUME = 25;      ///< The more flow is necessary per lake volume, the smaller lakes will be.  Constant for the "Lot of Rivers" options.
static const int MAX_FLOW_PER_LAKE_VOLUME = INT32_MAX;          ///< The more flow is necessary per lake volume, the smaller lakes will be.  High values leading to minimum sized lakes are a valid choice.

static const int MIN_NUMBER_OF_FLOW_MODIFICATIONS = 0;          ///< Flow modifications are an optional step of river generation to make rivers look nicer.  Omitting them is a valid choice.
static const int DEF_NUMBER_OF_FLOW_MODIFICATIONS = 20;         ///< Flow modifications are an optional step of river generation to make rivers look nicer.
static const int MAX_NUMBER_OF_FLOW_MODIFICATIONS = INT16_MAX;  ///< More flow calculations lead to more calculation time, but there is no actual senseful bound.

static const int DEF_FEW_RIVERS_WIDER_RIVERS_ENABLED = 0;       ///< If few rivers should be calculated, they shouldn´t be wider also.
static const int DEF_MODERATE_RIVERS_WIDER_RIVERS_ENABLED = 1;  ///< Generate rivers with really much flow wider, if "Moderate Rivers" are chosen.
static const int DEF_LOT_RIVERS_WIDER_RIVERS_ENABLED = 1;       ///< Generate rivers with really much flow wider, if "Lot of Rivers" are chosen.

static const int MIN_WIDER_RIVERS_MULTIPLIER = 40;              ///< 1 would mean infinitely wide rivers, thus we need at least value two.
static const int DEF_WIDER_RIVERS_MULTIPLIER = 20;              ///< Value 10 means, that for minimum necessary river flow 200, rivers with more than 2000 (20000, 200000) flow are 2 (3, 4) tiles wide.
static const int MAX_WIDER_RIVERS_MULTIPLIER = INT16_MAX;       ///< If the bound is high, no wider rivers will be generated, but this is a valid choice.

static const int DEF_WIDER_VALLEYS_ENABLED = 0;                 ///< Regard this a as an expert setting a user can enable if appropriate for the generated map at hand
static const int MIN_WIDER_VALLEYS_MULTIPLIER = 0;              ///< Negative values make no sense, zero would essentially disable wider valleys
static const int DEF_WIDER_VALLEYS_MULTIPLIER = 5;              ///< Default multiplier on the river width.
static const int MAX_WIDER_VALLEYS_MULTIPLIER = 2048;           ///< Values more than e.g. 10 probably make no sense, but there is no actual bound

static const int MIN_WIDER_VALLEYS_RANDOMNESS = 0;              ///< Minimum value for wider river randomness
static const int DEF_WIDER_VALLEYS_RANDOMNESS = 1000;           ///< Default value for wider rivers randomness
static const int MAX_WIDER_VALLEYS_RANDOMNESS = 65535;          ///< Maximum value for wider river randomness

static const uint MIN_RAINFALL_PROBABILITY = 0;                 ///< Probabilities are scaled in the range 0 ... 1000.
static const uint MAX_RAINFALL_PROBABILITY = 1000;              ///< Probabilities are scaled in the range 0 ... 1000.

static const uint DEF_LAKE_OUTFLOW_CANYON_PROBABILITY = 0;      ///< By default, don´t lower lakes by adding an outflow canyon.
static const uint DEF_LAKE_REDUCE_TO_GUARANTEED_PROBABILITY = 0;///< By default, don´t reduce lakes to their minimum necessary tiles to stay connected
static const uint DEF_LAKE_ISLAND_PROBABILITY = 10;             ///< Default probability that an island is started at some lake tile.
static const uint DEF_LAKE_SHORE_PROBABILITY = 50;              ///< Default probability that shore is expanded starting at a lake shore tile.

static const uint MIN_RAINFALL_MAX_SIZE = 0;                    ///< Fan delta / island / shore sizes generated in a step are arbitrary.
static const uint MAX_RAINFALL_MAX_SIZE = INT16_MAX;            ///< Fan delta / island / shore sizes generated in a step are arbitrary.

static const uint DEF_LAKE_ISLAND_MAX_SIZE = 20;                ///< Default maximum size of an island (as generated in one step, multiple islands can merge with each other and with the shore)
static const uint DEF_LAKE_SHORE_MAX_SIZE = 5;                  ///< Default maximum number of shore tiles generated in one step.

/** A river generator, that generates rivers based on simulating rainfall on each tile
 *  (currently, each tile receives the same rainfall, but this is no must in terms of the algorithm),
 *  and based on this, simulates flow downwards the landscape.  Where enough flow is available,
 *  rivers and lakes will be generated.
 *
 *  Furthermore, the river generator can be configured with various options, resulting in quite different
 *  looking rivers and lakes.
 */
struct RainfallRiverGenerator : public RiverGenerator {

private:

public:
	RainfallRiverGenerator() {}
	virtual void GenerateRivers();
};

#endif /* RIVERS_RAINFALL_H */
