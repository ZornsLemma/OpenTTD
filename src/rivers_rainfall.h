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
