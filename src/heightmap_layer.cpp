/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file heightmap_layer.cpp Base implementation of heightmap layers. */

#include <iostream> // SFTODO TEMP
#include "stdafx.h"
#include "heightmap_layer_base.h"
#include "error.h"
#include "fileio_func.h"
#include "ini_type.h"
#include "string_func.h"

#include "table/strings.h"

HeightmapLayer::~HeightmapLayer() {
	free(this->information);
}

/** Class for parsing the town layer in an extended heightmap. */
struct TownIniFile : IniLoadFile {
	bool error;

	TownIniFile() : error(false) {}

        virtual FILE *OpenFile(const char *filename, Subdirectory subdir, size_t *size) {
		return FioFOpenFile(filename, "rb", subdir, size);
	}

	virtual void ReportFileError(const char * const pre, const char * const buffer, const char * const post)
        {
		// EHTODO: Is there any way I can include pre/buffer/post in the error message?
		std::cout << "SFTODOERROR: " << pre << buffer << post << std::endl;
		error = true;
        }
};

/**
 * Construct a TownLayer object for use within an extended heightmap.
 * The basic properties are supplied as arguments (which the caller obtains from the top-level metadata.txt)
 * and the towns themselves are parsed from the supplied file.
 * @param width layer width
 * @param height layer height
 * @param default_radius default 'radius' value to use for towns which don't specify their own
 * @param file town file
 */
TownLayer::TownLayer(uint width, uint height, uint default_radius, const char *file)
: HeightmapLayer(HLT_TOWN, width, height), valid(false)
{
	TownIniFile ini;
	char *file2 = str_fmt("./%s", file);
	ini.LoadFromDisk(file2, HEIGHTMAP_DIR);
	free(file2);
	if (ini.error) {
		ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_PARSING_TOWN_FILE, INVALID_STRING_ID, WL_ERROR);
		return;
	}

	for (IniGroup *town_group = ini.group; town_group != nullptr; town_group = town_group->next) {
		IniItem *name = town_group->GetItem("name", false);
		if (name == nullptr) {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_MISSING_TOWN_NAME, INVALID_STRING_ID, WL_ERROR);
			return;
		}
		std::cout << "SFTODOA1 " << name->value << std::endl; // SFTODO TEMP

		IniItem *posx = town_group->GetItem("posx", false);
		if (posx == nullptr) {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_MISSING_POSX, INVALID_STRING_ID, WL_ERROR);
			return;
		}

		IniItem *posy = town_group->GetItem("posy", false);
		if (posy == nullptr) {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_MISSING_POSY, INVALID_STRING_ID, WL_ERROR);
			return;
		}

		IniItem *radius_item = town_group->GetItem("radius", false);
		uint radius = default_radius;
		if (radius_item != nullptr) {
			radius = atoi(radius_item->value); // SFTODO NO ERROR CHECKING!
		}

		IniItem *size_item = town_group->GetItem("size", false);
		if (size_item == nullptr) {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_MISSING_SIZE, INVALID_STRING_ID, WL_ERROR);
			return;
		}
		// SFTODO: CASE SENSITIVITY
		TownSize size;
		if (strcmp(size_item->value, "small") == 0) {
			size = TSZ_SMALL;
		} else if (strcmp(size_item->value, "medium") == 0) {
			size = TSZ_MEDIUM;
		} else if (strcmp(size_item->value, "large") == 0) {
			size = TSZ_LARGE;
		} else if (strcmp(size_item->value, "random") == 0) {
			size = TSZ_RANDOM;
		} else {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_INVALID_SIZE, INVALID_STRING_ID, WL_ERROR);
			return;
		}

		bool is_city = false;
		IniItem *city = town_group->GetItem("city", false);
		if (city != nullptr) {
			// SFTODO: CASE SENSITIVITY?
			if (strcmp(city->value, "false") == 0) {
				is_city = false;
			} else if (strcmp(city->value, "true") == 0) {
				is_city = true;
			} else {
				ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_INVALID_CITY, INVALID_STRING_ID, WL_ERROR);
				return;
			}
		}

		IniItem *layout_item = town_group->GetItem("layout", false);
		if (layout_item == nullptr) {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_MISSING_LAYOUT, INVALID_STRING_ID, WL_ERROR);
			return;
		}
		TownLayout layout;
		// SFTODO: CASE SENSITIVITY
		if (strcmp(layout_item->value, "original") == 0) {
			layout = TL_ORIGINAL;
		} else if (strcmp(layout_item->value, "better") == 0) {
			layout = TL_BETTER_ROADS;
		} else if (strcmp(layout_item->value, "2x2") == 0) {
			layout = TL_2X2_GRID;
		} else if (strcmp(layout_item->value, "3x3") == 0) {
			layout = TL_3X3_GRID;
		} else if (strcmp(layout_item->value, "random") == 0) {
			layout = TL_RANDOM;
		} else {
			ShowErrorMessage(STR_MAPGEN_HEIGHTMAP_ERROR_INVALID_LAYOUT, INVALID_STRING_ID, WL_ERROR);
			return;
		}

		// SFTODO: USE OF ATOI() MEANS NO ERROR CHECKING - EXCEPT THIS SUPER CRUDE BIT OF EXTRA
		assert(atoi(posx->value) < width);
		assert(atoi(posy->value) < height);
		this->towns.emplace_back(name->value, atoi(posx->value), atoi(posy->value), radius, size, is_city, layout);
	}

	this->valid = true; // SFTODO: MAKE SURE THIS IS LAST LINE OF CTOR!
}

TownLayer::~TownLayer()
{
}
