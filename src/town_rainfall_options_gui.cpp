/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rainfall_option_gui.cpp Expert GUI for configuring the rainfall river generator */

#include "stdafx.h"
#include "string_func.h"
#include "window_func.h"
#include "window_gui.h"

#include "table/strings.h"

#include "safeguards.h"

/** In this window, expert options for town placement using the rainfall river generator can be configured.
 *  It can be opened both from the world generation, and from the heightmap generation dialog.
 */
struct TownRainfallOptionsWindow : Window {

	TownRainfallOptionsWindow(WindowDesc *desc, Window *parent, WindowNumber window_number = 0) : Window(desc)
	{
		this->parent = parent;
		this->InitNested(window_number);
	}
};

/** Widgets for the date setting window. */
static const NWidgetPart _nested_town_rainfall_option_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_BROWN),
		NWidget(WWT_CAPTION, COLOUR_BROWN), SetDataTip(STR_TP_RAINFALL_CAPTION, STR_NULL),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_BROWN),
	EndContainer(),
};

/** Description of the date setting window. */
static WindowDesc _town_rainfall_option_desc(
	WDP_CENTER, NULL, 0, 0,
	WC_TOWN_RAINFALL_OPTIONS, WC_NONE,
	0,
	_nested_town_rainfall_option_widgets, lengthof(_nested_town_rainfall_option_widgets)
);

void ShowTownRainfallOptionsWindow(Window *parent)
{
	DeleteWindowByClass(WC_TOWN_RAINFALL_OPTIONS);

	new TownRainfallOptionsWindow(&_town_rainfall_option_desc, parent);
}
