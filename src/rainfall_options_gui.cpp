/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rainfall_option_gui.cpp Expert GUI for configuring the rainfall river generator */

#include "stdafx.h"
#include "strings_func.h"
#include "string_func.h"
#include "window_func.h"
#include "window_gui.h"

#include "table/sprites.h"
#include "table/strings.h"

#include "core/geometry_func.hpp"
#include "widgets/dropdown_type.h"
#include "widgets/dropdown_func.h"
#include "widgets/genworld_widget.h"

#include "safeguards.h"

/** Options for the dropdown controlling, wether wider rivers or valleys should be generated.
 */
static const StringID _widen_rivers_options[]   = {STR_RAINFALL_RIVER_EXPANSION_OFF, STR_RAINFALL_RIVER_EXPANSION_ON, INVALID_STRING_ID};
static const StringID _widen_valleys_options[]   = {STR_RAINFALL_WIDER_VALLEYS_OFF, STR_RAINFALL_WIDER_VALLEYS_ON, INVALID_STRING_ID};

/** In this window, expert options for the rainfall river generator can be configured.
 *  It can be opened both from the world generation, and from the heightmap generation dialog.
 */
struct RainfallOptionWindow : Window {

	RainfallOptionWindow(WindowDesc *desc, Window *parent, WindowNumber window_number = 0) : Window(desc)
	{
		this->parent = parent;
		this->InitNested(window_number);
	}

	virtual void SetStringParameters(int widget) const
	{
		switch (widget) {
			case WID_RFO_FLOW_FOR_RIVER_TEXT:              SetDParam(0, 0); break;
			case WID_RFO_LAKE_VOLUME_TEXT:                 SetDParam(0, 0); break;
			case WID_RFO_FLOW_MODIFICATIONS_TEXT:          SetDParam(0, 0); break;
			case WID_RFO_WIDER_RIVERS_DROPDOWN:            SetDParam(0, _widen_rivers_options[0]); break;
			case WID_RFO_WIDER_RIVERS_MULT_TEXT:           SetDParam(0, 0); break;
			case WID_RFO_WIDER_VALLEYS_DROPDOWN:           SetDParam(0, _widen_valleys_options[0]); break;
			case WID_RFO_WIDER_VALLEYS_MULT_TEXT:          SetDParam(0, 0); break;
			case WID_RFO_WIDER_VALLEYS_RANDOM_TEXT:        SetDParam(0, 0); break;

			case WID_RFO_OUTFLOW_CANYON_PROBABILITY_TEXT:  SetDParam(0, 0); break;
			case WID_RFO_MINIMIZE_LAKE_PROBABILITY_TEXT:   SetDParam(0, 0); break;
			case WID_RFO_ISLAND_PROBABILITY_TEXT:          SetDParam(0, 0); break;
			case WID_RFO_SHORE_PROBABILITY_TEXT:           SetDParam(0, 0); break;

			case WID_RFO_ISLAND_MAX_SIZE_TEXT:             SetDParam(0, 0); break;
			case WID_RFO_SHORE_MAX_SIZE_TEXT:              SetDParam(0, 0); break;
		}
	}

	virtual void UpdateWidgetSize(int widget, Dimension *size, const Dimension &padding, Dimension *fill, Dimension *resize)
	{
		switch (widget) {
			case WID_RFO_FLOW_FOR_RIVER_TEXT:
			case WID_RFO_LAKE_VOLUME_TEXT:
				SetDParam(0, 1000000);
				*size = GetStringBoundingBox(STR_JUST_INT);
				break;

			case WID_RFO_WIDER_RIVERS_DROPDOWN:
				*size = maxdim(GetStringBoundingBox(STR_RAINFALL_RIVER_EXPANSION_ON), GetStringBoundingBox(STR_RAINFALL_RIVER_EXPANSION_OFF));
			    size->width += padding.width;
				size->height += padding.height;
				break;

			case WID_RFO_WIDER_VALLEYS_DROPDOWN:
				*size = maxdim(GetStringBoundingBox(STR_RAINFALL_WIDER_VALLEYS_ON), GetStringBoundingBox(STR_RAINFALL_WIDER_VALLEYS_OFF));
			    size->width += padding.width;
				size->height += padding.height;
				break;

			case WID_RFO_WIDER_RIVERS_MULT_TEXT:
			case WID_RFO_WIDER_VALLEYS_MULT_TEXT:
			case WID_RFO_WIDER_VALLEYS_RANDOM_TEXT:
			case WID_RFO_FLOW_MODIFICATIONS_TEXT:
			case WID_RFO_OUTFLOW_CANYON_PROBABILITY_TEXT:
			case WID_RFO_MINIMIZE_LAKE_PROBABILITY_TEXT:
			case WID_RFO_ISLAND_PROBABILITY_TEXT:
			case WID_RFO_SHORE_PROBABILITY_TEXT:
				SetDParam(0, 1000);
				*size = GetStringBoundingBox(STR_JUST_INT);
				break;

			case WID_RFO_ISLAND_MAX_SIZE_TEXT:
			case WID_RFO_SHORE_MAX_SIZE_TEXT:
				SetDParam(0, 10000);
				*size = GetStringBoundingBox(STR_JUST_INT);
				break;
		}
	}
};

/** Widgets for the date setting window. */
static const NWidgetPart _nested_rainfall_option_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_BROWN),
		NWidget(WWT_CAPTION, COLOUR_BROWN), SetDataTip(STR_RAINFALL_CAPTION, STR_NULL),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_BROWN),
		/* Two blocks of settings on top of each other */
		NWidget(NWID_VERTICAL), SetPIP(0, 10, 4),
			NWidget(NWID_HORIZONTAL),
				/* The upper block has two columns: For basic settings and for river related settings. */
				NWidget(NWID_HORIZONTAL),
					/* The left column is for basic settings */
					NWidget(NWID_VERTICAL), SetPIP(0, 4, 0),
						NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_BASIC_SETTINGS_CAPTION, STR_RAINFALL_BASIC_SETTINGS_TOOLTIP), SetFill(1, 1),
						NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_FLOW_FOR_RIVER_LABEL, STR_RAINFALL_FLOW_FOR_RIVER_TOOLTIP), SetFill(1, 1),
						NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_FLOW_PER_LAKE_VOLUME_LABEL, STR_RAINFALL_FLOW_PER_LAKE_VOLUME_TOOLTIP), SetFill(1, 1),
						NWidget(NWID_SPACER), SetFill(0, 1), SetMinimalSize(0, 45),
					EndContainer(),
					NWidget(NWID_VERTICAL), SetPIP(0, 4, 0),
						NWidget(NWID_SPACER), SetFill(0, 1), SetMinimalSize(0, 25),
						/* Defining flow necessary for forming a river */
						NWidget(NWID_HORIZONTAL), SetPIP(8, 0, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_FLOW_FOR_RIVER_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_FLOW_FOR_RIVER_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_FLOW_FOR_RIVER_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_FLOW_FOR_RIVER_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_FLOW_FOR_RIVER_TOOLTIP), SetFill(0, 1),
						EndContainer(),
						/* How many flow is consumed per lake volume? */
						NWidget(NWID_HORIZONTAL), SetPIP(8, 0, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_LAKE_VOLUME_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_FLOW_PER_LAKE_VOLUME_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_LAKE_VOLUME_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_LAKE_VOLUME_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_FLOW_PER_LAKE_VOLUME_TOOLTIP), SetFill(0, 1),
						EndContainer(),
						NWidget(NWID_SPACER), SetFill(0, 1), SetMinimalSize(0, 45),
					EndContainer(),
				EndContainer(),
				NWidget(NWID_HORIZONTAL), SetPIP(20, 0, 0),
					/* The right column is for river related settings */
					NWidget(NWID_VERTICAL), SetPIP(0, 4, 0),
						NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_RIVER_SETTINGS_CAPTION, STR_RAINFALL_RIVER_SETTINGS_TOOLTIP), SetFill(1, 1),
						/* How often should the flow modificator run? */
						NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_FLOW_MODIFICATION_LABEL, STR_RAINFALL_FLOW_MODIFICATION_TOOLTIP), SetFill(1, 1),
						/* Optionally make rivers wider */
						NWidget(NWID_HORIZONTAL),
							NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_RIVER_EXPANSION_ENABLED_LABEL, STR_RAINFALL_RIVER_EXPANSION_TOOLTIP), SetFill(1, 1),
							NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 20),
								NWidget(WWT_DROPDOWN, COLOUR_ORANGE, WID_RFO_WIDER_RIVERS_DROPDOWN), SetDataTip(STR_JUST_STRING, STR_NULL), SetFill(1, 0),
							EndContainer(),
							NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_RIVER_EXPANSION_MULTIPLIER_LABEL, STR_RAINFALL_RIVER_EXPANSION_TOOLTIP), SetFill(1, 1),
						EndContainer(),
						/* Optionally make valleys wider */
						NWidget(NWID_HORIZONTAL),
							NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_WIDER_VALLEYS_ENABLED_LABEL, STR_RAINFALL_WIDER_VALLEYS_TOOLTIP), SetFill(1, 1),
							NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 20),
								NWidget(WWT_DROPDOWN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_DROPDOWN), SetDataTip(STR_JUST_STRING, STR_NULL), SetFill(1, 0),
							EndContainer(),
							NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_WIDER_VALLEYS_MULTIPLIER_LABEL, STR_RAINFALL_WIDER_VALLEYS_TOOLTIP), SetFill(1, 1),
						EndContainer(),
						NWidget(NWID_HORIZONTAL),
							NWidget(NWID_SPACER), SetFill(1, 0), SetResize(1, 0),
							NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_WIDER_VALLEYS_RANDOMNESS, STR_RAINFALL_WIDER_VALLEYS_RANDOMNESS_TOOLTIP), SetFill(1, 1), SetPIP(0, 0, 20),
						EndContainer(),
					EndContainer(),
					NWidget(NWID_VERTICAL), SetPIP(0, 4, 0),
						NWidget(NWID_SPACER), SetFill(0, 1), SetMinimalSize(0, 25),
						/* How often should the flow modificator run? */
						NWidget(NWID_HORIZONTAL), SetPIP(4, 0, 4),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_FLOW_MODIFICATIONS_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_FLOW_MODIFICATION_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_FLOW_MODIFICATIONS_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_FLOW_MODIFICATIONS_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_FLOW_MODIFICATION_TOOLTIP), SetFill(0, 1),
						EndContainer(),
						/* Optionally make rivers wider */
						NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 4),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_RIVERS_MULT_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_RIVER_EXPANSION_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_WIDER_RIVERS_MULT_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_RIVERS_MULT_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_RIVER_EXPANSION_TOOLTIP), SetFill(0, 1),
						EndContainer(),
						/* Optionally make valleys wider */
						NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 4),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_MULT_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_WIDER_VALLEYS_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_MULT_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_MULT_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_WIDER_VALLEYS_TOOLTIP), SetFill(0, 1),
						EndContainer(),
						/* With a defined randomness */
						NWidget(NWID_HORIZONTAL), SetPIP(0, 0, 4),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_RANDOM_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_WIDER_VALLEYS_RANDOMNESS_TOOLTIP), SetFill(0, 1),
							NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_RANDOM_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
							NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_WIDER_VALLEYS_RANDOM_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_WIDER_VALLEYS_RANDOMNESS_TOOLTIP), SetFill(0, 1),
						EndContainer(),
					EndContainer(),
				EndContainer(),
			EndContainer(),
			/* The lower block has just one column of settings, start with the caption */
			NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_SETTINGS_CAPTION, STR_MAPGEN_MAPSIZE_TOOLTIP), SetFill(1, 1),
			NWidget(NWID_HORIZONTAL),
				/* Column "Probability for..." */
				NWidget(NWID_VERTICAL, NC_EQUALSIZE), SetPIP(0, 4, 0),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_OUTFLOW_CANYON_PROBABILITY_LABEL, STR_RAINFALL_LAKE_OUTFLOW_CANYON_PROBABILITY_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_MINIMIZE_PROBABILITY_LABEL, STR_RAINFALL_LAKE_MINIMIZE_PROBABILITY_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_ISLAND_PROBABILITY_LABEL, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_PROBABILITY_LABEL, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(1, 1),
				EndContainer(),
				/* Column probability widgets */
				NWidget(NWID_VERTICAL, NC_EQUALSIZE), SetPIP(0, 4, 0),
					/* Probability for an outflow canyon */
					NWidget(NWID_HORIZONTAL), SetPIP(4, 0, 8),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_OUTFLOW_CANYON_PROBABILITY_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_OUTFLOW_CANYON_PROBABILITY_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_OUTFLOW_CANYON_PROBABILITY_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_OUTFLOW_CANYON_PROBABILITY_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_OUTFLOW_CANYON_PROBABILITY_TOOLTIP), SetFill(0, 1),
					EndContainer(),
					/* Probability for minimizing a lake */
					NWidget(NWID_HORIZONTAL), SetPIP(4, 0, 8),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_MINIMIZE_LAKE_PROBABILITY_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_MINIMIZE_PROBABILITY_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_MINIMIZE_LAKE_PROBABILITY_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_MINIMIZE_LAKE_PROBABILITY_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_MINIMIZE_PROBABILITY_TOOLTIP), SetFill(0, 1),
					EndContainer(),
					/* Probability for starting an island at a lake tile */
					NWidget(NWID_HORIZONTAL), SetPIP(4, 0, 8),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_ISLAND_PROBABILITY_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_ISLAND_PROBABILITY_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_ISLAND_PROBABILITY_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(0, 1),
					EndContainer(),
					/* Probability for expanding a shore starting at a lake shore tile */
					NWidget(NWID_HORIZONTAL), SetPIP(4, 0, 8),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_SHORE_PROBABILITY_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_SHORE_PROBABILITY_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_SHORE_PROBABILITY_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(0, 1),
					EndContainer(),
				EndContainer(),
				/* Column "/1000" */
				NWidget(NWID_VERTICAL, NC_EQUALSIZE), SetPIP(0, 4, 0),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_PROBABILITY_DIVISOR, STR_RAINFALL_LAKE_OUTFLOW_CANYON_PROBABILITY_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_PROBABILITY_DIVISOR, STR_RAINFALL_LAKE_MINIMIZE_PROBABILITY_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_PROBABILITY_DIVISOR, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_PROBABILITY_DIVISOR, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(1, 1),
				EndContainer(),
				/* Column "Maximum size" */
				NWidget(NWID_VERTICAL, NC_EQUALSIZE), SetPIP(0, 4, 0),
					NWidget(NWID_SPACER), SetFill(0, 1),
					NWidget(NWID_SPACER), SetFill(0, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_MAXIMUM_SIZE_LABEL, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(1, 1),
					NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_RAINFALL_LAKE_MAXIMUM_SIZE_LABEL, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(1, 1),
				EndContainer(),
				/* Column max size widgets */
				NWidget(NWID_VERTICAL, NC_EQUALSIZE), SetPIP(0, 4, 0),
					NWidget(NWID_SPACER), SetFill(0, 1),
					NWidget(NWID_SPACER), SetFill(0, 1),
					/* Maximum size of island */
					NWidget(NWID_HORIZONTAL), SetPIP(8, 0, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_ISLAND_MAX_SIZE_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_ISLAND_MAX_SIZE_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_ISLAND_MAX_SIZE_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_ISLAND_TOOLTIP), SetFill(0, 1),
					EndContainer(),
					/* Maximum size of shore expansion */
					NWidget(NWID_HORIZONTAL), SetPIP(8, 0, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_SHORE_MAX_SIZE_DOWN), SetDataTip(SPR_ARROW_DOWN, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(0, 1),
						NWidget(WWT_TEXTBTN, COLOUR_ORANGE, WID_RFO_SHORE_MAX_SIZE_TEXT), SetDataTip(STR_BLACK_INT, STR_NULL), SetFill(1, 0),
						NWidget(WWT_IMGBTN, COLOUR_ORANGE, WID_RFO_SHORE_MAX_SIZE_UP), SetDataTip(SPR_ARROW_UP, STR_RAINFALL_LAKE_MORE_LAND_AT_SHORE_TOOLTIP), SetFill(0, 1),
					EndContainer(),
				EndContainer(),
			EndContainer(),
		EndContainer(),
	EndContainer(),
};

/** Description of the date setting window. */
static WindowDesc _rainfall_option_desc(
	WDP_CENTER, NULL, 0, 0,
	WC_RAINFALL_OPTIONS, WC_NONE,
	0,
	_nested_rainfall_option_widgets, lengthof(_nested_rainfall_option_widgets)
);

void ShowRainfallOptionsWindow(Window *parent)
{
	DeleteWindowByClass(WC_RAINFALL_OPTIONS);

	new RainfallOptionWindow(&_rainfall_option_desc, parent);
}
