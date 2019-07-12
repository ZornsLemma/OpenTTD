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

#include "widgets/genworld_widget.h"

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
	  /* Two phases on top of each other */
	  NWidget(NWID_VERTICAL), SetPIP(0, 10, 4),
		  NWidget(NWID_VERTICAL),
			  NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_TP_RAINFALL_PHASE_ONE_CAPTION, STR_TP_RAINFALL_PHASE_ONE_TOOLTIP), SetFill(1, 1),
			  NWidget(NWID_HORIZONTAL),
				  NWidget(WWT_MATRIX, COLOUR_GREY, WID_TROP_PHASE_ONE_MATRIX), SetMinimalSize(600, 200), SetFill(1, 0), SetResize(1, 14), SetScrollbar(WID_TROP_PHASE_ONE_SCROLLBAR),
				  NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_TROP_PHASE_ONE_SCROLLBAR),
			  EndContainer(),
			  NWidget(NWID_HORIZONTAL),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_ONE_ADD_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_ADD_BUTTON_CAPTION, STR_TP_RAINFALL_ADD_BUTTON_TOOLTIP),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_ONE_EDIT_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_EDIT_BUTTON_CAPTION, STR_TP_RAINFALL_EDIT_BUTTON_TOOLTIP),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_ONE_DELETE_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_DELETE_BUTTON_CAPTION, STR_TP_RAINFALL_DELETE_BUTTON_TOOLTIP),
			  EndContainer(),
		  EndContainer(),
		  NWidget(NWID_VERTICAL),
			  NWidget(WWT_TEXT, COLOUR_ORANGE), SetDataTip(STR_TP_RAINFALL_PHASE_TWO_CAPTION, STR_TP_RAINFALL_PHASE_TWO_TOOLTIP), SetFill(1, 1),
			  NWidget(NWID_HORIZONTAL),
//			NWidget(WWT_PANEL, COLOUR_GREY, WID_VT_TIMETABLE_PANEL), SetMinimalSize(388, 82), SetResize(1, 10), SetDataTip(STR_NULL, STR_TIMETABLE_TOOLTIP), SetScrollbar(WID_VT_SCROLLBAR), EndContainer(),
				  NWidget(WWT_MATRIX, COLOUR_GREY, WID_TROP_PHASE_TWO_MATRIX), SetMinimalSize(600, 200), SetResize(1, 14), SetFill(1, 0), SetScrollbar(WID_TROP_PHASE_TWO_SCROLLBAR),
				  NWidget(NWID_VSCROLLBAR, COLOUR_GREY, WID_TROP_PHASE_TWO_SCROLLBAR),
			  EndContainer(),
			  NWidget(NWID_HORIZONTAL),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_TWO_ADD_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_ADD_BUTTON_CAPTION, STR_TP_RAINFALL_ADD_BUTTON_TOOLTIP),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_TWO_EDIT_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_EDIT_BUTTON_CAPTION, STR_TP_RAINFALL_EDIT_BUTTON_TOOLTIP),
				  NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TROP_PHASE_TWO_DELETE_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TP_RAINFALL_DELETE_BUTTON_CAPTION, STR_TP_RAINFALL_DELETE_BUTTON_TOOLTIP),
			  EndContainer(),
		  EndContainer(),
	  EndContainer(),
	EndContainer(),
};

/** Description of window. */
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
