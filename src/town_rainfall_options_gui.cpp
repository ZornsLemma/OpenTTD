/* $Id$ */

/*
 * This file is part of OpenTTD.
 * OpenTTD is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 2.
 * OpenTTD is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with OpenTTD. If not, see <http://www.gnu.org/licenses/>.
 */

/** @file rainfall_option_gui.cpp Expert GUI for configuring the rainfall river generator */

#include "stdafx.h"
#include "gfx_func.h"
#include "querystring_gui.h"
#include "rivers_rainfall.h"
#include "string_func.h"
#include "strings_func.h"
#include "town.h"
#include "window_func.h"
#include "window_gui.h"

#include "table/strings.h"

#include "widgets/dropdown_func.h"
#include "widgets/dropdown_type.h"
#include "widgets/genworld_widget.h"

#include "safeguards.h"

typedef void TownPlacerEditCallback(Window *w, TownPlacerPhase phase, TownPlacerConfig config);
void ShowTownPlacerEditWindow(Window *parent, int window_number, TownPlacerEditCallback *callback, TownPlacerPhase phase, TownPlacerConfig config);

/** In this window, expert options for town placement using the rainfall river generator can be configured.
 *  It can be opened both from the world generation, and from the heightmap generation dialog.
 */
struct TownRainfallOptionsWindow : Window {

	TownRainfallOptionsWindow(WindowDesc *desc, Window *parent, WindowNumber window_number = 0) : Window(desc)
	{
		this->parent = parent;
		this->InitNested(window_number);
	}

	static void AddTownPlacerCallback(Window *w, TownPlacerPhase phase, TownPlacerConfig config)
	{
	}

	static void EditTownPlacerCallback(Window *w, TownPlacerPhase phase, TownPlacerConfig config)
	{
	}

	virtual void OnClick(Point pt, int widget, int click_count)
	{
		TownPlacerConfig config = TownPlacerConfig();
		switch (widget) {
			case WID_TROP_PHASE_ONE_ADD_BUTTON:
				ShowTownPlacerEditWindow(this, 0, TownRainfallOptionsWindow::AddTownPlacerCallback, TPP_PHASE_ONE_CITY, config);
				break;
			case WID_TROP_PHASE_ONE_EDIT_BUTTON:
				ShowTownPlacerEditWindow(this, 0, TownRainfallOptionsWindow::EditTownPlacerCallback, TPP_PHASE_ONE_CITY, config);
				break;
			case WID_TROP_PHASE_TWO_ADD_BUTTON:
				ShowTownPlacerEditWindow(this, 0, TownRainfallOptionsWindow::AddTownPlacerCallback, TPP_PHASE_TWO_TOWN, config);
				break;
			case WID_TROP_PHASE_TWO_EDIT_BUTTON:
				ShowTownPlacerEditWindow(this, 0, TownRainfallOptionsWindow::EditTownPlacerCallback, TPP_PHASE_TWO_TOWN, config);
				break;
		}
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



/* ================================ Separate window for adding / editing configurations ===================================== */

struct PlacerWithParameter {
	TownPlacer *placer;
	int parameter;

	PlacerWithParameter() : placer(NULL) {}
	PlacerWithParameter(TownPlacer *placer, int parameter) : placer(placer), parameter(parameter) {};
};

static DropDownList *BuildTownPlacerDropDown(std::vector<TownPlacer*> placers)
{
	DropDownList *list = new DropDownList();

	for (uint z = 0; z < placers.size(); z++) {
		TownPlacer *placer = placers[z];
		DropDownListParamStringItem *item = new DropDownListParamStringItem(placer->GetName(), z, false);
		*list->Append() = item;
	}

	return list;
}

struct TownPlacerEditWindow : Window {
	TownPlacerEditCallback *callback;  ///< Callback to call once the user hits Ok
	TownPlacerPhase phase;
	TownPlacerConfig config;
	int town_placer_index;
	std::vector<TownPlacer*> town_placers;
	std::map<int, PlacerWithParameter> widget_id_to_placer_info;
	std::map<int, int> placer_index_to_first_widget_id;

	static const int MAX_EDITBOX_LENGTH = 10;

	TownPlacerEditWindow(WindowDesc *desc, WindowNumber window_number, Window *parent, TownPlacerEditCallback *callback, TownPlacerPhase phase, TownPlacerConfig config)
			  : Window(desc)
	{
		this->callback = callback;
		this->phase = phase;
		this->config = config;
		this->town_placer_index = -1;
		this->parent = parent;
		this->widget_id_to_placer_info = std::map<int, PlacerWithParameter>();
		this->placer_index_to_first_widget_id = std::map<int, int>();

		int curr_widget_index = WID_TPE_END;
		this->querystrings[WID_TPE_WEIGHT_TEXTBOX] = new QueryString(MAX_EDITBOX_LENGTH * MAX_CHAR_LENGTH, MAX_EDITBOX_LENGTH);
		this->querystrings[WID_TPE_WEIGHT_TEXTBOX]->text.afilter = CS_NUMERAL;

		if (this->config.weight >= 0) {
			SetDParam(0, this->config.weight);
			this->querystrings[WID_TPE_WEIGHT_TEXTBOX]->SetString(STR_JUST_INT);
		}

		/*  Fetch all town placers.  Due to C++ (in)capabilities (or at least, I didn´t found a better way), they come as
	     *  newly created objects, and this code is responsible to delete them once they aren´t needed any longer.  This
		 *  happens in the destructor below.
		 *
		 *  For the lifetime of the window, we set up a map <EditBox widget> to <Placer/parameter tuple>, which enables doing the right
		 *  things in the switch statements of the Window API methods.
		 *
		 *  Also, we need a QueryString object for each once, which we register under its widget id, and also create it dynamically
	     *  (and delete it in the destructor).
		 *
		 *  PLEASE NOTE: The widgets themself are created in a static function below, that has no access to our member variables.
	     *               Thus, we need to execute that TownPlacer / Parameter loop twice, once here, and the other time below in
		 *               that static function.  It is ESSENTIAL that both loops are structurally identical, i.e. loop over the
		 *               TownPlacers and parameters in the same order, and contribute the same widget ids.
		 */
		this->town_placers = GetAllTownPlacers();
		for (uint n = 0; n < this->town_placers.size(); n++) {
			TownPlacer *town_placer = this->town_placers[n];
			std::map<int, TownPlacerParameter> parameters = town_placer->GetParameters();
			for (std::map<int, TownPlacerParameter>::const_iterator it2 = parameters.begin(); it2 != parameters.end(); it2++) {
				int parameter_index = it2->first;
				TownPlacerParameter parameter = it2->second;
				this->widget_id_to_placer_info[curr_widget_index] = PlacerWithParameter(town_placer, parameter_index);
				this->querystrings[curr_widget_index] = new QueryString(MAX_EDITBOX_LENGTH * MAX_CHAR_LENGTH, MAX_EDITBOX_LENGTH);
				this->querystrings[curr_widget_index]->text.afilter = CS_NUMERAL;

				if (it2 == parameters.begin()) {
					placer_index_to_first_widget_id[n] = curr_widget_index;
				}

				if (this->config.parameter_map.find(parameter_index) != this->config.parameter_map.end()) {
					int desired_value = this->config.parameter_map[parameter_index]);

					if (desired_value >= parameter->GetMinValue() && desired_value <= parameter->GetMaxValue()) {
						SetDParam(0, desired_value);
						this->querystrings[curr_widget_index]->SetString(STR_JUST_INT);
					}
				}

				curr_widget_index++;
			}

			if (town_placer->GetKey() == this->config.town_placer) {
				this->town_placer_index = n;
			}
		}

		/* If the passed config contains no town_placer, select the first one (if it exists) */
		if (this->config.town_placer < 0 && this->town_placers.size() > 0) {
			this->town_placer_index = 0;
		}

		this->InitNested(window_number);
		this->OnTownPlacerSelected(false);
	}

	~TownPlacerEditWindow()
	{
		delete this->querystrings[WID_TPE_WEIGHT_TEXTBOX];
		for (std::map<int, PlacerWithParameter>::iterator it = this->widget_id_to_placer_info.begin(); it != this->widget_id_to_placer_info.end(); it++) {
			delete this->querystrings[it->first];
		}
		for (std::vector<TownPlacer*>::iterator it = this->town_placers.begin(); it != this->town_placers.end(); it++) {
			delete *it;
		}
	}


	virtual void OnClick(Point pt, int widget, int click_count)
	{
		switch (widget) {
			case WID_TPE_TOWN_PLACER_DROPDOWN: {
				int selected_index = this->town_placer_index < 0 ? 0 : this->town_placer_index;
				ShowDropDownList(this, BuildTownPlacerDropDown(this->town_placers), selected_index, WID_TPE_TOWN_PLACER_DROPDOWN);
				break;
			}
		}
	}

	void OnTownPlacerSelected(bool set_parameter_default_values)
	{
		if (this->town_placer_index >= 0) {
	  		NWidgetStacked *label_selection = this->GetWidget<NWidgetStacked>(WID_TPE_PARAM_LABEL_SELECTION);
	  		NWidgetStacked *widget_selection = this->GetWidget<NWidgetStacked>(WID_TPE_PARAM_WIDGET_SELECTION);
			label_selection->SetDisplayedPlane(this->town_placer_index);
			widget_selection->SetDisplayedPlane(this->town_placer_index);

			if (set_parameter_default_values) {
				TownPlacer *placer = this->town_placers[town_placer_index];
				int widget_offset = 0;
				std::map<int, TownPlacerParameter> parameters = placer->GetParameters();
				for (std::map<int, TownPlacerParameter>::const_iterator it2 = parameters.begin(); it2 != parameters.end(); it2++) {
					TownPlacerParameter parameter = it2->second;
					int querystring_index = placer_index_to_first_widget_id[this->town_placer_index] + widget_offset;
					SetDParam(0, parameter.GetDefaultValue());
					this->querystrings[querystring_index]->SetString(STR_JUST_INT);
					widget_offset++;
				}
			}
		}
	}

	virtual void OnDropdownSelect(int widget, int index)
	{
		switch (widget) {
			case WID_TPE_TOWN_PLACER_DROPDOWN:
				this->town_placer_index = index;
				this->OnTownPlacerSelected(true);

				this->SetDirty();
//				StringID description = this->town_placers[this->town_placer_index]->GetDescription();
//				this->GetWidget<NWidgetLeaf>(WID_TPE_DESC_CONTENT)->widget_data = description;
				break;
		}
	}

	virtual void SetStringParameters(int widget) const
	{
		switch (widget) {
			case WID_TPE_TOWN_PLACER_DROPDOWN:
				StringID town_placer_name;
				if (this->town_placer_index != -1) {
					town_placer_name = this->town_placers[this->town_placer_index]->GetName();
				} else {
					town_placer_name = STR_TOWN_PLACER_NONE_CHOSEN;
				}
				SetDParam(0, town_placer_name);
				break;
		}
	}

	virtual void DrawWidget(const Rect &r, int widget) const
	{
		switch (widget) {
			case WID_TPE_DESC_CONTENT:
//				GfxFillRect(r.left + 1, r.top + 1, r.right - 1, r.bottom - 1, PC_GREY);
				if (this->town_placer_index >= 0) {
					StringID description = this->town_placers[this->town_placer_index]->GetDescription();
					DrawStringMultiLine(r.left, r.right, r.top, r.bottom, description, TC_BLACK);
				}
				break;
		}
	}
};

static void AddVerticalSpacer(NWidgetVertical *column, int height)
{
	NWidgetSpacer* spacer = new NWidgetSpacer(0, 0);
	spacer->SetFill(1, 1);
	spacer->SetResize(1, 1);
	spacer->SetMinimalSize(0, height);
	column->Add(spacer);
}

static NWidgetBase *GetContentWidgets(int *biggest_index)
{
	NWidgetHorizontal *outer_container = new NWidgetHorizontal();
	NWidgetVertical *label_column = new NWidgetVertical();
	NWidgetVertical *widget_column = new NWidgetVertical();

	NWidgetLeaf* town_placer_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_TOWN_PLACER, STR_TPE_TOWN_PLACER_TOOLTIP);
	town_placer_label->SetFill(1, 1);
	label_column->Add(town_placer_label);
	NWidgetLeaf* town_placer_dropdown = new NWidgetLeaf(WWT_DROPDOWN, COLOUR_ORANGE, WID_TPE_TOWN_PLACER_DROPDOWN, STR_JUST_STRING, STR_TPE_TOWN_PLACER_TOOLTIP);
	town_placer_dropdown->SetMinimalSize(100, 12);
	widget_column->Add(town_placer_dropdown);
	AddVerticalSpacer(widget_column, 10);

	NWidgetLeaf* weight_label = new NWidgetLeaf(WWT_TEXT, COLOUR_ORANGE, -1, STR_TPE_WEIGHT, STR_TPE_WEIGHT_TOOLTIP);
	weight_label->SetFill(1, 1);
	label_column->Add(weight_label);
	AddVerticalSpacer(label_column, 5);
	NWidgetLeaf* weight_textbox = new NWidgetLeaf(WWT_EDITBOX, COLOUR_ORANGE, WID_TPE_WEIGHT_TEXTBOX, STR_NULL, STR_TPE_WEIGHT_TOOLTIP);
	weight_textbox->SetMinimalSize(100, 12);
	weight_textbox->SetFill(1, 0);
	weight_textbox->SetPadding(2,2,2,2);
	widget_column->Add(weight_textbox);
	AddVerticalSpacer(widget_column, 15);

	NWidgetLeaf* help_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP, STR_NULL);
	label_column->Add(help_label);
	AddVerticalSpacer(label_column, 4 * FONT_HEIGHT_NORMAL + 10 - 10);
	NWidgetLeaf* help_line_one_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP_LINE_ONE, STR_NULL);
	widget_column->Add(help_line_one_label);
	NWidgetLeaf* help_line_two_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP_LINE_TWO, STR_NULL);
	widget_column->Add(help_line_two_label);
	NWidgetLeaf* help_line_three_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP_LINE_THREE, STR_NULL);
	widget_column->Add(help_line_three_label);
	NWidgetLeaf* help_line_four_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP_LINE_FOUR, STR_NULL);
	widget_column->Add(help_line_four_label);
	NWidgetLeaf* help_line_five_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_HELP_LINE_FIVE, STR_NULL);
	widget_column->Add(help_line_five_label);
	AddVerticalSpacer(widget_column, 10);

	NWidgetLeaf* desc_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, STR_TPE_DESCRIPTION, STR_TPE_DESCRIPTION_TOOLTIP);
	label_column->Add(desc_label);
	AddVerticalSpacer(label_column, 170 - FONT_HEIGHT_NORMAL);
	NWidgetLeaf* desc_content = new NWidgetLeaf(WWT_EMPTY, COLOUR_GREY, WID_TPE_DESC_CONTENT, STR_NULL, STR_NULL);
	desc_content->SetMinimalSize(100, 190);
	desc_content->SetFill(1, 1);
	desc_content->SetResize(1, 1);
	widget_column->Add(desc_content);

	NWidgetStacked *param_label_selection = new NWidgetStacked();
	param_label_selection->SetIndex(WID_TPE_PARAM_LABEL_SELECTION);

	NWidgetStacked *param_widget_selection = new NWidgetStacked();
	param_widget_selection->SetIndex(WID_TPE_PARAM_WIDGET_SELECTION);

	int curr_widget_index = WID_TPE_END;

	std::vector<TownPlacer*> town_placers = GetAllTownPlacers();
	for (std::vector<TownPlacer*>::const_iterator it = town_placers.begin(); it != town_placers.end(); it++) {
		NWidgetVertical *param_label_container = new NWidgetVertical();
		NWidgetVertical *param_widget_container = new NWidgetVertical();

		TownPlacer *town_placer = *it;
		std::map<int, TownPlacerParameter> parameters = town_placer->GetParameters();
		for (std::map<int, TownPlacerParameter>::const_iterator it2 = parameters.begin(); it2 != parameters.end(); it2++) {
			TownPlacerParameter parameter = it2->second;

			NWidgetLeaf *param_label = new NWidgetLeaf(WWT_TEXT, COLOUR_GREY, -1, parameter.GetName(), parameter.GetTooltip());
			param_label->SetFill(1, 1);
			param_label_container->Add(param_label);
			AddVerticalSpacer(param_label_container, 0);

			NWidgetLeaf *param_textbox = new NWidgetLeaf(WWT_EDITBOX, COLOUR_GREY, curr_widget_index++, 0, parameter.GetTooltip());
			param_textbox->SetMinimalSize(100, 12);
			param_widget_container->Add(param_textbox);
		}
		delete town_placer;

		param_label_selection->Add(param_label_container);
		param_widget_selection->Add(param_widget_container);
	}

	label_column->Add(param_label_selection);
	widget_column->Add(param_widget_selection);

	outer_container->Add(label_column);
	outer_container->Add(widget_column);

	*biggest_index = curr_widget_index - 1;
	return outer_container;
}

static const NWidgetPart _nested_town_placer_edit_widgets[] = {
	NWidget(NWID_HORIZONTAL),
		NWidget(WWT_CLOSEBOX, COLOUR_BROWN),
		NWidget(WWT_CAPTION, COLOUR_BROWN, WID_TPE_CAPTION), SetDataTip(STR_TPE_CAPTION, STR_TOOLTIP_WINDOW_TITLE_DRAG_THIS),
	EndContainer(),
	NWidget(WWT_PANEL, COLOUR_BROWN),
		NWidget(NWID_VERTICAL),
			NWidgetFunction(GetContentWidgets),
			NWidget(NWID_SPACER), SetMinimalSize(0, 15),
			NWidget(NWID_HORIZONTAL),
				NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TPE_OK_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TPE_OK_BUTTON, STR_NULL),
				NWidget(WWT_PUSHTXTBTN, COLOUR_ORANGE, WID_TPE_ABORT_BUTTON), SetMinimalSize(81, 12), SetFill(0, 1), SetDataTip(STR_TPE_ABORT_BUTTON, STR_NULL),
				NWidget(NWID_SPACER), SetResize(1, 0),
			EndContainer(),
		EndContainer(),
	EndContainer()
};

static WindowDesc _town_placer_edit_desc(
	WDP_CENTER, NULL, 0, 0,
	WC_TOWN_PLACER_EDIT, WC_NONE,
	0,
	_nested_town_placer_edit_widgets, lengthof(_nested_town_placer_edit_widgets)
);

void ShowTownPlacerEditWindow(Window *parent, int window_number, TownPlacerEditCallback *callback, TownPlacerPhase phase, TownPlacerConfig config)
{
	DeleteWindowByClass(WC_TOWN_PLACER_EDIT);
	new TownPlacerEditWindow(&_town_placer_edit_desc, window_number, parent, callback, phase, config);
}
