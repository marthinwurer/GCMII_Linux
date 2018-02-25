/******************************************************************************
 *
 * 	mrweprefs.r
 *
 *
 *	Created on Thu Apr  8 20:33:43 2004 for modelF
 *	Copyright (c) 2004 All rights reserved.
 *	ABSOFT CORPORATION, Rochester Hills, Michigan 48309
 *	United States of America
 *
 ******************************************************************************/

#define DefaultFont      -1
#define DefaultFontSize  -1
#define DefaultLineSpace -1

#define NoPauseAtEnd  -1
#define PauseAtEnd     1

#define DefaultSaveOnClose -1
#define NeverSaveOnClose    1
#define PromptSaveOnClose   2
#define AlwaysSaveOnClose   3

#define WindowCloseBox   -1
#define NoWindowCloseBox  0

#define HasFontMenus   -1
#define NoFontMenus     0

#define DefaultTimerValue -1
#define DefaultCreator '????'
#define DefaultType    'TEXT'

#define WindowSizeNormal  -1
#define WindowSizeMaximize 2

type 'Mrwe' {
        longint	fontNum;
	longint fontSize;
	longint lineSpace;
	longint fontMenus;
	longint saveOC;
	longint pause;
	longint goAwayBox;
	longint timer;
	longint creator;
	longint fileType;
	PSTRING [255] save_name = "";
	PSTRING [27]  wind_pos = "";
	longint windowsize;
};

type 'STR ' {
	PSTRING tabsize = "8";
};


resource 'Mrwe' (128, "\0x00") {
	DefaultFont,
	9,
	DefaultLineSpace,
	NoFontMenus,
	AlwaysSaveOnClose,
	PauseAtEnd,
	WindowCloseBox,
	DefaultTimerValue,
	DefaultCreator,
	DefaultType,
	"",
	"",
	WindowSizeNormal
};

resource 'STR ' (131, "TABSIZE") {
	"8"
};
