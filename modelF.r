/**********************************************************************
*
* 	modelF.r
*
*
*	Created on Thu Apr  8 20:33:43 2004
*	Copyright (c) 1996 - 2004 All rights reserved.
*	ABSOFT CORPORATION, Rochester Hills, Michigan 48309
*	United States of America
*
***********************************************************************
*
*   U.S. GOVERNMENT RESTRICTED RIGHTS. The software and
*   documentation are provided with RESTRICTED RIGHTS. Use,
*   duplication, or disclosure by the Government is subject
*   to restrictions set forth in subparagraph (c)(1)(ii) of
*   the Rights of Technical Data and Computer Software
*   clause at 252.227-7013. The contractor/manufacturer is
*   Absoft Corporation, 2781 Bond Street, Rochester Hills,
*   Michigan 48309.
*
***********************************************************************
*
*   PURPOSE OF FILE
*
*   Contains resource source code for the Macintosh Runtime Window
*   Environment library. 
*
*   Macintosh files have 2 parts: the resource fork & the data fork. The
*   data fork stores information such as text or pre-linked object code. The
*   resource fork holds all of a file's resources. Resources have types
*   related to what kind of information they hold. For example, 'CODE'
*   resources store the executable code of a program. Each code resource
*   is a different program segment. Also, 'DLOG' resources each describe
*   how a dialog box in a program will appear (where it will appear, its
*   size, which DITL resource contains the items for this dialog, etc.).
*   MRWE has 2 dialogs and therefore 2 'DLOG' resources. It has 6 different
*   alert boxes and therefore 6 'CNTL' resources.  Since the Dialog Item
*   List ('DITL') resource is used for both dialogs and alerts, there are
*   8 'DITL' resources.
*
*   This file is compiled by the resource compiler 'Rez'. By changing some
*   of the resource descriptions in this file, you may be able to
*   customize the MRWE environment. See the comments included with each
*   resource description for details.
*
***********************************************************************
*
*   RESOURCES
*
*   DLOG(128)       About Box
*   DLOG(129)       Printing in progress box
*   ALRT(139)       Save Prompts
*   ALRT(140)       Alerts with OK button
*   ALRT(141)       Alerts with OK button & long text
*   ALRT(142)       Alerts with Cancel button
*   ALRT(143)       Alerts with Continue button
*   ALRT(144)       Alerts with Quit button
*   DITL(128-144)   Dialog Item lists for above DLOGs & ALRTs
*   SIZE(-1)        Application Memory Requirements and Flags for MultiFinder
*
***********************************************************************



/* Creator ID (Application signature) */

#define APPSGN 'Mrwe'


/* Include files for definitions of standard resources */

#define SystemSevenOrLater 1
#include "Carbon.r"




resource 'DLOG' (129, "Printing in progress") {
    {116, 244, 164, 436},
    dBoxProc,
    invisible,
    noGoAway,
    0x0,
    129,
    "Now Printing",
    alertPositionMainScreen
};
resource 'DLOG' (128, "About Box", purgeable) {
    {48, 180, 120, 446},
    dBoxProc,
    invisible,
    noGoAway,
    0x0,
    128,
    "About",
    alertPositionMainScreen
};

resource 'ALRT' (139, "Save Prompts") {
    {0, 0, 124, 404},
    139,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'ALRT' (140, "OK Alerts") {
    {0, 0, 124, 404},
    140,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'ALRT' (141, "Cancel Alerts") {
    {0, 0, 124, 404},
    141,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'ALRT' (142, "Continue Alerts") {
    {0, 0, 124, 404},
    142,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'ALRT' (143, "Quit Alerts") {
    {0, 0, 124, 404},
    143,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'ALRT' (144, "Wide OK Alerts") {
    {0, 0, 124, 465},
    144,
    {   /* array: 4 elements */
        /* [1] */
        OK, visible, sound1,
        /* [2] */
        OK, visible, sound1,
        /* [3] */
        OK, visible, sound1,
        /* [4] */
        OK, visible, sound1
    },
    alertPositionMainScreen
};

resource 'DITL' (128, "About Box", purgeable) {
    {   /* array DITLarray: 3 elements */
        /* [1] */
        {0, 0, 72, 266},
        UserItem {
            enabled
        },
        /* [2] */
        {4, 6, 42, 261},
        StaticText {
            enabled,
            "               Macintosh Runtime\n"
            "              Window Environment"
        },
        /* [3] */
        {46, 10, 77, 256},
        StaticText {
            enabled,
            "Copyright © 2004 Absoft Corporation"
        }
    }
};

resource 'DITL' (129, "Printing in progress") {
    {   /* array DITLarray: 1 elements */
        /* [1] */
        {6, 27, 42, 177},
        StaticText {
            disabled,
            "Printing in progress\nPress \0x11-. to cancel."
        }
    }
};

resource 'DITL' (139) {
    {   /* array DITLarray: 3 elements */
        /* [1] */
        {91, 331, 111, 391},
        Button {
            enabled,
            "Save"
        },
        /* [2] */
        {91, 238, 111, 318},
        Button {
            enabled,
            "Discard"
        },
        /* [3] */
        {13, 78, 78, 391},
        StaticText {
            disabled,
            "^0"
        },
        /* [4] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};

resource 'DITL' (140, "OK Alerts") {
    {   /* array DITLarray: 2 elements */
        /* [1] */
        {91, 331, 111, 391},
        Button {
            enabled,
            "OK"
        },
        /* [2] */
        {13, 78, 78, 391},
        StaticText {
            disabled,
            "^0"
        },
        /* [3] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};

resource 'DITL' (141, "Cancel Alerts") {
    {   /* array DITLarray: 2 elements */
        /* [1] */
        {91, 331, 111, 391},
        Button {
            enabled,
            "Cancel"
        },
        /* [2] */
        {13, 78, 78, 391},
        StaticText {
            disabled,
            "^0"
        },
        /* [3] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};

resource 'DITL' (142, "Continue Alerts") {
    {   /* array DITLarray: 2 elements */
        /* [1] */
        {91, 320, 111, 391},
        Button {
            enabled,
            "Continue"
        },
        /* [2] */
        {13, 78, 78, 391},
        StaticText {
            disabled,
            "^0"
        },
        /* [3] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};

resource 'DITL' (143, "Quit Alerts") {
    {   /* array DITLarray: 2 elements */
        /* [1] */
        {91, 331, 111, 391},
        Button {
            enabled,
            "Quit"
        },
        /* [2] */
        {13, 78, 78, 391},
        StaticText {
            disabled,
            "^0"
        },
        /* [3] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};

resource 'DITL' (144, "Wide OK Alerts") {
    {   /* array DITLarray: 2 elements */
        /* [1] */
        {91, 392, 111, 452},
        Button {
            enabled,
            "OK"
        },
        /* [2] */
        {13, 78, 78, 452},
        StaticText {
            disabled,
            "^0"
        },
        /* [3] */
        {13, 23, 45, 55},
        Icon {
            disabled,
            0
        }
    }
};



resource 'BNDL' (128, purgeable) {
    APPSGN,
    0,
    {   /* array TypeArray: 2 elements */
        /* [1] */
        'FREF',
        {   /* array IDArray: 3 elements */
            /* [1] */
            0, 128,
            /* [2] */
            1, 129,
            /* [3] */
            2, 130
        },
        /* [2] */
        'ICN#',
        {   /* array IDArray: 3 elements */
            /* [1] */
            0, 0,
            /* [2] */
            1, 0,
            /* [3] */
            2, 0
        }
    }
};

resource 'FREF' (128, purgeable) {
    'APPL',
    0,
    ""
};

resource 'FREF' (129, purgeable) {
    'TEXT',
    1,
    ""
};

resource 'FREF' (130, purgeable) {
    '****',
    2,
    ""
};

type APPSGN as 'STR ';
resource APPSGN (0, purgeable) {
    "Application built with Absoft compiler tools"
};


/* This resource serves as flags to MultiFinder to tell it how to treat this program.
    There should be no need to change anything here except the amount of memory that
    is to be reserved for the application.  Initially it is set to 1024K, but it can
    be set to anything.  If it isn't enough, the program will probably crash.  If it
    is too much, MultiFinder will tell you there isn't enough memory to run the
    program.  */

resource 'SIZE' (-1) {
    dontSaveScreen,
    acceptSuspendResumeEvents,
    enableOptionSwitch,
    canBackground,              /* we can background and will receive null events */
                                /*  to keep the application running */
    multiFinderAware,           /* this says we do our own activate/deactivate; don't fake us out */
    backgroundAndForeground,    /* this is definitely not a background-only application! */
    dontGetFrontClicks,         /* change this is if you want "do first click" behavior like the Finder */
    ignoreChildDiedEvents,      /* essentially, I'm not a debugger (sub-launching) */
    is32BitCompatible,          /* this app can be run in 32-bit address space */
    isHighLevelEventAware,
    localAndRemoteHLEvents,
    reserved,
    reserved,
    reserved,
    reserved,
    reserved,
    2048 * 1024,        /* Amount of memory preferred for the application */
    1024 * 1024         /* Minimum memory allowable to run the application */
};

/*********************************************************************

The following resources are only used by the GMH plotting routines

*********************************************************************/


resource 'WIND' (500, purgeable, preload) {
    {38, 1, 478, 637},
    zoomDocProc,
    invisible,
    goAway,
    0x0,
    "Graphics Window",
    noAutoCenter
};

resource 'WIND' (501, purgeable, preload) {
    {455, 3, 470, 113},
    plainDBox,
    invisible,
    noGoAway,
    0x0,
    "Coordinage Window",
    noAutoCenter
};

resource 'DLOG' (130, "File Error", purgeable) {
    {100, 100, 160, 500},
    dBoxProc,
    invisible,
    noGoAway,
    0x0,
    130,
    "File Error",
    noAutoCenter
};

resource 'DITL' (130, "File Error") {
    {   /* array DITLarray: 1 elements */
        /* [1] */
        {6, 27, 42, 177},
        StaticText {
            disabled,
            "File Error."
        }
    }
};
