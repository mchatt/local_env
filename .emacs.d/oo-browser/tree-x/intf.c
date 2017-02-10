/* ----------------------------------------------------------------------------
 * File    : intf.c
 * Purpose : X11/Athena interface code
 * ----------------------------------------------------------------------------
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_USLEEP
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#else
int usleep(unsigned long microSeconds);
#endif /* HAVE_USLEEP */

#define GLOBALS 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include <X11/IntrinsicP.h>
#include <X11/ShellP.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* /usr/ucb/cc on DEC OSF requires that the #includes
   not be indented. */
#ifdef XAW3D
#include <X11/Xaw3d/Box.h>
#include <X11/Xaw3d/Simple.h>
#include <X11/Xaw3d/MenuButton.h>
#include <X11/Xaw3d/SimpleMenu.h>
#include <X11/Xaw3d/SmeBSB.h>
#include <X11/Xaw3d/SmeLine.h>
#include <X11/Xaw3d/Form.h>
#include <X11/Xaw3d/Label.h>
#include <X11/Xaw3d/Dialog.h>
#include <X11/Xaw3d/AsciiText.h>
#include <X11/Xaw3d/AsciiSrc.h>
#include <X11/Xaw3d/Viewport.h>
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/Paned.h>
#include <X11/Xaw3d/Panner.h>
#include <X11/Xaw3d/Reports.h>
#else  
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Simple.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Panner.h>
#include <X11/Xaw/Reports.h>
#endif

#include "defs.h"
#include "tree.h"
#include "dbl.h"
#include "intf.h"
#include "rsrc.h"
#include "input.h"
#include "help.h"
#include "dissolve.h"

#include <X11/cursorfont.h>

/* ------------------------------------------------------------------------- */
static Tree *PrevNode = NULL;

/* ------------------------------------------------------------------------- */
/*				Global Variables                             */
/* ------------------------------------------------------------------------- */

extern char  *EnvNm;    /* Stores name of current Envir file */

Widget        TreeTopLevel;
Widget        TreeDrawingArea;
Display      *TreeDisplay;
XtAppContext  app;
int           TreeScreen;
int           TreeContourWidth;
int           TreeBorderSize = BORDER_SIZE;
int           TreeParentDistance = PARENT_DISTANCE;
XFontStruct  *TreeLabelFont;
DoubleBuffer *TreeDrawingAreaDB;
ContourOption TreeShowContourOption = NoContours;
DensityOption TreeLayoutDensity = Fixed;
char          TreeShowSteps =  FALSE;
char          TreeAlignNodes = FALSE;
char          PauseAfterStep    = FALSE;

/* ------------------------------------------------------------------------- */
/*				Local Variables                              */
/* ------------------------------------------------------------------------- */

static Tree *PopupNode;		/* node selected for popup */
static NodePosition NodePos;

static Widget statusText;

static Widget scrolledWindow;

static Widget helpShell;
static Widget helpForm;
static Widget helpText;
static Widget helpDoneButton;
static Widget helpTitle;

static Widget treeMenus[NUM_MENUS];
static Widget treeMenuBtns[NUM_MENUS - 1];   /* don't need button for */
					     /* node popup menu       */

static Widget treeMenuItems[TREE_MENU_ITEMS];
#if 0
static Widget layoutMenuItems[LAYOUT_MENU_ITEMS];
#endif
static Widget nodeMenuItems[NODE_MENU_ITEMS];
static Widget helpBtn;

static String labelStr[NUM_MENU_STRS];
static String errStr[NUM_ERRS];

static Widget dialog[NUM_DLG];

/* globals used for state values */

static Widget dlgLevelScale, dlgLevelValuator,
              dlgSiblingValuator, dlgSiblingScale;
static int dlgLevelValue;
static int dlgSiblingValue;
static int loading_file = 0;

static Pixmap check_pixmap, slider_pixmap;

static Boolean nodeFound;

static int    PauseTime         = ANIMATION_SPEED_FAST;
static Cursor PauseCursor;

static XtResource applRsrcsDesc[] = {
   {
      TrNbackgroundColor,       TrCBackgroundColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, background_color),
      XtRString,                TrDefaultBackgroundColor
   },
   {
      TrNtreeColor,             TrCTreeColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, tree_color),
      XtRString,                TrDefaultTreeColor
   },
   {
      TrNcontourColor,          TrCContourColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, contour_color),
      XtRString,                TrDefaultContourColor
   },
   {
      TrNsplitColor,            TrCSplitColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, split_color),
      XtRString,                TrDefaultSplitColor
   },
   {
      TrNhighlightColor,        TrCHighlightColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, highlight_color),
      XtRString,                TrDefaultHighlightColor
   },
   {
      TrNactionColor,           TrCActionColor,
      XtRPixel,                 sizeof(Pixel),
      XtOffset(ApplRsrcDataPtr, action_color),
      XtRString,                TrDefaultActionColor
   },
   {
      TrNcontourWidth,          TrCContourWidth,
      XtRInt,                   sizeof(int),
      XtOffset(ApplRsrcDataPtr, contour_width),
      XtRImmediate,             (XtPointer) TrDefaultContourWidth
   },
};

static void popdown_action    (Widget, XEvent *, String *, Cardinal *);
static void activate_action   (Widget, XEvent *, String *, Cardinal *);
static void quit_action       (Widget, XEvent *, String *, Cardinal *);

static void select_action     (Widget, XEvent *, String *, Cardinal *);
static void button_action     (Widget, XEvent *, String *, Cardinal *);
static void menu_popup_action (Widget, XEvent *, String *, Cardinal *);

static void set_levelscale_CB   (Widget, XtPointer, XtPointer);
static void     levelscale_CB   (Widget, XtPointer, XtPointer);
static void set_levelvalue_CB   (Widget, XtPointer, XtPointer);
static void     levelvalue_CB   (Widget, XtPointer, XtPointer);
static void set_siblingscale_CB (Widget, XtPointer, XtPointer);
static void     siblingscale_CB (Widget, XtPointer, XtPointer);
static void set_siblingvalue_CB (Widget, XtPointer, XtPointer);
static void     siblingvalue_CB (Widget, XtPointer, XtPointer);

static void center_widget(Widget w, int x, int y);

static XtActionsRec local_actions[] = {
  {"wmpopdown", popdown_action},
  {"dlg_activate", activate_action},
  {"node_select", select_action},
  {"node_menu_prepare", button_action},
  {"maybe_menu_popup", menu_popup_action},
  {"quit", quit_action},
};

String fallback_resources[]  = {
"*statusText.width:                      310",
"*treeDrawingArea.width:                 310",
"*treeDrawingArea.height:                400",
"*Text.Font:	      -*-lucidatypewriter-medium-r-*-*-12-*-*-*-*-*-iso8859-1",
"*Label.Font:	      -*-helvetica-medium-r-*-*-12-*-*-*-*-*-iso8859-1",
"*Command.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*SimpleMenu.Font:    -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*SmeBSB.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*MenuButton.Font:    -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*List.Font:	      -*-helvetica-bold-r-*-*-12-*-*-*-*-*-iso8859-1",
"*treeMenuBtn.label:			Tree",
#if 0
"*layoutMenuBtn.label:			Layout",
#endif
"*helpBtn.label:			Help...",
"*treeMenu*treeMenuNew.label:		New Tree",
"*treeMenu*treeMenuLoad.label:		Load Tree",
"*treeMenu*treeMenuSave.label:		Save Tree",
"*treeMenu*treeMenuQuit.label:		Quit",
#if 0
"*layoutMenu*layoutMenuFixed.label:	Fixed",
"*layoutMenu*layoutMenuVariable.label:	Variable",
"*layoutMenu*layoutMenuSpacing.label:	Spacing...",
"*layoutMenu*layoutMenuAlignNodes.label: Align Nodes on Level",
#endif
"*dlgNewTree.title:			New Tree",
"*dlgNodeName.title:			Node Name",
"*dlgInfo.title:			Information",
"*dlgError.title:			Error!",
"*dlgSpacing.title:			Tree Node Spacing",
"*helpShell.title:			Help",
"*dlgNewTreeForm.label:			Enter name of root node:",
"*dlgNodeNameForm.label:		Enter name of new node:",
"*dlgFileForm.label:			Enter name of file:",
"*dlgInfoForm.label:			Information:",
"*dlgErrorForm.label:			Error:",
"*dlgSpacing.label:			Spacing Dialog",
"*dlgLevelLabel.label:			Level spacing:",
"*dlgSiblingLabel.label:		Sibling spacing:",
"*dlgLevelValuator.width:		40",
"*dlgLevelValuator.width:		40",
"*dlgNewTreeOk.label:			Ok",
"*dlgNewTreeCancel.label:		Cancel",
"*dlgFileOk.label:			Ok",
"*dlgFileCancel.label:			Cancel",
"*dlgInfoButton.label:			Acknowledged",
"*dlgErrorButton.label:			Acknowledged",
"*dlgOkButton.label:			Ok",
"*dlgCancelButton.label:		Cancel",
"*dlgNodeNameOk.label:			Ok",
"*dlgNodeNameCancel.label:		Cancel",
"*helpDoneButton.label:			Dismiss",
"*helpTitle.label:			Help for Drawing Dynamic Trees",
"*treeDrawingArea.Translations: #override \\n\
                                <Btn1Down>:	node_select(EDIT)\\n\
                                <Btn2Down>:	node_select(VIEW)\\n\
                                <Btn3Down>:	XawPositionSimpleMenu(nodeMenu)node_menu_prepare()XtMenuPopup(nodeMenu)\\n",
NULL,
};

static ApplRsrcData    applRsrcs;
static ApplRsrcDataPtr applRsrcsPtr;

/* included bitmaps */

#define gray_width 8
#define gray_height 8
static unsigned char gray_bits[] = {
  0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa
};

/* This is a check mark stolen from the xmh application. :) */
#define check_width 9
#define check_height 8
static unsigned char check_bits[] = {
  0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
  0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};

#define slider_pixmap_width 4
#define slider_pixmap_height 4
static unsigned char slider_pixmap_bits[] = {
  0x05, 0x0a, 0x05, 0x0a
};

/* ------------------------------------------------------------------------- */
/*			 Forward Function Declarations                       */
/* ------------------------------------------------------------------------- */

static void popup_dialog(Widget, XtGrabKind );

static void Help_CB       (Widget, XtPointer, XtPointer);
static void HelpDone_CB   (Widget, XtPointer, XtPointer);
static void Scale_CB      (Widget, XtPointer, XtPointer);

static void TreeMenu_CB   (Widget, XtPointer, XtPointer);
#if 0
static void LayoutMenu_CB (Widget, XtPointer, XtPointer);
#endif
static void NodeMenu_CB   (Widget, XtPointer, XtPointer);

static void NodeLabel_CB  (Widget, XtPointer, XtPointer);
static void NewTree_CB    (Widget, XtPointer, XtPointer);
static void File_CB       (Widget, XtPointer, XtPointer);

static void Popdown_CB    (Widget, XtPointer, XtPointer);

/* ------------------------------------------------------------------------- */
/*				   Functions                                 */
/* ------------------------------------------------------------------------- */

/* ----------------------------------------------------------------------------
 *
 *   Status() displays the specified text in the status area.
 *   'urgent' overrides the value of TreeShowSteps.
 *
 * ----------------------------------------------------------------------------
 */

void
StatusMsg(char *msg, int urgent)
{
  if (TreeShowSteps || urgent)
    {
      Arg al [1];
      XtSetArg (al [0], XtNlabel, msg);
      XtSetValues(statusText, al, 1);
    }
}


/* ----------------------------------------------------------------------------
 *
 *  Pause is a trivial function, for the benefit of interface-dependent code
 *  inside tree.c. This avoids having to include X11 stuff inside that file.
 *  PauseTime is expected to contain an integer indicating 1/10ths of a sec.
 *
 * ----------------------------------------------------------------------------
 */

void
Pause(void)
{
  XEvent event;

  if (PauseAfterStep) {
    XGrabKeyboard(TreeDisplay, XtWindow(TreeTopLevel), False,
		  GrabModeAsync, GrabModeAsync, CurrentTime);
    XGrabPointer(TreeDisplay, XtWindow(TreeTopLevel), False,
		 ButtonPressMask | ButtonReleaseMask,
		 GrabModeAsync, GrabModeAsync, None,
		 PauseCursor, CurrentTime);
    XMaskEvent(TreeDisplay, ButtonPressMask | KeyPress, &event);
    XUngrabPointer(TreeDisplay, CurrentTime);
    XUngrabKeyboard(TreeDisplay, CurrentTime);
  }
  else if (TreeShowSteps) {
    XSync(TreeDisplay, 0);
    usleep(PauseTime * 100000);
  }
}

/* ----------------------------------------------------------------------------
 *
 *   BuildPopupMenus() builds the popup menus for use in the application
 *   window.
 *
 * ----------------------------------------------------------------------------
 */

static void
BuildPopupMenus(Widget parent)
{
  Arg al[2];

  labelStr[STR_NODE_COLLAPSE] = "Collapse Node";
  labelStr[STR_NODE_EXPAND]   = "Expand Node";

  XtSetArg (al [0], XtNlabel, " ");
  treeMenus[NODE_MENU] =
    XtCreatePopupShell("nodeMenu", simpleMenuWidgetClass, parent, al, 1);

  XtCreateManagedWidget("nodeMenuElision", smeLineObjectClass,
			treeMenus[NODE_MENU], NULL, 0);

#if 0
/*   These leave the display corrupted, probably due to double buffering
     issues and aren't needed for the OO-Browser at this point.  -- BW,
     BeOpen.com, 4/16/1998  */

  XtSetArg (al [0], XtNlabel, "Add Child");
  nodeMenuItems[NODE_MENU_ADD_CHILD] =
    XtCreateManagedWidget("nodeMenuAddChild", smeBSBObjectClass,
			  treeMenus[NODE_MENU], al, 1);
  XtAddCallback(nodeMenuItems[NODE_MENU_ADD_CHILD],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_CHILD);

  XtSetArg (al [0], XtNlabel, "Add Sibling Before");
  nodeMenuItems[NODE_MENU_ADD_BEFORE] =
    XtCreateManagedWidget("nodeMenuAddSiblingBefore", smeBSBObjectClass,
			  treeMenus[NODE_MENU], al, 1);
  XtAddCallback(nodeMenuItems[NODE_MENU_ADD_BEFORE],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_BEFORE);

  XtSetArg (al [0], XtNlabel, "Add Sibling After");
  nodeMenuItems[NODE_MENU_ADD_AFTER] =
    XtCreateManagedWidget("nodeMenuAddSiblingAfter", smeBSBObjectClass,
			  treeMenus[NODE_MENU], al, 1);
  XtAddCallback(nodeMenuItems[NODE_MENU_ADD_AFTER],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ADD_AFTER);

#endif

  XtSetArg (al [0], XtNlabel, labelStr[STR_NODE_COLLAPSE]);
  nodeMenuItems[NODE_MENU_ELISION] =
    XtCreateManagedWidget("nodeMenuElision", smeBSBObjectClass,
			  treeMenus[NODE_MENU], al, 1);
  XtAddCallback(nodeMenuItems[NODE_MENU_ELISION],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_ELISION);


  XtSetArg (al [0], XtNlabel, "Delete Node");
  nodeMenuItems[NODE_MENU_DELETE] =
    XtCreateManagedWidget("nodeMenuDeleteNode", smeBSBObjectClass,
			    treeMenus[NODE_MENU], al, 1);
  XtAddCallback(nodeMenuItems[NODE_MENU_DELETE],
		XtNcallback, NodeMenu_CB, (XtPointer) NODE_MENU_DELETE);
}

/* ----------------------------------------------------------------------------
 *
 *   BuildMenubar() builds the menubar that appears in the main window.
 *
 * ----------------------------------------------------------------------------
 */


static void
BuildMenubar(Widget parent)
{
  Arg al[4];
  Widget box;

  XtSetArg (al [0], XtNshowGrip, FALSE);
  XtSetArg (al [1], XtNborderWidth, 0);
  box = XtCreateManagedWidget("menuBox", boxWidgetClass, parent, al, 2);

  /* menu button widgets */
  XtSetArg (al [0], XtNmenuName, "treeMenu");
  treeMenuBtns[TREE_MENU] =
    XtCreateManagedWidget("treeMenuBtn", menuButtonWidgetClass, box, al, 1);
#if 0
  XtSetArg (al [0], XtNmenuName, "layoutMenu");
  treeMenuBtns[LAYOUT_MENU] =
    XtCreateManagedWidget("layoutMenuBtn", menuButtonWidgetClass, box, al, 1);
#endif

  helpBtn = XtCreateManagedWidget("helpBtn", commandWidgetClass, box, NULL, 0);

  XtAddCallback(helpBtn, XtNcallback, Help_CB, NULL);

  /* create pulldown menus */

  treeMenus[TREE_MENU] =
    XtCreatePopupShell("treeMenu", simpleMenuWidgetClass,
		       treeMenuBtns[TREE_MENU], NULL, 0);

#if 0
  treeMenus[LAYOUT_MENU] =
    XtCreatePopupShell("layoutMenu", simpleMenuWidgetClass,
		       treeMenuBtns[LAYOUT_MENU], NULL, 0);
#endif


  /* adding menu entries */

  /* Tree menu */

  treeMenuItems[TREE_MENU_NEW] =
    XtCreateManagedWidget("treeMenuNew", smeBSBObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);
  XtAddCallback(treeMenuItems[TREE_MENU_NEW],
		XtNcallback, TreeMenu_CB, TREE_MENU_NEW);

  treeMenuItems[TREE_MENU_LOAD] =
    XtCreateManagedWidget("treeMenuLoad", smeBSBObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);
  XtAddCallback(treeMenuItems[TREE_MENU_LOAD],
		XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_LOAD);

  treeMenuItems[TREE_MENU_SAVE] =
    XtCreateManagedWidget("treeMenuSave", smeBSBObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);
  XtAddCallback(treeMenuItems[TREE_MENU_SAVE],
		XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_SAVE);

  treeMenuItems[TREE_MENU_SEP1] =
    XtCreateManagedWidget("treeMenuSep1", smeLineObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);

#ifdef COMMENT
  XtSetArg (al [0], XtNlabel, labelStr[STR_SHOW_STATS]);
  treeMenuItems[TREE_MENU_STATS] =
    XtCreateManagedWidget("treeMenuStats", smeBSBObjectClass,
			  treeMenus[TREE_MENU], al, 1);
  XtAddCallback(treeMenuItems[TREE_MENU_STATS],
		XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_STATS);
  XtSetSensitive(treeMenuItems[TREE_MENU_STATS], FALSE);

  treeMenuItems[TREE_MENU_SEP2] =
    XtCreateManagedWidget("treeMenuSep2", smeLineObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);
#endif /* COMMENT */

  treeMenuItems[TREE_MENU_QUIT] =
    XtCreateManagedWidget("treeMenuQuit", smeBSBObjectClass,
			  treeMenus[TREE_MENU], NULL, 0);
  XtAddCallback(treeMenuItems[TREE_MENU_QUIT],
		XtNcallback, TreeMenu_CB, (XtPointer) TREE_MENU_QUIT);



  /* Layout menu */

#ifdef COMMENT
  XtSetArg (al [0], XtNleftMargin, 18);
  treeMenuItems[LAYOUT_MENU_FIXED] =
    XtCreateManagedWidget("layoutMenuFixed", smeBSBObjectClass,
			  treeMenus[LAYOUT_MENU], al, 1);

  XtAddCallback(treeMenuItems[LAYOUT_MENU_FIXED],
		XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_FIXED);

  XtSetArg (al [0], XtNleftMargin, 18);
  treeMenuItems[LAYOUT_MENU_VARIABLE] =
    XtCreateManagedWidget("layoutMenuVariable", smeBSBObjectClass,
			  treeMenus[LAYOUT_MENU], al, 1);

  XtAddCallback(treeMenuItems[LAYOUT_MENU_VARIABLE],
		XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_VARIABLE);


  treeMenuItems[LAYOUT_MENU_SEP1] =
    XtCreateManagedWidget("layoutSep1", smeLineObjectClass,
			  treeMenus[LAYOUT_MENU], NULL, 0)

  XtSetArg (al [0], XtNleftMargin, 18);
  treeMenuItems[LAYOUT_MENU_SPACING] =
    XtCreateManagedWidget("layoutMenuSpacing", smeBSBObjectClass,
			    treeMenus[LAYOUT_MENU], al, 1);
  XtAddCallback(treeMenuItems[LAYOUT_MENU_SPACING],
		XtNcallback, LayoutMenu_CB, (XtPointer) LAYOUT_MENU_SPACING);

  treeMenuItems[LAYOUT_MENU_SEP2] =
    XtCreateManagedWidget("layoutMenuSep2", smeLineObjectClass,
			  treeMenus[LAYOUT_MENU], NULL, 0);

  /* the following is a toggle button - we hack it using a bitmap */
  XtSetArg (al [0], XtNleftMargin, 18);
  treeMenuItems[LAYOUT_MENU_ALIGN_NODES] =
    XtCreateManagedWidget("layoutMenuAlignNodes", smeBSBObjectClass,
			  treeMenus[LAYOUT_MENU], al, 1);
  XtAddCallback(treeMenuItems[LAYOUT_MENU_ALIGN_NODES],
		XtNcallback, LayoutMenu_CB,
		(XtPointer) LAYOUT_MENU_ALIGN_NODES);
#endif /* COMMENT */

}

/* ----------------------------------------------------------------------------
 *
 *   BuildDialogs() builds all the dialogs used in this application.
 *
 * ----------------------------------------------------------------------------
 */

static void
BuildDialogs (Widget parent)
{
  Arg al [20]; int ac;
  Widget
    tmpwidget, dlgForm, dlgValue,
    dlgLevelLabel, dlgSiblingLabel,
    dlgOkButton, dlgCancelButton;

  labelStr[STR_LOAD_FILE] = "Name of file to load:";
  labelStr[STR_SAVE_FILE] = "Name of file to save:";

  errStr[ERR_OPENFAIL]  = "Failed to open file.";
  errStr[ERR_EMPTYFILE] = "Input file is empty.";
  errStr[ERR_MEMALLOC]  = "Memory allocation failed.";
  errStr[ERR_NOBEGIN] = "End of list does not have a matching beginning.";
  errStr[ERR_NOEND] = "Beginning of list does not have a matching end.";
  errStr[ERR_NOROOT] = "Input file does not have a root label.";
  errStr[ERR_MANYROOT] = "Input file has more than one root label.";


  XtSetArg (al [0], XtNresizable, TRUE);
  dialog[DLG_NEW] = XtCreatePopupShell("dlgNewTree", transientShellWidgetClass,
				       parent, al, 1);

  XtSetArg (al [0], XtNresizable, TRUE);
  XtSetArg (al [1], XtNvalue, "");
  tmpwidget = XtCreateManagedWidget("dlgNewTreeForm", dialogWidgetClass,
				    dialog[DLG_NEW], al, 2);

  XawDialogAddButton(tmpwidget, "dlgNewTreeOk", NewTree_CB,
		     (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgNewTreeCancel", NewTree_CB,
		     (XtPointer) FALSE);

  dlgValue = XtNameToWidget(tmpwidget, "value");

  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return:dlg_activate(dlgNewTreeOk)\n"));


  XtSetArg (al [0], XtNresizable, TRUE);
  dialog[DLG_NODE_NAME] = XtCreatePopupShell("dlgNodeName",
					     transientShellWidgetClass,
					     TreeTopLevel, al, 1);

  XtSetArg (al [0], XtNresizable, TRUE);
  XtSetArg (al [1], XtNvalue, "");
  tmpwidget= XtCreateManagedWidget("dlgNodeNameForm", dialogWidgetClass,
				   dialog[DLG_NODE_NAME], al, 2);

  XawDialogAddButton(tmpwidget, "dlgNodeNameOk", NodeLabel_CB,
		     (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgNodeNameCancel", NodeLabel_CB,
		     (XtPointer) FALSE);

  dlgValue = XtNameToWidget(tmpwidget, "value");

  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return: dlg_activate(dlgNodeNameOk)\n"));


  XtSetArg (al [0], XtNresizable, TRUE);
  dialog[DLG_FILE] = XtCreatePopupShell("dlgFile",
					transientShellWidgetClass,
					TreeTopLevel, al, 1);

  XtSetArg (al [0], XtNresizable, TRUE);
  XtSetArg (al [1], XtNvalue, "");
  tmpwidget = XtCreateManagedWidget("dlgFileForm", dialogWidgetClass,
				    dialog[DLG_FILE], al, 2);

  XawDialogAddButton(tmpwidget, "dlgFileOk",     File_CB, (XtPointer) TRUE);
  XawDialogAddButton(tmpwidget, "dlgFileCancel", File_CB, (XtPointer) FALSE);

  dlgValue = XtNameToWidget(tmpwidget, "value");

  XtOverrideTranslations
    (dlgValue, XtParseTranslationTable
     ("<Key>Return:dlg_activate(dlgFileOk)\n"));


  XtSetArg (al [0], XtNresizable, TRUE);
  dialog[DLG_INFO] = XtCreatePopupShell("dlgInfo",
					transientShellWidgetClass,
					TreeTopLevel, al, 1);

  XtSetArg (al [0], XtNresizable, TRUE);
  XtSetArg (al [1], XtNvalue, "");
  tmpwidget = XtCreatePopupShell("dlgInfoForm",
				 dialogWidgetClass,
				 dialog[DLG_INFO], al, 2);

  XawDialogAddButton(tmpwidget, "dlgInfoButton", Popdown_CB,
		     (XtPointer) dialog[DLG_INFO]);


  XtSetArg (al [0], XtNresizable, TRUE);
  dialog[DLG_ERROR] = XtCreatePopupShell("dlgError",
					 transientShellWidgetClass,
					 TreeTopLevel, al, 1);

  XtSetArg (al [0], XtNresizable, TRUE);
  XtSetArg (al [1], XtNvalue, "");
  tmpwidget = XtCreateManagedWidget("dlgErrorForm",
				      dialogWidgetClass,
				      dialog[DLG_ERROR], al, 2);

  XawDialogAddButton(tmpwidget, "dlgErrorButton", Popdown_CB,
		     (XtPointer) dialog[DLG_ERROR]);


  XtSetArg (al [0], XtNresizable, FALSE);
  dialog[DLG_SPACING] = XtCreatePopupShell("dlgSpacing",
					   transientShellWidgetClass,
					   TreeTopLevel, al, 1);

  XtSetArg (al [0], XtNresizable, FALSE);
  dlgForm = XtCreateManagedWidget("dlgSpacingForm",
				  formWidgetClass,
				  dialog[DLG_SPACING], al, 1);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  NULL);      ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);      ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);  ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);  ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft); ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft); ac++;
  XtSetArg (al [ac], XtNborderWidth, 0);       ac++;
  dlgLevelLabel = XtCreateManagedWidget("dlgLevelLabel", labelWidgetClass,
					dlgForm, al, ac);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  NULL);          ac++;
  XtSetArg (al [ac], XtNfromHoriz, dlgLevelLabel); ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);      ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);      ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNlabel, "   ");             ac++;
  dlgLevelValuator = XtCreateManagedWidget("dlgLevelValuator",
					   labelWidgetClass,
					   dlgForm, al, ac);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgLevelLabel); ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);          ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);      ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);      ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNcanvasWidth, 110);         ac++;
  XtSetArg (al [ac], XtNdefaultScale, 1);          ac++;
  XtSetArg (al [ac], XtNinternalSpace, 1);         ac++;
  XtSetArg (al [ac], XtNrubberBand, FALSE);        ac++;
  XtSetArg (al [ac], XtNshadowThickness, 0);       ac++;
  XtSetArg (al [ac], XtNallowOff, FALSE);          ac++;
  XtSetArg (al [ac], XtNwidth, 200);               ac++;
  XtSetArg (al [ac], XtNsliderWidth, 10);          ac++;
  XtSetArg (al [ac], XtNheight, 15);               ac++;
  dlgLevelScale = XtCreateManagedWidget("dlgLevelScale",
					pannerWidgetClass,
					dlgForm, al, ac);

  XtOverrideTranslations(dlgLevelScale, XtParseTranslationTable("\
<Btn2Down>:		start()\n\
<Btn2Motion>:		move()\n\
<Btn2Up>:		notify()stop()\n\
<Btn3Down>:		abort()\n"));

  XtAddCallback(dlgLevelScale, XtNreportCallback, levelscale_CB,
		(XtPointer) dlgLevelValuator);
  XtAddCallback(dlgLevelScale, XtNreportCallback, levelvalue_CB,
		(XtPointer) &dlgLevelValue);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgLevelScale); ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);          ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);      ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);      ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);     ac++;
  XtSetArg (al [ac], XtNborderWidth, 0);           ac++;
  dlgSiblingLabel = XtCreateManagedWidget("dlgSiblingLabel",
					  labelWidgetClass,
					  dlgForm, al, ac);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgLevelScale);   ac++;
  XtSetArg (al [ac], XtNfromHoriz, dlgSiblingLabel); ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);        ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);        ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNlabel, "   ");               ac++;
  dlgSiblingValuator = XtCreateManagedWidget("dlgLevelValuator",
					     labelWidgetClass,
					     dlgForm, al, ac);
  
  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgSiblingLabel); ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);            ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);        ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);        ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNcanvasWidth, 110);           ac++;
  XtSetArg (al [ac], XtNdefaultScale, 1);            ac++;
  XtSetArg (al [ac], XtNinternalSpace, 1);           ac++;
  XtSetArg (al [ac], XtNrubberBand, FALSE);          ac++;
  XtSetArg (al [ac], XtNshadowThickness, 0);         ac++;
  XtSetArg (al [ac], XtNallowOff, FALSE);            ac++;
  XtSetArg (al [ac], XtNwidth, 200);                 ac++;
  XtSetArg (al [ac], XtNsliderWidth, 10);            ac++;
  XtSetArg (al [ac], XtNheight, 15);                 ac++;
  dlgSiblingScale = XtCreateManagedWidget("dlgSiblingScale",
					  pannerWidgetClass,
					  dlgForm, al, ac);

  XtOverrideTranslations(dlgSiblingScale, XtParseTranslationTable("\
<Btn2Down>:		start()\n\
<Btn2Motion>:		move()\n\
<Btn2Up>:		notify()stop()\n\
<Btn3Down>:		abort()\n"));

  XtAddCallback(dlgSiblingScale, XtNreportCallback, siblingscale_CB,
		(XtPointer) dlgSiblingValuator);
  XtAddCallback(dlgSiblingScale, XtNreportCallback, siblingvalue_CB,
		(XtPointer) &dlgSiblingValue);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgSiblingLabel); ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);            ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);        ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);        ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNlabel, "   ");               ac++;
  dlgOkButton = XtCreateManagedWidget("dlgOkButton", commandWidgetClass,
				      dlgForm, al, ac);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  dlgSiblingScale); ac++;
  XtSetArg (al [ac], XtNfromHoriz, dlgOkButton);     ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);        ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);        ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);       ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);       ac++;
  dlgCancelButton = XtCreateManagedWidget("dlgCancelButton",
					  commandWidgetClass,
					  dlgForm, al, ac);

  XtAddCallback(dlgOkButton,     XtNcallback, Scale_CB, (XtPointer) TRUE);
  XtAddCallback(dlgCancelButton, XtNcallback, Scale_CB, (XtPointer) FALSE);
}

/* ----------------------------------------------------------------------------
 *
 *   BuildHelpWindow() builds the help window that is displayed when the
 *   Help button on the application main window is pressed.
 *
 * ----------------------------------------------------------------------------
 */


static void
BuildHelpWindow(Widget parent)
{
  Arg al [20]; int ac;
  
  helpShell = XtCreatePopupShell("helpShell", transientShellWidgetClass,
				   parent, NULL, 0);

  helpForm = XtCreateManagedWidget("helpForm", formWidgetClass,
				   helpShell, NULL, 0);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  NULL);      ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);      ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);  ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);  ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft); ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft); ac++;
  helpDoneButton = XtCreateManagedWidget("helpDoneButton",
					 commandWidgetClass,
					 helpForm, al, ac);

  XtAddCallback(helpDoneButton, XtNcallback, Popdown_CB,
		(XtPointer) helpShell);

  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  NULL);           ac++;
  XtSetArg (al [ac], XtNfromHoriz, helpDoneButton); ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);       ac++;
  XtSetArg (al [ac], XtNbottom, XawChainTop);       ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);      ac++;
  XtSetArg (al [ac], XtNright,  XawChainLeft);      ac++;
  XtSetArg (al [ac], XtNborderWidth, 0);            ac++;
  XtSetArg (al [ac], XtNlabel, "   ");              ac++;
  helpTitle = XtCreateManagedWidget("helpTitle", labelWidgetClass,
				    helpForm, al, ac);

  
  ac = 0;
  XtSetArg (al [ac], XtNfromVert,  helpDoneButton); ac++;
  XtSetArg (al [ac], XtNfromHoriz, NULL);           ac++;
  XtSetArg (al [ac], XtNtop,    XawChainTop);       ac++;
  XtSetArg (al [ac], XtNbottom, XawChainBottom);    ac++;
  XtSetArg (al [ac], XtNleft,   XawChainLeft);      ac++;
  XtSetArg (al [ac], XtNright,  XawChainRight);     ac++;
  XtSetArg (al [ac], XtNeditType, XawtextRead);     ac++;
  XtSetArg (al [ac], XtNdisplayCaret, FALSE);       ac++;
  XtSetArg (al [ac], XtNuseStringInPlace, TRUE);    ac++;
  XtSetArg (al [ac], XtNstring, help_text);         ac++;
  XtSetArg (al [ac], XtNwidth,  530);               ac++;
  XtSetArg (al [ac], XtNheight, 650);               ac++;
  XtSetArg (al [ac], XtNscrollVertical,   XawtextScrollAlways);     ac++;
  XtSetArg (al [ac], XtNscrollHorizontal, XawtextScrollWhenNeeded); ac++;
  helpText = XtCreateManagedWidget("helpText", asciiTextWidgetClass,
				   helpForm, al, ac);
}

/* ----------------------------------------------------------------------------
 *
 *   BuildApplicationWindow() assumes that TreeTopLevel has been initialized
 *   by XtInitialize(). TreeDrawingArea is set to the drawing area in this
 *   application window.
 *
 * ----------------------------------------------------------------------------
 */


static void
BuildApplicationWindow(Widget parent)
{
  Widget topPane;
  Arg al [20]; int ac;

  XtSetArg (al [0], XtNorientation, XtorientVertical);
  topPane = XtCreateManagedWidget("topPane", panedWidgetClass, parent, al, 1);

  BuildMenubar(topPane);

  XtSetArg (al [0], XtNborderWidth, 0);
  statusText = XtCreateManagedWidget("statusText", labelWidgetClass,
				     topPane, al, 1);

  ac = 0;
  XtSetArg (al [ac], XtNshowGrip,   FALSE); ac++;
  XtSetArg (al [ac], XtNallowHoriz, TRUE);  ac++;
  XtSetArg (al [ac], XtNallowVert,  TRUE);  ac++;
  XtSetArg (al [ac], XtNuseBottom,  TRUE);  ac++;
  XtSetArg (al [ac], XtNuseRight,   TRUE);  ac++;
  scrolledWindow = XtCreateManagedWidget("scrolledWindow", viewportWidgetClass,
					 topPane, al, ac);

  TreeDrawingArea = XtCreateManagedWidget("treeDrawingArea", simpleWidgetClass,
					  scrolledWindow, NULL, 0);
}

/* ----------------------------------------------------------------------------
 *
 *   HandleError() is used when ReadTreeFromFile() or SaveTreeToFile() returns
 *   an error.
 *
 * ----------------------------------------------------------------------------
 */

static void
HandleError(ErrCode error, Tree *tree)
{
  Widget dlg, text;

  dlg = tree ?
    XtNameToWidget(dialog[DLG_INFO],  "dlgInfoForm") :
    XtNameToWidget(dialog[DLG_ERROR], "dlgErrorForm");

  if (dlg) {
    Arg al [2];
    XtSetArg (al [0], XtNvalue, errStr[error]);
    XtSetArg (al [1], XtNwidth, 200);
    XtSetValues(dlg, al, 2);
    text = XtNameToWidget(dlg, "value");
    if (text) {
      XtSetArg (al [0], XtNdisplayCaret, FALSE);
      XtSetValues(text, al, 1);
      XtSetArg (al [0], XtNeditType, XawtextRead);
      XtSetValues(XawTextGetSource(text), al, 1);
    }
  }
  popup_dialog (tree ? dialog[DLG_INFO] : dialog[DLG_ERROR], XtGrabExclusive);
}


/* ----------------------------------------------------------------------------
 *
 *   InitializeInterface() initializes the environment for displaying
 *   the tree and creates the necessary supporting structures for interaction
 *   and display. This function should be called only once.
 *
 * ----------------------------------------------------------------------------
 */

static void
InitializeInterface(int *argc, char *argv[])
{
  XrmValue       rmvalue;
  char          *rmtype;
  Window         win;
  Pixel          background;
  XColor         colors[NUM_COLORS];
  Colormap       cmap;
  Pixmap         gray_pixmap;
  Atom		 wm_delete_window;
  Arg		 al [2];

  /* XtSetLanguageProc is broken on some systems.
     The symptom is an error of the form:
        Warning: X locale modifiers not supported, using default
     so don't use this call:

        XtSetLanguageProc(NULL, NULL, NULL);
  */

  TreeTopLevel = XtAppInitialize(&app, "Xoobr",
				 NULL, 0,
				 argc, argv,
				 fallback_resources,
				 NULL, 0);

  ASSERT(TreeTopLevel, "failed to open connection to X server");

  TreeDisplay = XtDisplay(TreeTopLevel);
  TreeScreen  = DefaultScreen(TreeDisplay);

  /* set up color resources */
  applRsrcsPtr = &applRsrcs;
  XtGetApplicationResources(TreeTopLevel, applRsrcsPtr,
			    applRsrcsDesc, XtNumber(applRsrcsDesc),
			    NULL, 0);

  /* add actions */
  XtAppAddActions(app, local_actions, 6);
  XawSimpleMenuAddGlobalActions(app);

  TreeContourWidth = applRsrcsPtr->contour_width;

  /* set up label font info */
  if (XrmGetResource(XtDatabase(TreeDisplay),
		     X11_FONT_RESOURCE, X11_FONT_CLASS_RESOURCE,
		     &rmtype, &rmvalue)) {
    if (! (TreeLabelFont = XLoadQueryFont(TreeDisplay, rmvalue.addr)) ) {
      WARN_4("cannot load user-specified font:", rmvalue.addr,
	     "using font:", X11_DEFAULT_FONT);
      TreeLabelFont = XLoadQueryFont(TreeDisplay, X11_DEFAULT_FONT);
    }
  }
  else
    TreeLabelFont = XLoadQueryFont(TreeDisplay, X11_DEFAULT_FONT);
  TreeLabelFont = XLoadQueryFont(TreeDisplay, "fixed");

  check_pixmap =
    XCreateBitmapFromData(TreeDisplay, RootWindow(TreeDisplay, TreeScreen),
			  (char *) check_bits, check_width, check_height);

  slider_pixmap =
    XCreateBitmapFromData(TreeDisplay, RootWindow(TreeDisplay, TreeScreen),
			  (char *) slider_pixmap_bits, slider_pixmap_width,
			  slider_pixmap_height);

  BuildApplicationWindow(TreeTopLevel);
  BuildHelpWindow(TreeTopLevel);
  BuildPopupMenus(TreeTopLevel);
  BuildDialogs(TreeTopLevel);


  XtAddEventHandler(TreeDrawingArea, ExposureMask, FALSE,
		    ExposeHandler, (XtPointer) NULL);

  XtSetArg (al [0], XtNbackground, applRsrcsPtr->background_color);
  XtSetValues(TreeDrawingArea, al, 1);

  XtRealizeWidget(TreeTopLevel);

  XtOverrideTranslations
    (TreeTopLevel,
     XtParseTranslationTable("<Message>WM_PROTOCOLS: quit()"));

  wm_delete_window = XInternAtom(TreeDisplay, "WM_DELETE_WINDOW", FALSE);
  (void) XSetWMProtocols(TreeDisplay, XtWindow(TreeTopLevel),
			 &wm_delete_window, 1);


  win = XtWindow(TreeDrawingArea);
  cmap = DefaultColormap(TreeDisplay, TreeScreen);

  /* set up double buffer */
  colors[BACKGROUND_COLOR].pixel = applRsrcsPtr->background_color;
  XQueryColor(TreeDisplay, cmap, &(colors[BACKGROUND_COLOR]));
  colors[TREE_COLOR].pixel = applRsrcsPtr->tree_color;
  XQueryColor(TreeDisplay, cmap, &(colors[TREE_COLOR]));
  colors[CONTOUR_COLOR].pixel = applRsrcsPtr->contour_color;
  XQueryColor(TreeDisplay, cmap, &(colors[CONTOUR_COLOR]));
  colors[HIGHLIGHT_COLOR].pixel = applRsrcsPtr->highlight_color;
  XQueryColor(TreeDisplay, cmap, &(colors[HIGHLIGHT_COLOR]));
  colors[SPLIT_COLOR].pixel = applRsrcsPtr->split_color;
  XQueryColor(TreeDisplay, cmap, &(colors[SPLIT_COLOR]));
  colors[ACTION_COLOR].pixel = applRsrcsPtr->action_color;
  XQueryColor(TreeDisplay, cmap, &(colors[ACTION_COLOR]));

  TreeDrawingAreaDB =
    DBLcreate_double_buffer(TreeDisplay,
			    XtWindow(TreeDrawingArea), TRUE,
			    colors, NUM_COLORS);

  ASSERT(TreeDrawingAreaDB, "could not create double buffer");

  background = DBLinq_background(TreeDrawingAreaDB);


  XtSetArg(al [0], XtNbackground, background);
  XtSetValues(TreeDrawingArea, al, 1);

  /* also set the scrolledWindow background to same color */
  XtSetArg(al [0], XtNbackground, background);
  XtSetValues(XtParent(TreeDrawingArea), al, 1);

  XSetFont(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc,
	   TreeLabelFont->fid);

  gray_pixmap =
    XCreatePixmapFromBitmapData(TreeDisplay, XtWindow(TreeDrawingArea),
				(char *)gray_bits, gray_width, gray_height,
				TreeDrawingAreaDB->colors[TREE_COLOR],
				background,
				DefaultDepth(TreeDisplay, TreeScreen));

#ifdef COMMENT
  XtSetArg(al [0], XtNleftBitmap, check_pixmap);
  XtSetValues(treeMenuItems[LAYOUT_MENU_VARIABLE], al, 1);
#endif

  XSetTile(TreeDrawingAreaDB->display, TreeDrawingAreaDB->gc, gray_pixmap);
  SetDrawColor(TREE_COLOR);

  InitializeDissolveEffect(TreeDisplay, XtWindow(TreeDrawingArea),
			   TreeDrawingAreaDB->colors[TREE_COLOR], background);

  PauseCursor = XCreateFontCursor(TreeDisplay, XC_watch);

  StatusMsg("Ready", TRUE);
}


/* ----------------------------------------------------------------------------
 *
 *   UserEventLoop() is called after InitializeInterface() to handle
 *   processing of events.
 *
 * ----------------------------------------------------------------------------
 */

static void
UserEventLoop(void)
{
  XtAppMainLoop(app);
}


/* ------------------------------------------------------------------------- */
/*			  Callback Routines                                  */
/* ------------------------------------------------------------------------- */

static void
TreeMenu_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  long		menuItem = (long) client_data;
  Widget	dlgFileForm;
  Arg		al [2];

  switch (menuItem) {
  case TREE_MENU_NEW:
    popup_dialog(dialog[DLG_NEW], XtGrabExclusive);
    break;
  case TREE_MENU_LOAD:
    loading_file = 1;
    XtSetArg (al [0], XtNtitle, "Load File");
    XtSetValues(dialog[DLG_FILE],  al, 1);
    dlgFileForm = XtNameToWidget(dialog[DLG_FILE], "dlgFileForm");
    XtSetArg (al [0], XtNvalue, "");
    XtSetValues(dlgFileForm, al, 1);
    popup_dialog(dialog[DLG_FILE], XtGrabExclusive);
    break;
  case TREE_MENU_SAVE:
    loading_file = 0;
    XtSetArg (al [0], XtNtitle, "Save File");
    XtSetValues(dialog[DLG_FILE],  al, 1);
    dlgFileForm = XtNameToWidget(dialog[DLG_FILE], "dlgFileForm");
    XtSetArg (al [0], XtNvalue, "");
    XtSetValues(dlgFileForm, al, 1);
    popup_dialog(dialog[DLG_FILE], XtGrabExclusive);
    break;
  case TREE_MENU_QUIT:
    exit(0);
    break;

  default:
    WARN("unknown menu item in TreeMenu_CB");
  }
}


/* ------------------------------------------------------------------------- */

#if 0

static void
LayoutMenu_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  long	menuItem = (long) client_data;
  char	tmpstr[10];
  Arg	al [2];

  switch (menuItem) {
#ifdef COMMENT
  case LAYOUT_MENU_FIXED:
    XtSetArg (al [0], XtNleftBitmap, None);
    XtSetValues(layoutMenuItems[LAYOUT_MENU_VARIABLE], al, 1);

    XtSetArg (al [0], XtNleftBitmap, check_pixmap);
    XtSetValues(layoutMenuItems[LAYOUT_MENU_FIXED], al, 1);

    if (TreeLayoutDensity != Fixed) {
      TreeLayoutDensity = Fixed;
      DeleteTree(TheTree, TRUE);
      SetupTree(TheTree);
    }
    break;
  case LAYOUT_MENU_VARIABLE:
    XtSetArg (al [0], XtNleftBitmap, check_pixmap);
    XtSetValues(layoutMenuItems[LAYOUT_MENU_VARIABLE], al, 1);

    XtSetArg (al [0], XtNleftBitmap, None);
    XtSetValues(layoutMenuItems[LAYOUT_MENU_FIXED], al, 1);

    if (TreeLayoutDensity != Variable) {
      TreeLayoutDensity = Variable;
      DeleteTree(TheTree, TRUE);
      SetupTree(TheTree);
    }
    break;
#endif /* COMMENT */
  case LAYOUT_MENU_SPACING:
    dlgLevelValue = TreeParentDistance;
    dlgSiblingValue = TreeBorderSize;
    sprintf(tmpstr, "%d  ", dlgLevelValue);
    XtSetArg (al [0], XtNlabel, tmpstr);
    XtSetValues(dlgLevelValuator, al, 1);

    /* These setvalues need some adjustments... */
    XtSetArg (al [0], XtNsliderX, dlgLevelValue * 2);
    XtSetValues(dlgLevelScale, al, 1);
    sprintf(tmpstr, "%d  ", dlgSiblingValue);
    XtSetArg (al [0], XtNlabel, tmpstr);
    XtSetValues(dlgSiblingValuator, al, 1);
    XtSetArg (al [0], XtNsliderX, dlgSiblingValue * 4);
    XtSetValues(dlgSiblingScale, al, 1);
    popup_dialog(dialog[DLG_SPACING], XtGrabNone);
    break;
  case LAYOUT_MENU_ALIGN_NODES:
    TreeAlignNodes = !TreeAlignNodes;
    XtSetArg (al [0], XtNleftBitmap, TreeAlignNodes ? check_pixmap : None);
    XtSetValues(treeMenuItems[LAYOUT_MENU_ALIGN_NODES], al, 1);
    DeleteTree(TheTree, TRUE);
    ResetLabels(TheTree);
    SetupTree(TheTree);
    break;
  default:
    WARN("unknown menu item in LayoutMenu_CB");
  }
}

#endif /* 0 */

/* ------------------------------------------------------------------------- */

static void
NodeMenu_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  long	menuItem = (long) client_data;

  switch (menuItem) {
#if 0
  case NODE_MENU_ADD_CHILD:
    NodePos = Child;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
  case NODE_MENU_ADD_BEFORE:
    NodePos = Before;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
  case NODE_MENU_ADD_AFTER:
    NodePos = After;
    popup_dialog(dialog[DLG_NODE_NAME], XtGrabExclusive);
    break;
#endif
  case NODE_MENU_ELISION:
    ExpandCollapseNode(PopupNode);
    break;
  case NODE_MENU_DELETE:
    DeleteNode(PopupNode);
    if (TheTree == NULL)
      XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
    break;
  default:
    WARN("unknown menu item in NodeMenu_CB");
  }
}


/* ------------------------------------------------------------------------- */

static void
Help_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  popup_dialog(helpShell, XtGrabNone);
}


/* ------------------------------------------------------------------------- */

static void
HelpDone_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  XtPopdown(helpShell);
}


/* ------------------------------------------------------------------------- */

static void
NodeLabel_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  XtPopdown(dialog[DLG_NODE_NAME]);
  if (client_data == (XtPointer) TRUE) {
    char *string = XawDialogGetValueString(XtParent(w));
    char *node_label = XtNewString(string);
    if (*node_label)
      InsertNode(PopupNode, NodePos, node_label);
  }
}


/* ------------------------------------------------------------------------- */

static void
NewTree_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char	*node_label;
  Tree	*tree;

  XtPopdown(dialog[DLG_NEW]);
  if (client_data == (XtPointer) TRUE) {
    node_label = XtMalloc((strlen(XawDialogGetValueString(XtParent(w))) + 1)
			  * sizeof(char));
    node_label = strcpy(node_label, XawDialogGetValueString(XtParent(w)));
    if (*node_label) {
      if (TheTree) {
	Delete(TheTree);
	XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
      }
      tree = MakeNode();
      SetNodeLabel(tree, node_label);
      PrevNode = NULL;
      SetupTree(tree);
      XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
    }
  }
  XSync(TreeDisplay, FALSE);
}


/* ------------------------------------------------------------------------- */

static void
File_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  char *fname;
  Tree *tree;
  ErrCode error;
  int menuItem;

  if (client_data == (XtPointer) TRUE) {
    fname = XawDialogGetValueString(XtParent(w));
    if (*fname) {

      if (loading_file == 1) {
	if (TheTree) {
	  PrevNode = NULL;
	  Delete(TheTree);
	  XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
	}
	tree = ReadTreeFromFile(fname, &error);
	XtPopdown(dialog[DLG_FILE]);
	if (error != ERR_NONE)
	  HandleError(error, tree);
	XSync(TreeDisplay, 0);
	if (tree) {
	  SetupTree(tree);
	  XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
	} else {
	  SetDrawingTree(NULL);
	  BeginFrame();
	  EndFrame();
	}
      } else if (menuItem == TREE_MENU_SAVE) {
	if (!SaveTreeToFile(TheTree, fname)) {
	  XtPopdown(dialog[DLG_FILE]);
	  HandleError(ERR_OPENFAIL, NULL);
	} else
	  XtPopdown(dialog[DLG_FILE]);
      }
    } else
      XtPopdown(dialog[DLG_FILE]);
  } else
    XtPopdown(dialog[DLG_FILE]);
}


/* ------------------------------------------------------------------------- */

static void
Scale_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  int level_value, sibling_value;

  XtPopdown(dialog[DLG_SPACING]);

  /* check if OK button was selected */
  if (client_data) {
    level_value = dlgLevelValue;

    sibling_value = dlgSiblingValue;

    if (level_value != TreeParentDistance ||
	sibling_value != TreeBorderSize) {

      TreeParentDistance = level_value;
      TreeBorderSize = sibling_value;
      XSync(TreeDisplay, 0);
      if (TheTree) {
	DeleteTree(TheTree, TRUE);
	PrevNode = NULL;
	if (TreeAlignNodes)
	  ResetLabels(TheTree);
	SetupTree(TheTree);
      }
    }
  }
}


/* ------------------------------------------------------------------------- */
/*									     */
/* Auxilary actions and callback functions                                   */
/*									     */
/* ------------------------------------------------------------------------- */

static void
Popdown_CB(Widget w, XtPointer client_data, XtPointer call_data)
{
  Widget widget = (Widget) client_data;

  XtPopdown(widget);
}

static void
activate_action(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  if (*num_params == 1) {
    Widget button = XtNameToWidget(XtParent(w), params[0]);

    if (button)
      XtCallCallbacks(button, XtNcallback, (XtPointer) TRUE);
  }
}

static void
quit_action(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  exit(0);
}




void
maybe_highlight_node(Tree *tree)
{
  if (tree == PrevNode) {
    Tree *tem;

    tem = tree->parent;
    tree->parent = NULL;
    HiliteNode(tree, New);
    tree->parent = tem;
  }
}

static void
select_action(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  Tree		*node;
  Boolean	edit = FALSE;

  if ((*num_params > 0) &&
      (!strcmp(params[0], "EDIT") ||
       !strcmp(params[0], "edit")))
    edit = TRUE;

  if (SearchTree(TheTree, event->xbutton.x, event->xbutton.y, &node)) {
    Tree *ParNode = NULL;

    PopupNode = node;
    if (PrevNode != NULL)
      DrawNode(PrevNode, New);
    PrevNode = PopupNode;

    printf("%s^^%s^^%s\n",
	   EnvNm,
	   edit ? "br-edit" : "br-view",
	   (PopupNode->value) ? PopupNode->value : PopupNode->label.text);

    StatusMsg(PopupNode->label.text, 1);

    ParNode = PopupNode->parent;
    PopupNode->parent = NULL;
    HiliteNode(PopupNode, New);
    PopupNode->parent = ParNode;
  }
}

static void
menu_popup_action(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  char *strargs[] = {"nodeMenu", NULL};
  /* Widget menu = XtNameToWidget(TreeTopLevel, params[0]); */
  /* Boolean popup; */
  /* ShellWidget shell_widget = (ShellWidget) menu; */

  if (nodeFound) {

    /* This is giving me a headache.
     * Why do popup menus come up unselectable?
     */

    XtCallActionProc(TreeDrawingArea,
		     "XtMenuPopup",
		     event,
		     strargs,
		     1);

/*
    XtSetSensitive(menu, TRUE);
    XtPopupSpringLoaded(menu);
 */

/*    if (!XtIsShell(menu)) {
      printf("error: not shell widget!\n");
    } else {
      if (! shell_widget->shell.popped_up) {

	XtGrabKind call_data = XtGrabExclusive;

	XtCallCallbacks(menu, XtNpopupCallback, (XtPointer) &call_data);

	shell_widget->shell.popped_up = TRUE;
	shell_widget->shell.grab_kind = XtGrabExclusive;
	shell_widget->shell.spring_loaded = TRUE;

	if (shell_widget->shell.create_popup_child_proc != NULL) {
	  (*(shell_widget->shell.create_popup_child_proc))(menu);
	}

	XtAddGrab(menu, TRUE, TRUE);

	XtRealizeWidget(menu);

	XMapRaised(XtDisplay(menu), XtWindow(menu));

      } else
	XRaiseWindow(XtDisplay(menu), XtWindow(menu));
    }
    */
  }
}


static void
button_action(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
  Tree *node;
  Arg	al [2];

  if (SearchTree(TheTree, event->xbutton.x, event->xbutton.y, &node)) {

    nodeFound = TRUE;

    XtSetSensitive(nodeMenuItems[NODE_MENU_DELETE], TRUE);

    XtSetArg (al [0], XtNlabel, node->label.text);
    XtSetValues(treeMenus[NODE_MENU], al, 1);

    PopupNode = node;
    if (IS_LEAF(node))  {

      XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION], FALSE);

      XtSetArg (al [0], XtNlabel, labelStr[STR_NODE_COLLAPSE]);
      XtSetValues(nodeMenuItems[NODE_MENU_ELISION], al, 1);

    } else {

      XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION], TRUE);
      if (node->elision) {
	XtSetArg (al [0], XtNlabel, labelStr[STR_NODE_EXPAND]);
	XtSetValues(nodeMenuItems[NODE_MENU_ELISION], al, 1);
      } else {
	XtSetArg (al [0], XtNlabel, labelStr[STR_NODE_COLLAPSE]);
	XtSetValues(nodeMenuItems[NODE_MENU_ELISION], al, 1);
      }
    }

#if 0
    if (node->parent) {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], TRUE);
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER] , TRUE);
    } else {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], FALSE);
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER] , FALSE);
    }

    if (node->elision) {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD], FALSE);
    } else {
      XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD], TRUE);
    }
#endif

  } else {
    nodeFound = FALSE;

    XtSetArg (al [0], XtNlabel, " ");
    XtSetValues(treeMenus[NODE_MENU], al, 1);

#if 0
    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_BEFORE], FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_AFTER],  FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_ADD_CHILD],  FALSE);
#endif
    XtSetSensitive(nodeMenuItems[NODE_MENU_ELISION],    FALSE);
    XtSetSensitive(nodeMenuItems[NODE_MENU_DELETE],     FALSE);
  }
}


/* ------------------------------------------------------------------------- */
/*									     */
/* General purpose popdown widget callback (for dialog popdown buttons)      */
/*									     */
/* ------------------------------------------------------------------------- */

static void center_widget(Widget w, int x, int y)
{
  Dimension	width, height, border;
  Position	max_x, max_y;
  Arg		al [3];

  XtSetArg (al [0], XtNwidth,       &width);
  XtSetArg (al [1], XtNheight,      &height);
  XtSetArg (al [2], XtNborderWidth, &border);
  XtGetValues(w, al, 3);

  width  += 2 * border;
  height += 2 * border;

  x -= ((Position) width / 2);
  y -= ((Position) height / 2);

  if (x < 0) x = 0;
  if (y < 0) y = 0;

  if (x > (max_x = (Position) (XtScreen(w)->width - width)) )
    x = max_x;

  if (y > (max_y = (Position) (XtScreen(w)->height - height)) )
    y = max_y;

  XtSetArg (al [0], XtNx, x);
  XtSetArg (al [1], XtNy, y);
  XtSetValues(w, al, 2);
}

#define shell_translations "<Message>WM_PROTOCOLS: wmpopdown()\n"

static void
popup_dialog(Widget shell, XtGrabKind grab_kind)
{
  int		idummy, x, y;
  unsigned int  uidummy;
  Window	wdummy;
  Atom		wm_delete_window;

  XtOverrideTranslations(shell, XtParseTranslationTable(shell_translations));
  XtRealizeWidget(shell);
  wm_delete_window = XInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW",
				 FALSE);
  (void) XSetWMProtocols(XtDisplay(shell), XtWindow(shell),
			 &wm_delete_window, 1);

  XQueryPointer(TreeDisplay, DefaultRootWindow(TreeDisplay),
		&wdummy, &wdummy,
		&x, &y, &idummy, &idummy, &uidummy);

  center_widget(shell, x, y);

  XtPopup(shell, grab_kind);
}

static void
popdown_action(Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
  XtPopdown(widget);
}

static void
set_siblingscale_CB(Widget widget, XtPointer w, XtPointer percent_ptr)
{
  char tmpstr[10];
  Arg al [1];

  sprintf(tmpstr, "%d  ", (int) (*(float*) percent_ptr*MAX_BORDER_SIZE));

  XtSetArg (al [0], XtNlabel, tmpstr);
  XtSetValues((Widget) w, al, 1);
}

static void
siblingscale_CB(Widget widget, XtPointer w, XtPointer call_data)
{
  char tmpstr[10];
  XawPannerReport *report = (XawPannerReport *) call_data;
  Arg al [1];

  sprintf(tmpstr, "%d ", (int)
	  ((float) report->slider_x / report->slider_width
	   * MAX_BORDER_SIZE / 10));

  XtSetArg (al [0], XtNlabel, tmpstr);
  XtSetValues((Widget) w, al, 1);
}

static void
set_levelscale_CB(Widget widget, XtPointer w, XtPointer percent_ptr)
{
  char	tmpstr[10];
  Arg al [1];

  sprintf(tmpstr, "%d  ", (int) (*(float*) percent_ptr*MAX_PARENT_DISTANCE));

  XtSetArg (al [0], XtNlabel, tmpstr);
  XtSetValues((Widget) w, al, 1);
}

static void
levelscale_CB(Widget widget, XtPointer w, XtPointer call_data)
{
  char	tmpstr[10];
  XawPannerReport *report = (XawPannerReport *) call_data;
  Arg al [1];

  sprintf(tmpstr, "%d ", (int)
	  ((float) report->slider_x / report->slider_width
	   * MAX_PARENT_DISTANCE / 10));

  XtSetArg (al [0], XtNlabel, tmpstr);
  XtSetValues((Widget) w, al, 1);
}

static void
set_siblingvalue_CB(Widget widget, XtPointer var_ptr, XtPointer percent_ptr)
{
  * (int *)var_ptr = (int) (*(float*) percent_ptr*MAX_BORDER_SIZE);
}

static void
set_levelvalue_CB(Widget widget, XtPointer var_ptr, XtPointer percent_ptr)
{
  * (int *)var_ptr = (int) (*(float*) percent_ptr*MAX_PARENT_DISTANCE);
}


static void
siblingvalue_CB(Widget widget, XtPointer var_ptr, XtPointer call_data)
{
  XawPannerReport *report = (XawPannerReport *) call_data;

  * (int *) var_ptr = (int)
    ((float) report->slider_x / report->slider_width
     * MAX_BORDER_SIZE / 10);
}

static void
levelvalue_CB(Widget widget, XtPointer var_ptr, XtPointer call_data)
{
  XawPannerReport *report = (XawPannerReport *) call_data;

  * (int *) var_ptr = (int)
    ((float) report->slider_x / report->slider_width
     * MAX_PARENT_DISTANCE / 10);
}


/* ----------------------------------------------------------------------------
 *
 *   Main routine
 *
 * ----------------------------------------------------------------------------
 */

int
main(int argc, char *argv[])
{
  Tree *tree = NULL;
  ErrCode error = ERR_NONE;

  ProgramName = strdup(argv[0]);
  if (ProgramName == NULL) {
    fprintf(stderr, "%s: insufficient memory available\n", argv[0]);
    exit(0);
  }

  InitializeInterface(&argc, argv);
  XSync(TreeDisplay, 0);

  if (argc >= 2) {
    tree = ReadTreeFromFile(argv[1], &error);
    if (tree) StatusMsg(argv[1], TRUE);
    if (argc > 2)
      WARN("extraneous arguments ignored");
  }

  if (tree) {
    SetupTree(tree);
    XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], TRUE);
  } else {
    SetDrawingTree(NULL);
    XtSetSensitive(treeMenuItems[TREE_MENU_SAVE], FALSE);
  }

  if (error != ERR_NONE)
    HandleError(error, tree);

  StatusMsg("Left = Edit Node; Mid = View Node; Right = Node Menu", 1);
  UserEventLoop();

  return 0;
}

/* ------------------------------------------------------------------------- */
/*				  End of File                                */
/* ------------------------------------------------------------------------- */
