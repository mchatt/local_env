#include <windows.h>

#include <commctrl.h>

#include <commdlg.h>

#include <stdlib.h>



#include "resource.h"

#include "dbl.h"

#include "intf.h"

#include "tree.h"

#include "input.h"





/*

 * Globals 

 */



char         *ProgramName = "oobr";



int           TreeContourWidth = 1;

int           TreeBorderSize = BORDER_SIZE;

int           TreeParentDistance = PARENT_DISTANCE;

DoubleBuffer *TreeDrawingAreaDB;

ContourOption TreeShowContourOption = NoContours;

DensityOption TreeLayoutDensity = Fixed;

char          TreeShowSteps =  FALSE;

char          TreeAlignNodes = FALSE;

char          PauseAfterStep    = FALSE;





HWND hwndFrame, hwndClient;



static const char* SZ_FRAME_CLASS = "OObrFrameClass";

static const char* SZ_CLIENT_CLASS = "OObrClientClass";

static HWND hwndStatus;

static HINSTANCE hAppInst;



static BOOL FileModified = FALSE;

static char CurrentFile [MAX_PATH];

static char NextFile [MAX_PATH];

static char NewNodeName [MAX_NODE_NAME_LENGHT];

static char *FileTitle = NULL;



/* The semantics for these is different than in X. 

   Selected node is the one selected with left click.

   Popup node is the tempoparily highlighted one, by a

   right click, only while a popup menu is active */

static Tree *SelectedNode;

static Tree *PopupNode;



/*

 * Prototypes

 */

BOOL LoadFile (void);

BOOL SaveFileAs (void);

BOOL SaveFile (void);

void UpdateFrameTitle (void);

BOOL PromptFileName (BOOL bSave);

UINT FileMessage (UINT idstr, char* file, UINT flags);



/*

 * ------------------------------------

 * File handling

 * ------------------------------------

 */



static void

SetupInterface ()

{

  UpdateFrameTitle ();



  PopupNode = NULL;

  SelectedNode = NULL;

  SetWindowOrgEx (TreeDrawingAreaDB->hdc, 0, 0, NULL);



  InvalidateRect (hwndClient, NULL, TRUE);

}



static BOOL

NewFile (void)

{

  CurrentFile[0] = '\0';

  FileTitle = NULL;

  SetupInterface ();

  FileModified = TRUE;



  if (TheTree)

    Delete (TheTree);

  TheTree = MakeNode();

  SetNodeLabel(TheTree, strdup (NewNodeName));

  SetupTree(TheTree);



  return TRUE;

}



static BOOL

LoadFile (void)

{

  Tree* newTree;

  ErrCode error;



  /* Save file under the name in NextFile,

     as set by PromptFileName */

  newTree = ReadTreeFromFile (NextFile, &error);

  

  if (error != ERR_NONE)

    {

      if (newTree)

	Delete (newTree);

      FileMessage (IDS_ERRORFIRST + error, NextFile,

		   MB_OK | MB_ICONEXCLAMATION);

      return FALSE;

    }



  strcpy (CurrentFile, NextFile);

  SetupInterface ();

  FileModified = FALSE;



  if (TheTree)

    Delete (TheTree);

  TheTree = newTree;

  SetupTree (newTree);



  return TRUE;

}



static BOOL

SaveFileAs (void)

{

  if (!PromptFileName (TRUE))

    return FALSE;



  /* Save file under the name in NextFile,

     as set by PromptFileName */

  if (!SaveTreeToFile (TheTree, NextFile))

    {

      FileMessage (IDS_ERRORFIRST + ERR_OPENFAIL, NextFile,

		   MB_OK | MB_ICONEXCLAMATION);

      return FALSE;

    }



  strcpy (CurrentFile, NextFile);

  UpdateFrameTitle ();

  FileModified = FALSE;

}



static BOOL

SaveFile (void)

{

  /* Always ask for filename if currently untitled */

  if (CurrentFile[0] == 0)

    return SaveFileAs ();



  if (!SaveTreeToFile (TheTree, CurrentFile))

    {

      FileMessage (IDS_ERRORFIRST + ERR_OPENFAIL, CurrentFile,

		   MB_OK | MB_ICONEXCLAMATION);

      return FALSE;

    }



  FileModified = FALSE;

  return TRUE;

}



/* 

 * Set frame title to either "FILE : OObrowser" or "OObrowser",

 * depending on whether a file is open

 */

static void

UpdateFrameTitle (void)

{

  char buf [MAX_PATH + 4], *p;

  

  if (FileTitle == NULL || FileTitle[0] == '\0')

    LoadString (hAppInst, IDS_UNTITLED, buf, sizeof(buf));

  else

    strcpy (buf, FileTitle);



  strcat (buf, " - ");

  p = buf + strlen (buf);



  LoadString (hAppInst, IDS_APPTITLE, p,

	      sizeof (buf) - (p - buf));

  SetWindowText (hwndFrame, buf);

}



/*

 *

 */

static UINT

FileMessage (UINT idstr, char* file, UINT flags)

{

  char buf [MAX_PATH + 128], format [128], title[64];



  LoadString (hAppInst, IDS_APPTITLE, title, sizeof(title));

  LoadString (hAppInst, idstr, format, sizeof(format));

  wsprintf (buf, format, file);



  return MessageBox (hwndFrame, buf, title, flags);

}



/*

 * Propmt for file name for save or load. Return TRUE if user has

 * chosen a name, FALSE if canceled. If name is chosen, it is

 * stored into NextFile, and its title part into FileTitle

 */

static BOOL

PromptFileName (BOOL bSave)

{

  OPENFILENAME ofn;

  BOOL ok;



  if (bSave)

    {

      /* Default to current file name */

      strcpy (NextFile, CurrentFile);

    }

  else

    {

      /* Start with empty file name */

      *NextFile = 0;

    }



  ofn.lStructSize = sizeof (ofn);

  ofn.hwndOwner = hwndFrame;

  ofn.lpstrFilter = NULL; /* #### */

  ofn.lpstrCustomFilter = NULL;

  ofn.lpstrFile = NextFile;

  ofn.nMaxFile = sizeof (NextFile);

  ofn.lpstrFileTitle = NULL;

  ofn.lpstrInitialDir = NULL;

  ofn.lpstrTitle = NULL;

  ofn.Flags = (bSave

	       ? OFN_OVERWRITEPROMPT | OFN_NOTESTFILECREATE

	       : OFN_FILEMUSTEXIST);

  ofn.lpstrDefExt = NULL;

  ofn.lCustData = 0;

  ofn.lpfnHook = NULL;

  ofn.lpTemplateName = NULL;



  ok = (bSave

	? GetSaveFileName (&ofn)

	: GetOpenFileName (&ofn));

  if (ok)

    FileTitle = NextFile + ofn.nFileOffset;



  return ok;

}



/*

 * Dialog procedure for the node name editing  dialog

 */

static BOOL CALLBACK

DlgNodeNameProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)

{

  switch (uMsg)

    {

    case WM_INITDIALOG:

      /* lParam is TRUE for new root node when creating the tree,

	 FALSE for node name editing. We show different caption

	 based on this flag. */

      {

	char buf [128];

	LoadString (hAppInst, lParam ? IDS_ROOTNODE : IDS_MERENODE,

		    buf, sizeof (buf));

	SetWindowText (hwnd, buf);

	SetDlgItemText (hwnd, IDC_NAME, NewNodeName);

      }

      break;



    case WM_COMMAND:

      switch (LOWORD(wParam))

	{

	case IDOK:

	  GetDlgItemText (hwnd, IDC_NAME, NewNodeName, sizeof(NewNodeName));

	  EndDialog (hwnd, TRUE);

	  break;



	case IDCANCEL:

	  EndDialog (hwnd, FALSE);

	  break;

	}

      break;



    default:

      return FALSE;

    }

  return TRUE;

}



/*

 * Dialog procedure for the spacing dialog

 */

static BOOL CALLBACK

DlgSpacingProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)

{

  switch (uMsg)

    {

    case WM_INITDIALOG:

      SendDlgItemMessage (hwnd, IDC_LEVEL_SL, TBM_SETRANGE, FALSE, MAKELONG (10, 50));

      SendDlgItemMessage (hwnd, IDC_LEVEL_SL, TBM_SETPOS, TRUE, TreeParentDistance);

      SetDlgItemInt (hwnd, IDC_LEVEL_ED, TreeParentDistance, FALSE);



      SendDlgItemMessage (hwnd, IDC_SIBLING_SL, TBM_SETRANGE, FALSE, MAKELONG (1, 15));

      SendDlgItemMessage (hwnd, IDC_SIBLING_SL, TBM_SETPOS, TRUE, TreeBorderSize);

      SetDlgItemInt (hwnd, IDC_SIBLING_ED, TreeBorderSize, FALSE);

      break;



    case WM_COMMAND:

      switch (LOWORD(wParam))

	{

	case IDOK:

	  {

	    int NewLevel = SendDlgItemMessage (hwnd, IDC_LEVEL_SL, TBM_GETPOS, 0, 0);

	    int NewSibling = SendDlgItemMessage (hwnd, IDC_SIBLING_SL, TBM_GETPOS, 0, 0);

	    if (NewLevel != TreeParentDistance

		|| NewSibling != TreeBorderSize)

	      {

	        TreeParentDistance = NewLevel;

	        TreeBorderSize = NewSibling;

	        if (TheTree)

		  {

    		    DeleteTree(TheTree, TRUE);

		    if (TreeAlignNodes)

		      ResetLabels(TheTree);

		    SetupTree(TheTree);

		    InvalidateRect (hwndClient, NULL, TRUE);

  		  }

	      }

	  }

	  EndDialog (hwnd, TRUE);

	  break;



	case IDCANCEL:

	  EndDialog (hwnd, FALSE);

	  break;

	}

      break;



    case WM_HSCROLL:

      SetDlgItemInt (hwnd, IDC_LEVEL_ED,

	SendDlgItemMessage (hwnd, IDC_LEVEL_SL, TBM_GETPOS, 0, 0), FALSE);

      SetDlgItemInt (hwnd, IDC_SIBLING_ED,

	SendDlgItemMessage (hwnd, IDC_SIBLING_SL, TBM_GETPOS, 0, 0), FALSE);

      break;



    default:

      return FALSE;

    }

  return TRUE;

}

   

/*

 * Dialog procedure for the about dialog

 */

static BOOL CALLBACK

DlgAboutProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)

{

  if (uMsg == WM_COMMAND && LOWORD(wParam) == IDOK)

    {

      EndDialog (hwnd, 0);

      return TRUE;

    }

  return FALSE;

}



/*

 * Edit node name given in NAME. Returns pointer to the

 * new name static buffer, or NULL if canceled. The ROOT

 * parameter affects dialog title only

 */

char*

DlgNodeName (const char* name, BOOL root)

{

  if (name)

    strcpy (NewNodeName, name);

  else

    *NewNodeName = 0;



  return (DialogBoxParam (hAppInst, MAKEINTRESOURCE (IDD_NODENAME), 

			  hwndFrame, DlgNodeNameProc, root)

	  ? NewNodeName : NULL);

}



static BOOL

PromptRootNode (int u_n_u_s_e_d)

{

  return DlgNodeName (NULL, TRUE) != 0;

}



static BOOL

PromptAndSave (BOOL (*proc)(int), int param)

{

  if (proc && !proc (param))

    return FALSE;



  if (FileModified)

    {

      char buf[80], *p;

      UINT uRes;



      /* Substitute "Untitled" if no file */

      if (CurrentFile[0])

	p = CurrentFile;

      else

	{

	  LoadString (hAppInst, IDS_UNTITLED, buf, sizeof(buf));

	  p = buf;

	}



      uRes = FileMessage (IDS_SAVECHANGES, p, MB_YESNOCANCEL | MB_ICONQUESTION);

      if (uRes == IDCANCEL)

	return FALSE;

      if (uRes == IDNO)

	return TRUE;



      return SaveFile ();

    }

  else

    return TRUE;

}



static void

MaybeNewTreeNode (Tree* node, NodePosition pos)

{

  char *newName = DlgNodeName (NULL, FALSE);

  if (newName)

    {

      InsertNode (node, pos, strdup (newName));

      InvalidateRect (hwndClient, NULL, TRUE);

      FileModified = TRUE;

    }

}



static void

SelectNode (Tree* node, BOOL edit)

{    

  /* Redraw old node in un-selected state */

  if (SelectedNode != NULL)

    {

      SelectedNode->highlight = FALSE;

      SelectedNode->focus = FALSE;

      DrawNode (SelectedNode, New);

    }



  SelectedNode = node;



  if (node)

    {

    

      printf ("%s^^%s^^%s\n", 

	      CurrentFile,

	      edit ? "br-edit" : "br-view",

	      (SelectedNode->value) ? SelectedNode->value : SelectedNode->label.text);



      StatusMsg (SelectedNode->label.text, 1);

    

      /* Redraw new node in selected state */

      SelectedNode->highlight = TRUE;

      SelectedNode->focus = TRUE;

      DrawNode(SelectedNode, New);

    }

  else

    StatusMsg ("", 1);

}



static void

ChangeFontMaybe (void)

{

  CHOOSEFONT cf;

  LOGFONT lf;



  cf.lStructSize = sizeof (cf);

  cf.hwndOwner = hwndFrame;

  cf.lpLogFont = &lf;

  cf.Flags = (CF_FORCEFONTEXIST | CF_NOSIMULATIONS | CF_NOVECTORFONTS

	      | CF_SELECTSCRIPT | CF_NOVERTFONTS | CF_SCREENFONTS);



  if (ChooseFont (&cf) && SetNewFont (&lf))

    {

      DeleteTree(TheTree, TRUE);

      ResetLabels(TheTree);

      SetupTree(TheTree);

      InvalidateRect (hwndClient, NULL, TRUE);

    }

}





/*

 * Menu command handlers

 */

static void

HandleClientMenuCommand (WORD wId)

{

  switch (wId)

    {

    case ID_EDIT_ADDCHILD:

      MaybeNewTreeNode (PopupNode, Child);

      break;



    case ID_EDIT_ADDSIBLINGBEFORE:

      MaybeNewTreeNode (PopupNode, Before);

      break;



    case ID_EDIT_ADDSIBLIBNGAFTER:

      MaybeNewTreeNode (PopupNode, After);

      break;



    case ID_EDIT_DELETE:

      SelectNode(NULL, FALSE); /* deselect node before deleting it! */

      DeleteNode(PopupNode);

      InvalidateRect (hwndClient, NULL, TRUE);

      FileModified = TRUE;

      break;



    case ID_VIEW_COLLAPSE:

    case ID_VIEW_EXPAND:

      ExpandCollapseNode(PopupNode);

      InvalidateRect (hwndClient, NULL, TRUE);

      break;



    case ID_VIEW_SPACING:

      DialogBox (hAppInst, MAKEINTRESOURCE (IDD_SPACING), hwndFrame, DlgSpacingProc);

      break;



    case ID_VIEW_FONT:

      ChangeFontMaybe ();

      break;



    case ID_VIEW_ALIGNLEVEL:

      TreeAlignNodes = !TreeAlignNodes;

      DeleteTree(TheTree, TRUE);

      ResetLabels(TheTree);

      SetupTree(TheTree);

      InvalidateRect (hwndClient, NULL, TRUE);

      break;



    case ID_POPUP_EDIT:

      SelectNode (PopupNode, TRUE);

      break;



    case ID_POPUP_VIEW:

      SelectNode (PopupNode, FALSE);

      break;

    }

}



static BOOL

HandleFrameMenuCommand (WORD wId)

{

  switch (wId)

    {

    case ID_FILE_NEW:

      if (PromptAndSave (PromptRootNode, 0))

	NewFile ();

      break;



    case ID_FILE_OPEN:

      if (PromptAndSave (PromptFileName, FALSE))

	LoadFile ();

      break;



    case ID_FILE_SAVE:

      SaveFile ();

      break;



    case ID_FILE_SAVEAS:

      SaveFileAs ();

      break;



    case ID_FILE_EXIT:

      if (PromptAndSave (NULL, 0))

	DestroyWindow (hwndFrame);

      break;



    case ID_HELP_CONTENTS:

      WinHelp (hwndFrame, "oobr.hlp", HELP_CONTENTS, 0);

      break;



    case ID_HELP_ABOUT:

      DialogBox (hAppInst, MAKEINTRESOURCE (IDD_ABOUT), hwndFrame, DlgAboutProc);

      break;



    default:

      return FALSE;

    }

  return TRUE;

}



static void

HandleScroll (HWND hwnd, int dir, int op, int newpos)

{

  SCROLLINFO si;

  POINT pt;

  int oldpos, maxpos;



  /* Get scroll metrics */

  si.cbSize = sizeof (si);

  si.fMask = SIF_ALL;

  GetScrollInfo (hwnd, dir, &si);



  /* Determine old position */

  GetWindowOrgEx (TreeDrawingAreaDB->hdc, &pt);

  oldpos = dir == SB_VERT ? pt.y : pt.x;

  maxpos = si.nMax - si.nPage - 1;



  switch (op)

    {

    case SB_TOP:

      newpos = 0;

      break;

    case SB_BOTTOM:

      newpos = maxpos;

      break;

    case SB_PAGEUP:

      newpos = oldpos - 3 * si.nPage / 4;

      break;

    case SB_PAGEDOWN:

      newpos = oldpos + 3 * si.nPage / 4;

      break;

    case SB_LINEUP:

      newpos = oldpos - 16;

      break;

    case SB_LINEDOWN:

      newpos = oldpos + 16;

      break;

    case SB_ENDSCROLL:

      return;

    }

  

  newpos = max (newpos, 0);

  newpos = min (newpos, maxpos);



  if (newpos == oldpos)

    return;



  *(dir == SB_VERT ? &pt.y : &pt.x) = newpos;

  SetWindowOrgEx (TreeDrawingAreaDB->hdc, pt.x, pt.y, NULL);

  ScrollWindow (hwnd,

	        dir == SB_VERT ? 0 : oldpos - newpos,

		dir != SB_VERT ? 0 : oldpos - newpos,

		NULL, NULL);

  SetScrollPos (hwnd, dir, newpos, TRUE);

}



static POINT

ToLP (LPARAM lParam)

{

  POINT pt;

  pt.x = LOWORD (lParam);

  pt.y = HIWORD (lParam);

  DPtoLP (TreeDrawingAreaDB->hdc, &pt, 1);

  return pt;

}



static void

HandleLButton (POINT pt, BOOL edit)

{

  Tree *node;

  if (SearchTree(TheTree, pt.x, pt.y, &node))

    SelectNode (node, edit);

  else

    SelectNode (NULL, edit);

}



static void

HandleRButton (POINT ptLP, POINT ptClick)

{

  Tree *node;

  if (SearchTree(TheTree, ptLP.x, ptLP.y, &node))

    {

      HMENU hMenu, hPopup;

    

      PopupNode = node;



      if (SelectedNode != PopupNode)

	{

	  PopupNode->highlight = TRUE;

	  DrawNode(PopupNode, New);

	  if (SelectedNode)

	    {

	      SelectedNode->highlight = FALSE;

	      DrawNode(SelectedNode, New);

	    }

	}



      hMenu = LoadMenu (hAppInst, MAKEINTRESOURCE (IDR_NODEPOPUP));

      hPopup = GetSubMenu (hMenu, 0);



      TrackPopupMenu (hPopup, TPM_LEFTBUTTON | TPM_RIGHTBUTTON,

		      ptClick.x, ptClick.y, 0, hwndClient, NULL);



      DestroyMenu (hMenu);



      if (SelectedNode != PopupNode)

	{

	  PopupNode->highlight = FALSE;

	  DrawNode(PopupNode, New);

	  if (SelectedNode)

	    {

	      SelectedNode->highlight = TRUE;

	      DrawNode(SelectedNode, New);

	    }

	}

    }

}



static void

HandleMenuPopup (HMENU hMenu)

{

#define FROB(item,cond) EnableMenuItem (hMenu, (item), MF_BYCOMMAND | ((cond) ? MF_GRAYED : 0));



  FROB (ID_FILE_SAVE, TheTree == NULL);

  FROB (ID_FILE_SAVEAS, TheTree == NULL);

  FROB (ID_VIEW_EXPAND, PopupNode == NULL || !PopupNode->elision || IS_LEAF(PopupNode));

  FROB (ID_VIEW_COLLAPSE, PopupNode == NULL || PopupNode->elision || IS_LEAF(PopupNode));

  FROB (ID_FILE_SAVEAS, TheTree == NULL);

  FROB (ID_VIEW_ALIGNLEVEL, TheTree == NULL);

 

#undef FROB

  

  CheckMenuItem (hMenu, ID_VIEW_ALIGNLEVEL, MF_BYCOMMAND | (TreeAlignNodes ? MF_CHECKED : 0));

}



/*

 * Client window procedure

 */

static LRESULT CALLBACK

ClientWndProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)

{

  switch (uMsg)

    {

    case WM_CREATE:

      /* We really need that very early. Scrollbar setup depends on this */

      hwndClient = hwnd;

      TreeDrawingAreaDB = DBLcreate_double_buffer ();

      break;



    case WM_PAINT:

      {

	PAINTSTRUCT ps;

	BeginPaint (hwnd, &ps);

	ExposeHandler ();

	EndPaint (hwnd, &ps);

      }

      break;



    case WM_SIZE:

      if (TreeDrawingAreaDB)

	{

	  int w, h;

	  GetDrawingSize (&w, &h);

	  SetDrawingSize (w, h);

	}

      break;



    case WM_MENUSELECT:

      {

	int zero = 0;

	MenuHelp (uMsg, wParam, lParam, NULL, hAppInst, hwndStatus, &zero);

      }

      break;



    case WM_INITMENUPOPUP:

      HandleMenuPopup ((HMENU) wParam);

      break;



    case WM_VSCROLL:

      HandleScroll (hwnd, SB_VERT, LOWORD(wParam), HIWORD(wParam));

      break;



    case WM_HSCROLL:

      HandleScroll (hwnd, SB_HORZ, LOWORD(wParam), HIWORD(wParam));

      break;



    case WM_LBUTTONDOWN:

      /* View node command */

      HandleLButton (ToLP (lParam), FALSE);

      break;



    case WM_LBUTTONDBLCLK:

      /* Edit node command */

      HandleLButton (ToLP (lParam), TRUE);

      break;



    case WM_RBUTTONDOWN:

      /* Pop up the menu */

      {

	POINT ptClick;

	ptClick.x = LOWORD (lParam);

	ptClick.y = HIWORD (lParam);

	ClientToScreen (hwnd, &ptClick);

        HandleRButton (ToLP (lParam), ptClick);

      }

      break;



    case WM_COMMAND:

      HandleClientMenuCommand (LOWORD(wParam));

      break;



    default:

      return DefWindowProc (hwnd, uMsg, wParam, lParam);

    }

  return 0;

}



/*

 * Frame window procedure

 */

static LRESULT CALLBACK

FrameWndProc (HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)

{

  switch (uMsg)

    {

    case WM_CREATE:

      /* Create status bar */

      hwndStatus = CreateStatusWindow (SBARS_SIZEGRIP | WS_CHILD | WS_VISIBLE,

				       NULL, hwnd, 0);

      /* Create client window */

      hwndClient = CreateWindowEx (WS_EX_CLIENTEDGE, SZ_CLIENT_CLASS, NULL,

				   WS_CHILD | WS_VSCROLL | WS_HSCROLL | WS_VISIBLE,

				   0, 0, 10, 10, /* Adjusted in WM_SIZE handler */

  				   hwnd, NULL, hAppInst, NULL);

      break;



    case WM_DESTROY:

      WinHelp (hwndFrame, "oobr.hlp", HELP_QUIT, 0);

      PostQuitMessage (0);

      break;



    case WM_CLOSE:

      HandleFrameMenuCommand (ID_FILE_EXIT);

      break;



    case WM_SIZE:

      /* Adjust status bar */

      SendMessage (hwndStatus, uMsg, wParam, lParam);

      /* Adjust client rectangle */

      {

	RECT rc;

	int delta;

	GetClientRect (hwndStatus, &rc);

	delta = rc.bottom - rc.top;

	GetClientRect (hwnd, &rc);

	rc.bottom -= delta;

	MoveWindow (hwndClient, rc.left, rc.top,

		    rc.right - rc.left, rc.bottom - rc.top, TRUE);

      }

      break;



    case WM_MENUSELECT:

      {

	int zero = 0;

	MenuHelp (uMsg, wParam, lParam, NULL, hAppInst, hwndStatus, &zero);

      }

      break;



    case WM_INITMENUPOPUP:

      HandleMenuPopup ((HMENU) wParam);

      break;



    case WM_COMMAND:

      /* Route unhandled command to the client */

      if (!HandleFrameMenuCommand (LOWORD(wParam)))

	SendMessage (hwndClient, uMsg, wParam, lParam);

      break;



    case WM_SETFOCUS:

      SetFocus (hwndClient);

      break;



    default:

      return DefWindowProc (hwnd, uMsg, wParam, lParam);

    }

  return 0;

}



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

    SendMessage (hwndStatus, SB_SETTEXT, 0, (LPARAM)msg);

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

Pause()

{

}



#ifdef WINDOWED

int WINAPI

WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow)

{

  hAppInst = hInstance;

#else

int

main (int argc, char* argv[])

{

  LPTSTR lpCmdLine = argc > 1 ? argv[1] : NULL;

  int nCmdShow = SW_SHOWNORMAL;

  hAppInst = GetModuleHandle (NULL);

#endif



#ifdef _DEBUG

  _CrtSetDbgFlag (_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

#endif



  InitCommonControls ();



  /* Register window classes */

  {

    WNDCLASS wcl;



    /* Frame window class */

    wcl.style = 0;

    wcl.lpfnWndProc = FrameWndProc;

    wcl.cbClsExtra = 0;

    wcl.cbWndExtra = 0;

    wcl.hInstance = hAppInst;

    wcl.hIcon = LoadIcon (hAppInst, MAKEINTRESOURCE (IDI_OOBR));

    wcl.hCursor = LoadCursor (NULL, IDC_ARROW);

    wcl.hbrBackground = NULL;

    wcl.lpszMenuName = MAKEINTRESOURCE (IDR_MAINMENU);

    wcl.lpszClassName = SZ_FRAME_CLASS;

    RegisterClass (&wcl);



    /* Client window class */

    wcl.style = CS_OWNDC | CS_DBLCLKS;

    wcl.lpfnWndProc = ClientWndProc;

    wcl.cbClsExtra = 0;

    wcl.cbWndExtra = 0;

    wcl.hInstance = hAppInst;

    wcl.hIcon = NULL;

    wcl.hCursor = LoadCursor (NULL, IDC_ARROW);

    wcl.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);

    wcl.lpszMenuName = NULL;

    wcl.lpszClassName = SZ_CLIENT_CLASS;

    RegisterClass (&wcl);

  }



  /* Create frame */

  hwndFrame = CreateWindowEx (0, SZ_FRAME_CLASS, NULL, WS_OVERLAPPEDWINDOW,

			      CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,

			      NULL, NULL, hAppInst, NULL);



  /* Show it */

  ShowWindow (hwndFrame, nCmdShow);



  if (lpCmdLine && lpCmdLine[0])

    {

      strcpy (NextFile, lpCmdLine);

      if (LoadFile ())

	FileTitle = strrchr (CurrentFile, '/');

	if (FileTitle == NULL)

	  FileTitle = strrchr (CurrentFile, '\\');

	if (FileTitle == NULL)

	  FileTitle = CurrentFile;

    }



  UpdateFrameTitle ();



  /* Pump messages */

  {

    MSG msg;

    while (GetMessage (&msg, 0, 0, 0))

      {

	TranslateMessage (&msg);

	DispatchMessage (&msg);

      }

  }



  if (TheTree)

    Delete (TheTree);



  return 0;

}

