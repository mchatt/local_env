#include <windows.h>
#include "dbl.h"
#include "intf.h"

static DoubleBuffer db;

DoubleBuffer*
DBLcreate_double_buffer (void)
{
  db.hdc = GetDC (hwndClient);
  db.colors[BACKGROUND_COLOR] = RGB (255, 255, 255);
  db.colors[TREE_COLOR] = RGB (0, 0, 0);
  db.colors[CONTOUR_COLOR] = RGB (0, 192, 0);
  db.colors[HIGHLIGHT_COLOR] = RGB (255, 255, 255);
  db.colors[HIGHLIGHT_BK_COLOR] = RGB (0, 0, 128);
  db.colors[SPLIT_COLOR] = RGB (192, 192, 0);
  db.colors[ACTION_COLOR] = RGB (0, 192, 192);

  db.DrawingWidth = 0;
  db.DrawingHeight = 0;

  SetTextAlign (db.hdc, TA_TOP | TA_LEFT);

  return &db;
}
