module Graphics.Win32.DesktopWindowManager where

import System.Win32.Types 
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.GDI.Types (RECT, peekRECT, sizeofRECT, allocaRECT)
import Graphics.Win32.Message (WindowMessage)
import Foreign
import Foreign.C.Types (CIntPtr(..))

#include <Windows.h>
#include <dwmapi.h>

type DWMAPI = HRESULT

foreign import ccall unsafe "DwmExtendFrameIntoClientArea"
  dwmExtendFrameIntoClientArea
  :: HWND -> Ptr MARGINS -> IO DWMAPI

foreign import ccall "DwmDefWindowProc"
  c_DwmDefWindowProc
  :: HWND -> WindowMessage -> WPARAM -> LPARAM -> Ptr LRESULT -> IO BOOL

dwmDefWindowProc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO (Maybe LRESULT)
dwmDefWindowProc hwnd wmsg wparam lparam =
  alloca $ \p -> do
    r <- c_DwmDefWindowProc hwnd wmsg wparam lparam p
    if r 
      then Just <$> peek p
      else return Nothing

foreign import ccall "DwmGetWindowAttribute"
  c_DwmGetWindowAttribute :: HWND -> DwmAttribute -> LPVOID -> DWORD -> IO DWMAPI

data MARGINS = Margins { mLeft::INT, mRight::INT, mTop::INT, mBottom::INT}
  deriving (Show) 

defMARGINS :: MARGINS
defMARGINS = Margins 0 0 0 0

instance Storable MARGINS where
 sizeOf _ = #{size MARGINS}
 alignment _ = #{alignment MARGINS}
 peek p = do
   mLeft' <- #{peek MARGINS,cxLeftWidth} p
   mRight' <- #{peek MARGINS,cxRightWidth} p
   mTop' <- #{peek MARGINS,cyTopHeight} p
   mBottom' <- #{peek MARGINS,cyBottomHeight} p
   return Margins { mLeft = mLeft', mRight = mRight', mTop = mTop', mBottom = mBottom' }
 poke p m = do
   #{poke MARGINS,cxLeftWidth} p $ mLeft m
   #{poke MARGINS,cxRightWidth} p $ mRight m
   #{poke MARGINS,cyTopHeight} p $ mTop m
   #{poke MARGINS,cyBottomHeight} p $ mBottom m

-- | Note the following exerpt from the MSDN documentation, which is relevant:
-- "Retrieves the bounds of the caption button area in window-relative space. [..]
-- If the window is minimized or otherwise not visible to the user, then the value
-- of the RECT is undefined. You should check whether the retrieved RECT contains
-- a boundary that you can work with, and if it doesn't then you can conclude that
-- the window is minimized or otherwise not visible to the user"
dwmGetCaptionButtonBounds :: HWND -> IO RECT
dwmGetCaptionButtonBounds hwnd = 
  allocaRECT $ \p_rect -> do
    failIf_ (/= 0) "DwmGetWindowAttribute dWMWA_CAPTION_BUTTON_BOUNDS" 
      $ c_DwmGetWindowAttribute hwnd dWMWA_CAPTION_BUTTON_BOUNDS (castPtr p_rect) (fromIntegral sizeofRECT)
    
    peekRECT p_rect 

newtype DwmAttribute = DwmAttribute DWORD


#{enum DwmAttribute, DwmAttribute
  , dWMWA_NCRENDERING_ENABLED = DWMWA_NCRENDERING_ENABLED
  , dWMWA_NCRENDERING_POLICY = DWMWA_NCRENDERING_POLICY
  , dWMWA_TRANSITIONS_FORCEDISABLED = DWMWA_TRANSITIONS_FORCEDISABLED
  , dWMWA_ALLOW_NCPAINT = DWMWA_ALLOW_NCPAINT
  , dWMWA_CAPTION_BUTTON_BOUNDS = DWMWA_CAPTION_BUTTON_BOUNDS
  , dWMWA_NONCLIENT_RTL_LAYOUT = DWMWA_NONCLIENT_RTL_LAYOUT
  , dWMWA_FORCE_ICONIC_REPRESENTATION = DWMWA_FORCE_ICONIC_REPRESENTATION
  , dWMWA_FLIP = DWMWA_FLIP3D_POLICY
  , dWMWA_EXTENDED_FRAME_BOUNDS = DWMWA_EXTENDED_FRAME_BOUNDS
  , dWMWA_HAS_ICONIC_BITMAP = DWMWA_HAS_ICONIC_BITMAP
  , dWMWA_DISALLOW_PEEK = DWMWA_DISALLOW_PEEK
  , dWMWA_EXCLUDED_FROM_PEEK = DWMWA_EXCLUDED_FROM_PEEK
  , dWMWA_CLOAK = DWMWA_CLOAK
  , dWMWA_CLOAKED = DWMWA_CLOAKED
  , dWMWA_FREEZE_REPRESENTATION = DWMWA_FREEZE_REPRESENTATION
  , dWMWA_LAST = DWMWA_LAST
}
