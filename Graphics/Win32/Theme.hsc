{-# LANGUAGE RecordWildCards #-}

module Graphics.Win32.Theme where

import Graphics.Win32
import Foreign 
import Foreign.C.String


#include <Windows.h>
#include <uxtheme.h>

type HTHEME = HANDLE 

foreign import ccall "OpenThemeData" c_openThemeData :: HWND -> LPCWSTR -> IO HTHEME

openThemeData :: Maybe HWND -> String -> IO HTHEME
openThemeData hwnd classList = withCWString classList $ c_openThemeData (maybePtr hwnd)

foreign import ccall "CloseThemeData" closeThemeData :: HTHEME -> IO ()

foreign import ccall "GetThemeSysFont"
  getThemeSysFont :: HTHEME -> ThemeSysFont -> Ptr LOGFONTW -> IO HRESULT

newtype ThemeSysFont = ThemeSysFont INT

tMT_CAPTIONFONT :: ThemeSysFont
tMT_CAPTIONFONT = ThemeSysFont 801 -- mingw does not have this const yet

foreign import ccall "DrawThemeTextEx"
  c_DrawThemeTextEx :: HTHEME -> HDC -> INT -> INT -> LPCWSTR -> INT -> DWORD -> Ptr RECT -> Ptr DTTOPTS -> IO ()

drawThemeTextEx :: HTHEME -> HDC -> INT -> INT -> String -> DWORD -> RECT -> DTTOPTS -> IO ()
drawThemeTextEx htheme hdc iPartId iStateId text dwTextFlags rect options = 
  withCWStringLen text $ \(pszText, cchText) -> 
   with options $ \pOptions ->
    allocaRECT $ \pRect -> do
     pokeRECT pRect rect
     c_DrawThemeTextEx htheme hdc iPartId iStateId pszText (fromIntegral cchText) dwTextFlags pRect pOptions 

data DTTOPTS = DTTOPTS
  { dttDwFlags :: DWORD
  , dttCrText ::               COLORREF          
  , dttCrBorder ::             COLORREF          
  , dttCrShadow ::             COLORREF          
  , dttITextShadowType ::      INT               
  , dttPtShadowOffset ::       POINT             
  , dttIBorderSize ::          INT               
  , dttIFontPropId ::          INT               
  , dttIColorPropId ::         INT               
  , dttIStateId ::             INT               
  , dttFApplyOverlay ::        BOOL              
  , dttIGlowSize ::            INT               
  , dttPfnDrawTextCallback ::  DTT_CALLBACK_PROC 
  , dttLParam ::               LPARAM            
} 
type DTT_CALLBACK_PROC = ()

defDTTOPTS :: DTTOPTS
defDTTOPTS = DTTOPTS 0 0 0 0 0 (0,0) 0 0 0 0 False 0 () 0
  
instance Storable DTTOPTS where
  sizeOf _ = #{size DTTOPTS}
  alignment _ = #{alignment DTTOPTS}
  peek p = do
     dttDwFlags <- #{peek DTTOPTS,dwFlags} p
     dttCrText <- #{peek DTTOPTS,crText} p
     dttCrBorder <- #{peek DTTOPTS,crBorder} p
     dttCrShadow <- #{peek DTTOPTS,crShadow} p
     dttITextShadowType <- #{peek DTTOPTS,iTextShadowType} p
     dttPtShadowOffset <- peekPOINT (#{ptr DTTOPTS,ptShadowOffset} p)
     dttIBorderSize <- #{peek DTTOPTS,iBorderSize} p
     dttIFontPropId <- #{peek DTTOPTS,iFontPropId} p
     dttIColorPropId <- #{peek DTTOPTS,iColorPropId} p
     dttIStateId <- #{peek DTTOPTS,iStateId} p
     dttFApplyOverlay <- #{peek DTTOPTS,fApplyOverlay} p
     dttIGlowSize <- #{peek DTTOPTS,iGlowSize} p
     let dttPfnDrawTextCallback = ()
     dttLParam <- #{peek DTTOPTS,lParam} p
     return DTTOPTS{..}
  poke p DTTOPTS{..} = do
   #{poke DTTOPTS,dwSize} p (#{size DTTOPTS} :: DWORD)
  
   #{poke DTTOPTS,dwFlags} p dttDwFlags
   #{poke DTTOPTS,crText} p dttCrText
   #{poke DTTOPTS,crBorder} p dttCrBorder
   #{poke DTTOPTS,crShadow} p dttCrShadow
   #{poke DTTOPTS,iTextShadowType} p dttITextShadowType
   pokePOINT (#{ptr DTTOPTS,ptShadowOffset} p) dttPtShadowOffset
   #{poke DTTOPTS,iBorderSize} p dttIBorderSize
   #{poke DTTOPTS,iFontPropId} p dttIFontPropId
   #{poke DTTOPTS,iColorPropId} p dttIColorPropId
   #{poke DTTOPTS,iStateId} p dttIStateId
   #{poke DTTOPTS,fApplyOverlay} p dttFApplyOverlay
   #{poke DTTOPTS,iGlowSize} p dttIGlowSize
   #{poke DTTOPTS,pfnDrawTextCallback} p (castPtrToFunPtr nullPtr)
   #{poke DTTOPTS,lParam} p dttLParam
   
    
#{enum DWORD,
, dT_LEFT = DT_LEFT
, dT_WORD_ELLIPSIS = DT_WORD_ELLIPSIS
}

#{enum DWORD,
, dTT_COMPOSITED = DTT_COMPOSITED
, dTT_GLOWSIZE = DTT_GLOWSIZE
, dTT_TEXTCOLOR = DTT_TEXTCOLOR
}
