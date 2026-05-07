(* ::Package:: *)

BeginPackage["SolarCoreV1`"]

GetSystemData::usage = "Restituisce i dati esatti con le dimensioni e velocit\[AGrave] scelte."

Begin["`Private`"]

(* UsaTexture \[EGrave] opzionale, teniamo il colore di base come fallback *)
safeTexture[name_] := Quiet[Check[PlanetData[name, "Image"], None]]

GetSystemData[] := <|
  "Sole" -> <|"Color"->Yellow, "Dist"->0, "Speed"->0, "Size"->2.5, "Texture"->safeTexture["Sun"],
    "Info"->"La stella al centro del sistema solare"|>,
    
  "Mercurio" -> <|"Color"->LightGray, "Dist"->4, "Speed"->1.62, "Size"->0.4, "Texture"->safeTexture["Mercury"],
    "Info"->"Diametro: 4,879 km\nDistanza dal Sole: 57.9M km\nPeriodo: 88 giorni\nTemp: -180\[Degree]C a 430\[Degree]C\nLune: 0"|>,
    
  "Venere" -> <|"Color"->Orange, "Dist"->7, "Speed"->1.18, "Size"->0.7, "Texture"->safeTexture["Venus"],
    "Info"->"Diametro: 12,104 km\nDistanza dal Sole: 108.2M km\nPeriodo: 225 giorni\nTemp: 462\[Degree]C\nLune: 0"|>,
    
  "Terra" -> <|"Color"->Blue, "Dist"->10, "Speed"->1, "Size"->0.8, "Texture"->safeTexture["Earth"],
    "Info"->"Diametro: 12,742 km\nDistanza dal Sole: 149.6M km\nPeriodo: 365 giorni\nTemp: -89\[Degree]C a 57\[Degree]C\nLune: 1"|>,
    
  "Marte" -> <|"Color"->Red, "Dist"->15, "Speed"->0.81, "Size"->0.6, "Texture"->safeTexture["Mars"],
    "Info"->"Diametro: 6,779 km\nDistanza dal Sole: 227.9M km\nPeriodo: 687 giorni\nTemp: -140\[Degree]C a 20\[Degree]C\nLune: 2"|>,
    
  "Giove" -> <|"Color"->Brown, "Dist"->25, "Speed"->0.44, "Size"->1.5, "Texture"->safeTexture["Jupiter"],
    "Info"->"Diametro: 139,820 km\nDistanza dal Sole: 778.6M km\nPeriodo: 12 anni\nTemp: -108\[Degree]C\nLune: 95"|>,
    
  "Saturno" -> <|"Color"->LightOrange, "Dist"->35, "Speed"->0.32, "Size"->1.3, "Texture"->safeTexture["Saturn"],
    "Info"->"Diametro: 116,460 km\nDistanza dal Sole: 1.434B km\nPeriodo: 29 anni\nTemp: -139\[Degree]C\nLune: 146"|>,
    
  "Urano" -> <|"Color"->LightBlue, "Dist"->45, "Speed"->0.23, "Size"->1.1, "Texture"->safeTexture["Uranus"],
    "Info"->"Diametro: 50,724 km\nDistanza dal Sole: 2.871B km\nPeriodo: 84 anni\nTemp: -197\[Degree]C\nLune: 27"|>,
    
  "Nettuno" -> <|"Color"->Darker[Cyan, 0.5], "Dist"->55, "Speed"->0.18, "Size"->1.1, "Texture"->safeTexture["Neptune"],
    "Info"->"Diametro: 49,244 km\nDistanza dal Sole: 4.495B km\nPeriodo: 165 anni\nTemp: -201\[Degree]C\nLune: 16"|>
|>;

End[]
EndPackage[]
