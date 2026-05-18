(* ::Package:: *)

BeginPackage["SolarCoreV1`"]

GetSystemData::usage = "Restituisce i dati esatti, info e domande per i quiz."
GetStarField::usage = "Restituisce i punti e i colori per lo sfondo stellato."
CalculatePosition::usage = "Calcola le coordinate in base a distanza e velocita."
OrbitPoints::usage = "Genera i punti per disegnare le orbite."

Begin["`Private`"]

safeTexture[name_] := Quiet[Check[PlanetData[name, "Image"], None]]

(* --- GENERAZIONE STELLE OTTIMIZZATA --- *)
generateStarField[n_] := Module[{coords, colors},
  coords = Table[
    With[{r = RandomReal[{100, 200}], theta = RandomReal[{0, Pi}], phi = RandomReal[{0, 2 Pi}]},
     r {Sin[theta] Cos[phi], Sin[theta] Sin[phi], Cos[theta]}
    ], {n}];
  
  colors = Table[RandomChoice[{8, 1, 1} -> {White, LightBlue, LightYellow}], {n}];
    
  (* INVECE DI 2000 OGGETTI, CREIAMO 1 SOLO OGGETTO MULTIPLO. VELOCIT\[CapitalAGrave] ESTREMA. *)
  {PointSize[0.003], Point[coords, VertexColors -> colors]}
]

$starFieldData = generateStarField[2000];
GetStarField[] := $starFieldData;
(* ------------------------------------------- *)

GetSystemData[] := <|
  "Sole" -> <|"Color"->Glow[Yellow], "Size"->2.5, "Dist"->0, "Speed"->0, "Texture"->safeTexture["Sun"],
    "Info"->"La stella al centro del sistema solare",
    "Quiz"->"Sono una palla di fuoco gigante che vi d\[AGrave] luce e calore ogni giorno. Chi sono?"|>,
    
  "Mercurio" -> <|"Color"->LightGray, "Size"->0.4, "Dist"->4, "Speed"->1.62, "Texture"->safeTexture["Mercury"],
    "Info"->"Diametro: 4,879 km\nDistanza dal Sole: 57.9M km\nPeriodo: 88 giorni\nTemp: -180\[Degree]C a 430\[Degree]C\nLune: 0",
    "Quiz"->"Sono il pianeta pi\[UGrave] piccolo di tutti e corro velocissimo vicinissimo al Sole! Chi sono?"|>,
    
  "Venere" -> <|"Color"->Orange, "Size"->0.7, "Dist"->7, "Speed"->1.18, "Texture"->safeTexture["Venus"],
    "Info"->"Diametro: 12,104 km\nDistanza dal Sole: 108.2M km\nPeriodo: 225 giorni\nTemp: 462\[Degree]C\nLune: 0",
    "Quiz"->"Faccio caldissimo! Sono il pianeta pi\[UGrave] rovente del sistema solare e brillo tanto nel cielo. Chi sono?"|>,
    
  "Terra" -> <|"Color"->Blue, "Size"->0.8, "Dist"->10, "Speed"->1.0, "Texture"->safeTexture["Earth"],
    "Info"->"Diametro: 12,742 km\nDistanza dal Sole: 149.6M km\nPeriodo: 365 giorni\nTemp: -89\[Degree]C a 57\[Degree]C\nLune: 1",
    "Quiz"->"Sono l'unico pianeta con tanta acqua liquida, foreste e... te! Chi sono?"|>,
    
  "Marte" -> <|"Color"->Red, "Size"->0.6, "Dist"->15, "Speed"->0.81, "Texture"->safeTexture["Mars"],
    "Info"->"Diametro: 6,779 km\nDistanza dal Sole: 227.9M km\nPeriodo: 687 giorni\nTemp: -140\[Degree]C a 20\[Degree]C\nLune: 2",
    "Quiz"->"Mi chiamano il 'Pianeta Rosso' e i terrestri mi mandano tanti robot da esplorare. Chi sono?"|>,
    
  "Giove" -> <|"Color"->Brown, "Size"->1.5, "Dist"->25, "Speed"->0.44, "Texture"->safeTexture["Jupiter"],
    "Info"->"Diametro: 139,820 km\nDistanza dal Sole: 778.6M km\nPeriodo: 12 anni\nTemp: -108\[Degree]C\nLune: 95",
    "Quiz"->"Sono il gigante della famiglia! Sono cos\[IGrave] grande che ci starebbero dentro tutti gli altri pianeti. Chi sono?"|>,
    
  "Saturno" -> <|"Color"->LightOrange, "Size"->1.3, "Dist"->35, "Speed"->0.32, "Texture"->safeTexture["Saturn"],
    "Info"->"Diametro: 116,460 km\nDistanza dal Sole: 1.434B km\nPeriodo: 29 anni\nTemp: -139\[Degree]C\nLune: 146",
    "Quiz"->"Sono famoso in tutto lo spazio per i miei bellissimi anelli fatti di ghiaccio e roccia. Chi sono?"|>,
    
  "Urano" -> <|"Color"->LightBlue, "Size"->1.1, "Dist"->45, "Speed"->0.23, "Texture"->safeTexture["Uranus"],
    "Info"->"Diametro: 50,724 km\nDistanza dal Sole: 2.871B km\nPeriodo: 84 anni\nTemp: -197\[Degree]C\nLune: 27",
    "Quiz"->"Sono un gigante di ghiaccio azzurro e... rotolo su un fianco invece di girare dritto! Chi sono?"|>,
    
  "Nettuno" -> <|"Color"->Darker[Cyan, 0.5], "Size"->1.1, "Dist"->55, "Speed"->0.18, "Texture"->safeTexture["Neptune"],
    "Info"->"Diametro: 49,244 km\nDistanza dal Sole: 4.495B km\nPeriodo: 165 anni\nTemp: -201\[Degree]C\nLune: 16",
    "Quiz"->"Sono il pianeta pi\[UGrave] lontano dal Sole, sono blu scuro e ho dei venti fortissimi! Chi sono?"|>
|>;

CalculatePosition[d_, t_, v_] := If[d == 0, {0, 0, 0}, {d * Cos[v * t], d * Sin[v * t], 0}]
OrbitPoints[d_] := Table[{d * Cos[a], d * Sin[a], 0}, {a, 0, 2 Pi, 2 Pi / 100}]

End[]
EndPackage[]
