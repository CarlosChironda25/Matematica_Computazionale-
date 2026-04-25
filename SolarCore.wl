(* ::Package:: *)

BeginPackage["SolarCore`"]

(* Funzioni Pubbliche *)
GetSystemData::usage = "GetSystemData[] restituisce i parametri di tutti i corpi celesti."
CalculatePosition::usage = "CalculatePosition[distanza, tempo, velocita] calcola la posizione {x, y, z}."
OrbitPoints::usage = "OrbitPoints[distanza] genera la lista di punti per il disegno dell'orbita."

Begin["`Private`"]
(* Funzione interna per scaricare texture in sicurezza*)
safeTexture[name_] := Quiet[Check[PlanetData[name, "Image"], None]]
(* Database dei pianeti: facile da estendere *)
GetSystemData[] := <|
	"Sole"-> <|"Color"->Glow[Yellow], "Size"-> .15, "Dist"-> 0, "Speed"-> 0, "Texture"-> safeTexture["Sun"]|>,
	"Mercurio"-> <|"Color"->Glow[Gray], "Size"-> 0.02, "Dist"-> 0.4, "Speed"-> 1.6,"Texture"-> safeTexture["Mercury"]|>,
	"Venere"-> <|"Color"->Glow[Orange], "Size"-> 0.04, "Dist"-> 0.7, "Speed"-> 1.1,"Texture"-> safeTexture["Venus"]|>,
	"Terra"-> <|"Color"->Glow[Blue], "Size"-> 0.045, "Dist"-> 1.0, "Speed"-> 1.0,"Texture"-> safeTexture["Earth"]|>,
	"Marte"-> <|"Color"->Glow[Red], "Size"-> 0.03, "Dist"-> 1.5, "Speed"-> 0.8,"Texture"-> safeTexture["Mars"]|>,
	"Jupiter"-> <|"Color"->Glow[Brown], "Size"-> 0.1, "Dist"-> 5.2, "Speed"-> 0.43,"Texture"-> safeTexture["Jupiter"]|>,
	"Saturno"-> <|"Color"->Glow[LightOrange], "Size"-> 0.085, "Dist"-> 9.5, "Speed"-> 0.32,"Texture"-> safeTexture["Saturn"]|>,
	"Uranus"-> <|"Color"->Glow[LightBlue], "Size"-> 0.07, "Dist"-> 19.2, "Speed"-> 0.23,"Texture"-> safeTexture["Uranus"]|>,
	"Neptuno"-> <|"Color"->Glow[Darker[Cyan, 0.5]], "Size"-> 0.065, "Dist"-> 30.1, "Speed"-> 0.18,"Texture"-> safeTexture["Neptune"]|>
|>

(* Logica del movimento: Kplero semplificato *)
CalculatePosition[d_, t_, v_] := If[d == 0, {0, 0, 0},
	d*{Cos[v*t / d^1.5], Sin[v * t/d^1.5], 0}
]

(* Generatore di cerchi 3D *)
OrbitPoints[d_] := Table[N@{d Cos[a], d Sin[a], 0}, {a, 0, 2 Pi, 2 Pi/100}] 

End[]
EndPackage[]
