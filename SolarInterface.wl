(* ::Package:: *)

BeginPackage["SolarInterface`", {"SolarCore`"}]

D\[EAcute]marrerApp::usage = "D\[EAcute]marrerApp[] lancia l'interfaccia avanzata del sistema solare."

Begin["`Private`"]

D\[EAcute]marrerApp[] := DynamicModule[{dati = GetSystemData[], listaPianeti},
  listaPianeti = Keys[dati];
  
  Manipulate[
   Graphics3D[{
   (*Sole*)
     {Glow[Yellow], 
     If[usaTexture && dati["Sole", "Texture"] =!= None, Texture[dati["Sole", "Texture"]], dati["Sole", "Color"]], 
     Sphere[{0, 0, 0}, dati["Sole", "Size"]]},
     
     (*Pianeti*)
     Table[
     With[{pData = dati[p]},
      {
       If[mostraOrbite, {Opacity[0.2], White, Line[OrbitPoints[pData["Dist"]]]}, {}],
       
       (*Se l'utente vuole texture  e la texture esiste, la usiamo, altrimenti usiamo il colore *)
       If[usaTexture && pData["Texture"] =!= None, Texture[pData["Texture"]],  pData["Color"]],
       
       Sphere[CalculatePosition[pData["Dist"], t * velocita, pData["Speed"]], pData["Size"]]
      }], 
      {p, Rest[listaPianeti]}
     ]
    },
    PlotRange -> 35, Lighting -> "Neutral", Boxed -> False, 
    Background -> Black, 
    (*Zoom corretto: 1.1 - zoomVAlue inverte lo slider*)
    ViewAngle -> (1.1 - zoomFactor)* Degree * 40, 
    ImageSize -> {700, 500}
   ],
   
   {{t, 0, "Tempo"}, 0, 100, 
    ControlType -> Animator,
    AnimationRunning->True,
    AnimationRate-> 1,
    AppearanceElements->All},
    
    
   {{velocita, 1, "Velocit\[AGrave]"}, 0.1, 5},
   {{mostraOrbite, True, "Orbite"}, {True, False}},
   {{usaTexture, False, "Texture (Se scaricate)"}, {True, False}},
   
   (*Lo slider va da lontano a vicino*)
   {{zoomFactor, 0.35, "Zoom"}, 0.1, 0.9},
   
   
   ControlPlacement -> Left
   TrackedSymbols :> {t, velocita, mostraOrbite, usaTexture, zoomFactor}
  ]
]

End[]
EndPackage[]
